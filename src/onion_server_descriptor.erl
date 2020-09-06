%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Server Descriptor API.
%%%
%%% This module contains functions for working with server descriptors. For
%%% information about the server descriptor format see section 2.1.1 of Tor's
%%% dir-spec.txt
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_server_descriptor).

%% API.
-export([encode/1,
         decode/1]).

-include("onion_test.hrl").

%% @doc Encode a given ServerDescriptor into an iolist().
%%
%% This function encodes a given ServerDescriptor into an iolist().
%%
%% @end
-spec encode(ServerDescriptor) -> iolist()
    when
        ServerDescriptor :: map().
encode(ServerDescriptor) ->
    %% Router information.
    Nickname = maps:get(nickname, ServerDescriptor),
    Address  = maps:get(address, ServerDescriptor),
    ORPort   = maps:get(or_port, ServerDescriptor),

    %% Bandwidth information.
    BWAverage  = maps:get(bandwidth_average, ServerDescriptor),
    BWBurst    = maps:get(bandwidth_burst, ServerDescriptor),
    BWObserved = maps:get(bandwidth_observed, ServerDescriptor),

    %% Platform information.
    Platform = maps:get(platform, ServerDescriptor),

    %% Contact information.
    Contact = maps:get(contact, ServerDescriptor),

    %% Family.
    Family = maps:get(family, ServerDescriptor, []),

    %% Published information.
    Published = maps:get(published, ServerDescriptor),

    %% Uptime information.
    Uptime = maps:get(uptime, ServerDescriptor),

    %% Signing Key information.
    SigningKey = maps:get(signing_key, ServerDescriptor),

    %% Onion Key information.
    OnionKey = maps:get(onion_key, ServerDescriptor),

    %% NTor Onion Key information.
    NTorOnionKey = maps:get(ntor_onion_key, ServerDescriptor),

    %% Exit policy information (with a sensible default, just in case).
    ExitPolicy = maps:get(exit_policy, ServerDescriptor, [{reject, "*:*"}]),

    %% Encode the document.
    onion_document:encode([
        %% "router" nickname address ORPort SOCKSPort DirPort
        {router, [Nickname, inet:ntoa(Address), ORPort, 0, 0]},

        %% "bandwidth" bandwidth-avg bandwidth-burst bandwidth-observed
        {bandwidth, [BWAverage, BWBurst, BWObserved]},

        %% "platform" string
        {platform, [Platform]},

        %% "contact" info
        {contact, [Contact]},

        %% "family" names (ignore if empty)
        case Family of
            [] ->
                [];

            Family ->
                {family, Family}
        end,

        %% "published" YYYY-MM-DD HH:MM:SS
        {published, [{datetime, Published}]},

        %% "fingerprint" fingerprint
        {fingerprint, [onion_binary:fingerprint(sha, SigningKey)]},

        %% "uptime" number
        {uptime, [Uptime]},

        %% "onion-key" NL a public key in PEM format
        {'onion-key', [], [{"RSA PUBLIC KEY", OnionKey}]},

        %% "ntor-onion-key" base-64-encoded-key
        {'ntor-onion-key', [onion_base64:encode(NTorOnionKey)]},

        %% "signing-key" NL a public key in PEM format
        {'signing-key', [], [{"RSA PUBLIC KEY", SigningKey}]},

        %% "accept" exitpattern
        %% "reject" exitpattern
        lists:map(fun ({Rule, Pattern}) ->
                      {Rule, [Pattern]}
                  end, ExitPolicy),

        %% "router-signature" NL Signature (the Signature is added outside of this function).
        {'router-signature', []}
    ]).


%% @doc Parse and verify a server descriptor document.
-spec decode(Document) -> {ok, ParsedItems}
    when
      Document    :: binary(),
      ParsedItems :: [term()].
decode(Document) ->
    ServerDescriptorSpec = [
                            {router,                     exactly_once},
                            {'identity-ed25519',         at_most_once},
                            {'master-key-ed25519',       at_most_once},
                            {bandwidth,                  exactly_once},
                            {platform,                   at_most_once},
                            {published,                  exactly_once},
                            {fingerprint,                at_most_once},
                            {hibernating,                at_most_once},
                            {uptime,                     at_most_once},
                            {'onion-key',                exactly_once},
                            {'onion-key-crosscert',      at_most_once},
                            {'ntor-onion-key',           at_most_once},
                            {'ntor-onion-key-crosscert', at_most_once},
                            {'signing-key',              exactly_once},
                            {accept,                     any_number  },
                            {reject,                     any_number  },
                            {'ipv6-policy',              at_most_once},
                            {'router-sig-ed25519',       at_most_once},
                            {'router-signature',         exactly_once},
                            {contact,                    at_most_once},
                            {family,                     at_most_once},
                            {'read-history',             at_most_once},
                            {'write-history',            at_most_once},
                            {eventdns,                   at_most_once},
                            {'caches-extra-info',        at_most_once},
                            {'extra-info-digest',        at_most_once},
                            {'hidden-service-dir',       at_most_once},
                            {protocols,                  at_most_once},
                            {'allow-single-hop-exits',   at_most_once},
                            {'or-address',               any_number  },
                            {'tunnelled-dir-server',     at_most_once},
                            {proto,                      at_most_once}
                           ],

    %% Basic parsing, decoding & verification
    {ok, Items} = onion_document:decode(Document),
    {ParsedItems, DocumentLength, ItemOrder} = onion_document_utils:decode_items(Items, fun item_decoder/3),
    ok = onion_document_utils:verify_existence_properties(ServerDescriptorSpec, ItemOrder),

    %% Router entry should exist and be the first
    #{router := [1]} = ItemOrder,

    %% Verify that a valid exit policy is defined.
    ok = verify_exit_policy(ParsedItems, ItemOrder),

    %% identity-ed25519 should be second if it's present
    [2] = maps:get('identity-ed25519', ItemOrder, [2]),

    %% Verify existence & position of document signatures
    SecondLast = DocumentLength - 1,
    [SecondLast] = maps:get('router-sig-ed25519', ItemOrder, [SecondLast]),
    [DocumentLength] = maps:get('router-signature', ItemOrder),

    %% [At most once, required when identity-ed25519 is present]:
    %%   'ntor-onion-key-crosscert'
    %%   'router-sig-ed25519'
    %%   'onion-key-crosscert'
    %% The spec actually says actually says "forbidden otherwise" for
    %% onion-key-crosscert but we do not check that. Not sure it is necessary.
    %% If one of these is NOT present when identity-ed25519 is, the verification
    %% below will fail.
    case identity_ed25519_is_present(ItemOrder) of
        true ->
            ok = verify_identity_ed25519_cert(ParsedItems),
            ok = verify_onion_key_crosscert(ParsedItems),
            ok = verify_ntor_onion_key_crosscert(ParsedItems),
            ok = verify_ed25519_router_signature(Document, ParsedItems);
        false ->
            ok
    end,

    %% Certificate verification
    ok = verify_fingerprint(ParsedItems),
    ok = verify_rsa_signature(Document, ParsedItems),
    {ok, ParsedItems}.


%% @private
-spec verify_fingerprint([term()]) -> ok.
verify_fingerprint(Items) ->
    case proplists:get_value('fingerprint', Items, unavailable) of
        unavailable ->
            ok;

        Fingerprint ->
            SigningKey = proplists:get_value('signing-key', Items),
            Fingerprint = crypto:hash(sha, SigningKey),
            ok
    end.

%% @private
-spec verify_rsa_signature(Document, Items) -> ok
    when
        Document :: binary(),
        Items    :: [term()].
verify_rsa_signature(Document, Items) ->
    %% Extract public key & signature
    SigningKeyDER = proplists:get_value('signing-key', Items),
    {ok, SigningKey} = onion_rsa:der_decode_public_key(SigningKeyDER),
    Signature = proplists:get_value('router-signature', Items),

    %% Create message digest
    {Offset, Length} = binary:match(Document, <<"\nrouter-signature\n">>),
    Message = binary:part(Document, {0, Offset + Length}),
    MessageDigest = crypto:hash(sha, Message),

    %% Verify that signature is encrypted message digest
    MessageDigest = onion_rsa:public_decrypt(Signature, SigningKey, rsa_pkcs1_padding),
    ok.


%% @private
-spec verify_ed25519_router_signature(Document, Items) -> ok
    when
        Document :: binary(),
        Items    :: [term()].
verify_ed25519_router_signature(Document, Items) ->
    case proplists:get_value('identity-ed25519', Items) of
        {#{ cert_key := SignedKey}, _, _} ->
            %% Construct signed message
            {Offset, Length} = binary:match(Document, <<"\nrouter-sig-ed25519 ">>),
            MessagePart = binary:part(Document, {0, Offset + Length}),
            Message = <<"Tor router descriptor signature v1", MessagePart/binary>>,
            MessageDigest = crypto:hash(sha256, Message),

            %% Verify message signature
            Signature = proplists:get_value('router-sig-ed25519', Items),
            true = onion_ed25519:open(Signature, MessageDigest, SignedKey),
            ok;

        %% we check if this item and identity-ed25519 co-exist elsewhere
        undefined ->
            ok
    end.


%% @private
-spec identity_ed25519_is_present(DocumentStats) -> ok
    when
      DocumentStats :: #{}.
identity_ed25519_is_present(DocumentStats) ->
    case maps:get('identity-ed25519', DocumentStats, not_found) of
        not_found ->
            false;
        _ ->
            true
    end.


%% @private
-spec item_decoder(Keyword, Arguments, Object) -> [ParsedItem]
  when
    Keyword    :: atom() | binary(),
    Arguments  :: binary(),
    Object     :: binary(),
    ParsedItem :: term().
item_decoder(published, UTCTime, no_object) ->
    onion_document_utils:parse_datetime(UTCTime);

item_decoder(fingerprint, FingerprintRaw, no_object) ->
    %% Verify 4-char blocks
    true = lists:all(fun(Chunk) -> size(Chunk) =:= 4 end, onion_document_utils:sp_split(FingerprintRaw)),

    %% Verify valid hex encoding
    FingerprintHex = binary:replace(FingerprintRaw, <<" ">>, <<"">>, [global]),
    true = onion_base16:valid(FingerprintHex),

    %% 10 blocks of 4 hex digits is the least (sha1) hash digest size.
    true = size(FingerprintHex) >= 40,

    %% Decode hex fingerprint
    {ok, Fingerprint} = onion_base16:decode(FingerprintHex),
    Fingerprint;

item_decoder(hibernating, BoolRaw, no_object) ->
    onion_document_utils:decode_bool(BoolRaw);

item_decoder(uptime, Uptime, no_object) ->
    erlang:binary_to_integer(Uptime);

item_decoder(family, NamesBinary, no_object) ->
    Names = [binary_to_list(NameBin) || NameBin <- onion_document_utils:sp_split(NamesBinary)],
    lists:map(fun (Name) ->
                  true = onion_relay:valid_nickname_or_relay_fingerprint(Name)
              end, Names),
    Names;

item_decoder(eventdns, BoolRaw, no_object) ->
    %% not seen in any cached server-descriptors
    onion_document_utils:decode_bool(BoolRaw);

item_decoder(router, Arguments, no_object) ->
    %% Ignore any remaining arguments according to dir-spec section 2.1.1
    [NickName, Address, ORPortBin, SOCKSPortBin, DirPortBin | _RestArguments] = onion_document_utils:sp_split(Arguments),
    true = onion_relay:valid_nickname_or_relay_fingerprint(NickName),

    %% 0 = SOCKSPort According to spec
    [ORPort, 0 = _SOCKSPort, DirPort] = onion_document_utils:binaries2integers([ORPortBin, SOCKSPortBin, DirPortBin]),

    true = lists:all(fun(Port) -> onion_document_utils:verify_port(Port) end, [ORPort, DirPort]),
    true = ORPort /= 0,
    {ok, IPv4} = inet:parse_ipv4strict_address(erlang:binary_to_list(Address)),
    #{ nickname => NickName,
       address  => IPv4,
       or_port  => ORPort,
       dir_port => DirPort};

item_decoder(bandwidth, Arguments, no_object) ->
    ArgumentList = binary:split(Arguments, <<" ">>, [global]),
    [BandwidthAvg, BandwidthBurst, BandwidthObserved] = BWList = onion_document_utils:binaries2integers(ArgumentList),
    true = lists:all(fun(Int) -> Int >= 0 end, BWList),
    #{ bandwidth_average  => BandwidthAvg,
       bandwidth_burst    => BandwidthBurst,
       bandwidth_observed => BandwidthObserved };

item_decoder('ipv6-policy', PolicyRaw, no_object) ->
    %% FIXME (lga) A missing "ipv6-policy" line is equivalent to "ipv6-policy reject 1-65535"
    %% According to dir-spec.txt L2122-2130:
    %% FIXME (lga) verify that portnumbers are valid integers
    case PolicyRaw of
        <<"accept ", PortList/binary>> ->
            {accept, onion_document_utils:parse_portlist(PortList)};
        <<"reject ", PortList/binary>> ->
            {reject, onion_document_utils:parse_portlist(PortList)}
    end;

item_decoder(proto, EntriesRaw, no_object) ->
    Entries = onion_document_utils:sp_split(EntriesRaw),
    lists:map(fun parse_proto_entry/1, Entries);

item_decoder('or-address', Address, no_object) ->
    onion_document_utils:decode_address(Address);

item_decoder('extra-info-digest', Digests, no_object) ->
    case onion_document_utils:sp_split(Digests) of
        [Sha1Digest] ->
            {ok, Sha1} = onion_base16:decode(Sha1Digest),
            true = size(Sha1) =:= 20, % 160 bits
            [Sha1];

        [Sha1Digest, Sha256Digest] ->
            {ok, Sha1} = onion_base16:decode(Sha1Digest),
            {ok, Sha256} = onion_base64:decode(Sha256Digest),
            true = size(Sha1) =:= 20, % 160 bits
            true = size(Sha256) =:= 32, % 256 bits
            [Sha1, Sha256]
    end;

item_decoder('hidden-service-dir', VersionNumsBin, no_object) ->
    case onion_document_utils:binaries2integers(onion_document_utils:sp_split(VersionNumsBin)) of
        [] ->
            [2];
        MultipleVersions ->
            % FIXME (lga) do a check on version numbers to check they are valid
            MultipleVersions
     end;

%% tor-dir.txt section 2.1.2.1.:
%%
%% "Tools should continue to accept read-history and write-history values
%% in server descriptors produced by older versions of Tor
%% until all Tor versions earlier than 0.2.0.x are obsolete."
%%
%% Do we need to parse them at all?
item_decoder('write-history', History, no_object) ->
    History;
item_decoder('read-history', History, no_object) ->
    History;

%% FIXME (lga) reject / accept items needs some subtle parsing, i think.
%% Should probably write a lexer/parser for this. See section 2.1.3. on details
item_decoder(accept, ExitPattern, no_object) ->
    ExitPattern;

item_decoder(reject, ExitPattern, no_object) ->
    ExitPattern;

item_decoder(protocols, <<"protocols Link 1 2 Circuit 1">>, no_object) ->
    %% dir-spec L690-691:
    %% As of 30 Mar 2008, specified protocols are "Link 1 2 Circuit 1".
    #{ link => [1,2], circuit => 1 };

item_decoder('caches-extra-info', Arguments, Object) ->
    true = no_extra_arguments({Arguments, Object});

item_decoder('allow-single-hop-exits', Arguments, Object) ->
    true = no_extra_arguments({Arguments, Object});

item_decoder('tunnelled-dir-server', Arguments, Object) ->
    true = no_extra_arguments({Arguments, Object});

item_decoder(platform, Platform, no_object) ->
    Platform;

item_decoder(contact, ContactInfo, no_object) ->
    ContactInfo;

item_decoder('identity-ed25519', Arguments, {<<"ED25519 CERT">>, CertRaw}) ->
    true = no_extra_arguments(Arguments),
    {ok, Cert} = onion_base64:decode(CertRaw),
    onion_cert:decode(Cert);

item_decoder('master-key-ed25519', MasterKeyBase64, no_object) ->
    {ok, MasterKey} = onion_base64:decode(MasterKeyBase64),
    MasterKey;

item_decoder('onion-key', Arguments, {<<"RSA PUBLIC KEY">>, RSAKeyRaw}) ->
    true = no_extra_arguments(Arguments),
    decode_and_verify_onion_key(RSAKeyRaw);

item_decoder('onion-key-crosscert', Arguments, {<<"CROSSCERT">>, CertRaw}) ->
    true = no_extra_arguments(Arguments),
    {ok, Cert} = onion_base64:decode(CertRaw),
    Cert;

item_decoder('ntor-onion-key', PublicKeyBase64, no_object) ->
    {ok, PublicKey} = onion_base64:decode(PublicKeyBase64),
    PublicKey;

item_decoder('ntor-onion-key-crosscert', BitRaw, {<<"ED25519 CERT">>, CertRaw}) ->
    %% This item also has a [No extra arguments] clause, but these two checks ensure that
    {ok, CertEncoded} = onion_base64:decode(CertRaw),
    {Cert, Message, Signature} = onion_cert:decode(CertEncoded),
    Bit = erlang:binary_to_integer(BitRaw),
    {Bit, Cert, Message, Signature};

item_decoder('signing-key', Arguments, {<<"RSA PUBLIC KEY">>, PublicKeyBase64}) ->
    true = no_extra_arguments(Arguments),
    {ok, PublicKey} = onion_base64:decode(PublicKeyBase64),
    PublicKey;

item_decoder('router-sig-ed25519', SignatureBase64, no_object) ->
    %% This item does not have a [No extra arguments] in spec.. should it?
    {ok, Signature} = onion_base64:decode(SignatureBase64),
    Signature;

item_decoder('router-signature', Arguments, {<<"SIGNATURE">>, SignatureBase64}) ->
    true = no_extra_arguments(Arguments),
    {ok, Signature} = onion_base64:decode(SignatureBase64),
    Signature;


item_decoder(_Keyword, Arguments, Object) ->
    %% We ignore unknown items, according to dir-spec (section 1.2 L222-225)
    {Arguments, Object}.


%% @private
-spec no_extra_arguments(ExtraInput) -> boolean()
    when
      ExtraInput :: term().
no_extra_arguments(no_arguments) ->
    true;
no_extra_arguments({no_arguments, no_object}) ->
    true;
no_extra_arguments(_) ->
    false.


%% @private
-spec parse_proto_entry(Entry) -> {atom(), [binary()]}
    when
      Entry :: binary().
parse_proto_entry(Entry) ->
    %% This could be more precise. Fix when needed.
    [KeyWord, Values] = binary:split(Entry, <<"=">>),
    ValuesList = binary:split(Values, <<",">>),
    {erlang:binary_to_atom(KeyWord, utf8), ValuesList}.


%% @private
-spec verify_identity_ed25519_cert(Items) -> ok
    when
        Items :: [term()].
verify_identity_ed25519_cert(Items) ->
    %% In the spec the following prefix should be used in the message of the cert
    %%  <<"Tor node signing key certificate v1">>
    %% However, it seems that this is not used by tor.
    {Cert, Message, Signature} = proplists:get_value('identity-ed25519', Items),

    %% Verify certificate type and extract Ed25519Key
    #{ version := 1,
       cert_type := 4,
       cert_key_type := 1,
       extensions := [{ExtensionType, ExtensionFlags, Ed25519Key}] } = Cert,
    4 = ExtensionType,
    0 = ExtensionFlags,

    %% Verify that if the 'master-key-ed25519' item exists it should be equal to the key used here
    Ed25519Key = proplists:get_value('master-key-ed25519', Items, Ed25519Key),

    %% Verify certificate signature
    true = onion_ed25519:open(Signature, Message, Ed25519Key),
    ok.


%% @private
-spec verify_onion_key_crosscert(Items) -> ok
    when
        Items :: [term()].
verify_onion_key_crosscert(Items) ->
    %% Extract keys and signature
    Signature = proplists:get_value('onion-key-crosscert', Items),
    OnionPubkey = proplists:get_value('onion-key', Items),
    IdentityKey = get_ed25519_identity_key(Items),
    SigningKey = proplists:get_value('signing-key', Items),

    %% Verify signature
    SigningKeyDigest = crypto:hash(sha, SigningKey),
    DecryptedData = onion_rsa:public_decrypt(Signature, OnionPubkey, rsa_no_padding),
    Payload = <<SigningKeyDigest/binary, IdentityKey/binary>>,
    CommonSuffix = binary:longest_common_suffix([Payload, DecryptedData]),
    CommonSuffix = size(Payload),
    ok.

%% @private
verify_exit_policy(ParsedItems, ItemOrder) ->
    %% There should be at least one accept or reject keyword
    %% and the last of these MUST be '*:*', according to spec
    AcceptPositions = maps:get(accept, ItemOrder, [-1]),
    RejectPositions = maps:get(reject, ItemOrder, [-1]),
    AllPositions = AcceptPositions ++ RejectPositions,
    {_, <<"*:*">>} = lists:nth(lists:max(AllPositions), ParsedItems),
    ok.

%% @private
-spec verify_ntor_onion_key_crosscert(Items) -> ok
    when
        Items :: [term()].
verify_ntor_onion_key_crosscert(Items) ->
    %% Extract certificate & signing key
    PubkeyCurve25519 = proplists:get_value('ntor-onion-key', Items),
    {Bit, CrossCert, Message, Signature} = proplists:get_value('ntor-onion-key-crosscert', Items),
    Ed25519Key = onion_ed25519:public_key_from_x25519_public_key(PubkeyCurve25519, Bit),

    %% Verify certificate type and that key from identity-ed25519 is equal to cert_key
    #{ version       := 1,
       cert_type     := 10, % 1a in hex used in dir-spec
       cert_key      := IdentityKey,
       cert_key_type := 1,
       extensions    := [] } = CrossCert,

    %% Verify that the 'identity-ed25519' is the same as the signed key in crosscert
    IdentityKey = get_ed25519_identity_key(Items),

    %% Verify certificate
    true = onion_ed25519:open(Signature, Message, Ed25519Key),
    ok.


%% @private
-spec decode_and_verify_onion_key(RSAKeyBase64) -> RSAKey
    when
        RSAKeyBase64 :: binary(),
        RSAKey       :: onion_rsa:public_key().
decode_and_verify_onion_key(RSAKeyBase64) ->
    {ok, RSAKeyDER} = onion_base64:decode(RSAKeyBase64),
    {ok, {'RSAPublicKey', _, Exponent} = RSAKey} = onion_rsa:der_decode_public_key(RSAKeyDER),
    true = (65537 =:= Exponent),
    true = (1024 =:= onion_rsa:key_size(RSAKey)),
    RSAKey.

%% @private
-spec get_ed25519_identity_key(Items) -> IdentityKey | no_key
    when
        Items       :: [term()],
        IdentityKey :: binary().
get_ed25519_identity_key(Items) ->
    case proplists:get_value('identity-ed25519', Items) of
        {#{ extensions := [{_, _, IdentityKey}] }, _, _} ->
            IdentityKey;
        _ ->
            no_key
    end.
