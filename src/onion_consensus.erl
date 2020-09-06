%%%
%%% Copyright (c) 2017 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Lasse Grinderslev Andersen <lasse@etableret.dk>
%%% @doc Consensus API.
%%%
%%% This module contains functions for working with consensus documents. For
%%% information about the consensus format see section 3.4.1 of Tor's
%%% dir-spec.txt
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_consensus).

-export([decode/1,
         verify_consensus/3]).

-record(authority_group, {dir_source, contact, vote_digest}).

%% @doc Verify consensus signatures made by the authorities
-spec verify_consensus(Document, Items, KeyCertificates) -> ok
      when
          Document        :: binary(),
          Items           :: [term()],
          KeyCertificates :: [term()].
verify_consensus(Document, Items, KeyCertificates) ->
    {ok, Fingerprint2KeyCert} = mapify(KeyCertificates),
    SigningPart = part_to_sign(Document),
    lists:all(fun({'directory-signature', SignatureData}) ->
                      valid_signature(SigningPart, SignatureData, Fingerprint2KeyCert);
                 (_) ->
                      true
              end, Items),
    ok.


%% @private
-spec mapify([KeyCertificate]) -> {ok, #{}}
      when
          KeyCertificate :: term().
mapify(KeyCertificates) ->
    mapify(KeyCertificates, #{}).

mapify([KeyCertificate | Rest], DocumentMap) ->
    Fingerprint = proplists:get_value(fingerprint, KeyCertificate),
    mapify(Rest, DocumentMap# { Fingerprint => KeyCertificate });

mapify([], DocumentMap) ->
    {ok, DocumentMap}.


%% @private
-spec valid_signature(Message, SignatureData, Fingerprint2KeyCert) -> true
    when
        Message             :: binary(),
        SignatureData       :: {atom(), binary(), binary(), binary()},
        Fingerprint2KeyCert :: #{}.
valid_signature(Message, {Algorithm, Identity, SigKeyDigest, Signature}, Fingerprint2KeyCert) ->
    MessageDigest = crypto:hash(Algorithm, Message),

    %% Extract signing key from key certificate
    KeyCert = maps:get(Identity, Fingerprint2KeyCert),
    SigningKeyDER = proplists:get_value('dir-signing-key', KeyCert),
    {ok, SigningKey} = onion_rsa:der_decode_public_key(SigningKeyDER),

    %% Verify that signing key digest matches the one supplied in consensus directory-signature item
    SigKeyDigest = crypto:hash(sha, SigningKeyDER),

    %% Verify that signature is encrypted message digest
    MessageDigest = onion_rsa:public_decrypt(Signature, SigningKey, rsa_pkcs1_padding),
    true.

%% @private
-spec part_to_sign(binary()) -> binary().
part_to_sign(Consensus) ->
    {Offset, Length} = binary:match(Consensus, <<"\ndirectory-signature ">>),
    binary:part(Consensus, {0, Offset + Length}).


%% @doc Parse and verify a consensus document.
-spec decode(Document) -> {ok, ParsedItems}
    when
      Document :: binary(),
      ParsedItems :: [term()].
decode(Document) ->
    %% Preabmle section
    PreambleSection = [
                     {'network-status-version',       exactly_once},
                     {'vote-status',                  exactly_once},
                     {'consensus-method',             at_most_once},
                     {'valid-after',                  exactly_once},
                     {'fresh-until',                  exactly_once},
                     {'valid-until',                  exactly_once},
                     {'voting-delay',                 exactly_once},
                     {'client-versions',              at_most_once},
                     {'server-versions',              at_most_once},
                     {package,                        any_number},
                     {'known-flags',                  exactly_once},
                     {'recommended-client-protocols', at_most_once},
                     {'recommended-relay-protocols',  at_most_once},
                     {'required-client-protocols',    at_most_once},
                     {'required-relay-protocols',     at_most_once},
                     {params,                         at_most_once},
                     {'shared-rand-previous-value',   at_most_once},
                     {'shared-rand-current-value',    at_most_once}
                     ],


    %% Authority section
    %% within group the order is as specfied here. The groups are ordered by identity digest.
    %%
    %% FIXME Note the following remark from dir-spec.txt relation to AuthoritySectionGroup:
    %%
    %%  For each "legacy-dir-key" in the vote, there is an additional "dir-source"
    %%  line containing that legacy key's fingerprint, the authority's nickname
    %%  with "-legacy" appended, and all other fields as in the main "dir-source"
    %%  line for that authority.  These "dir-source" lines do not have
    %%  corresponding "contact" or "vote-digest" entries.
    AuthoritySectionGroup = [
                             {'dir-source',  exactly_once},
                             {contact,       exactly_once},
                             {'vote-digest', exactly_once}
                           ],


    %%% Router-status section
    %%% within group the order is as specfied here. The groups are ordered by identity digest.
    RouterStatusEntries = [
                           {r,  exactly_once},
                           {a,  any_number},
                           {s,  exactly_once},
                           {v,  at_most_once},
                           {pr, at_most_once},
                           {w,  at_most_once},
                           {p,  at_most_once}
                          ],

    %%% Footer section
    %%% I guess the order is irrelevant since they are not part of the message to sign
    FooterSection = [
                     {'directory-footer',    exactly_once}, % "exactly_once" not specified but seems to be implied implictly.
                     {'bandwidth-weights',   at_most_once},
                     {'directory-signature', at_least_once}
                    ],

    %% Parse document and split into appropriate sections
    {ok, Items}                             = onion_document:decode(Document),
    {ok, PreambleItems, ItemsRest1}         = onion_document_utils:split_items(Items, 'dir-source'),
    {ok, AuthoritySectionItems, ItemsRest2} = onion_document_utils:split_items(ItemsRest1, r),
    {ok, RouterStatusItems, FooterItems}    = onion_document_utils:split_items(ItemsRest2, 'directory-footer'),

    %% Process each section individually
    {ok, ParsedPreambleItems}      = process_preamble(PreambleItems, PreambleSection),
    {ok, ParsedAuthSectionItems}   = process_authority_section(AuthoritySectionItems, AuthoritySectionGroup),
    {ok, ParsedRouterSectionItems} = process_router_section(RouterStatusItems, RouterStatusEntries),
    {ok, ParsedFooterItems}        = process_footer_section(FooterItems, FooterSection),
    {ok, ParsedPreambleItems ++ ParsedAuthSectionItems ++ ParsedRouterSectionItems ++ ParsedFooterItems}.


%% @private
process_preamble(Items, ItemSpecs) ->
    {ParsedItems, _Length, ItemOrder} = onion_document_utils:decode_items(Items, fun preamble_decoder/3),
    ok = onion_document_utils:verify_existence_properties(ItemSpecs, ItemOrder),
    ok = onion_document_utils:verify_order(ItemSpecs, ItemOrder),
    {ok, ParsedItems}.

process_authority_section(Items, _ItemSpecs) ->
    {ParsedItems, _, _} = onion_document_utils:decode_items(Items, fun authority_decoder/3),
    Groups = create_authority_groups(ParsedItems),

    ok = verify_group_order(Groups, authorities),
    {ok, Groups}.

process_router_section(Items, ItemSpecs) ->
    {ok, Routers} = verify_router_groups(Items, ItemSpecs),
    ok = verify_group_order(Routers, routers),
    {ok, Routers}.


%% @private
create_authority_groups(Items) ->
    create_authority_groups(Items, []).

create_authority_groups([{'dir-source', DirSource}, {contact, Contact}, {'vote-digest', Digest} | Rest], Grouped) ->
    Group = #authority_group { dir_source  = DirSource,
                               contact     = Contact,
                               vote_digest = Digest
                             },
    create_authority_groups(Rest, [Group | Grouped]);

create_authority_groups([], Grouped) ->
    lists:reverse(Grouped).


%% @private
verify_router_groups(Items, ItemSpecs) ->
    verify_router_groups(Items, ItemSpecs, []).

verify_router_groups(Items, RouterSpec, ParsedGroups) ->
    [RouterFirstItem | Items2 ] = Items,
    case onion_document_utils:split_items(Items2, r) of
        {ok, RouterRest, Rest} ->
            Router = [RouterFirstItem | RouterRest],
            {ParsedRouter, _Length, ItemOrder} = onion_document_utils:decode_items(Router, fun router_decoder/3),
            ok = onion_document_utils:verify_existence_properties(RouterSpec, ItemOrder),
            ok = onion_document_utils:verify_order(RouterSpec, ItemOrder),
            verify_router_groups(Rest, RouterSpec, [ParsedRouter | ParsedGroups]);
        keyword_not_found ->
            {ok, lists:reverse(ParsedGroups)}
    end.


%% @private
verify_group_order(Routers, routers) ->
    Digests = lists:map(fun(Router) ->
                                RouterInfo = proplists:get_value(r, Router),
                                {_, DigestB64, _, _, _, _, _} = RouterInfo,
                                {ok, Digest} = onion_base64:decode(DigestB64),
                                Digest
                        end, Routers),
    Digests = lists:sort(Digests),
    ok;

verify_group_order(Authorities, authorities) ->
    Digests = [IdDigest || #authority_group { dir_source = {_, IdDigest, _, _, _, _} } <- Authorities],
    Digests = lists:sort(Digests),
    ok.


%% @private
process_footer_section(Items, ItemSpecs) ->
    {ParsedItems, _Length, ItemOrder} = onion_document_utils:decode_items(Items, fun footer_decoder/3),
    ok = onion_document_utils:verify_order(ItemSpecs, ItemOrder),
    {ok, ParsedItems}.


%% @private
-spec preamble_decoder(Keyword, Arguments, Object) -> [ParsedItem]
  when
    Keyword    :: binary(),
    Arguments  :: binary(),
    Object     :: binary(),
    ParsedItem :: term().
preamble_decoder('network-status-version', <<"3">>, no_object) ->
    3;

preamble_decoder('vote-status', <<"consensus">>, no_object) ->
    consensus;

preamble_decoder('consensus-method', Version, no_object) ->
    erlang:binary_to_integer(Version);

preamble_decoder('valid-after', DateTime, no_object) ->
    onion_document_utils:parse_datetime(DateTime);

preamble_decoder('fresh-until', DateTime, no_object) ->
    onion_document_utils:parse_datetime(DateTime);

preamble_decoder('valid-until', DateTime, no_object) ->
    onion_document_utils:parse_datetime(DateTime);

preamble_decoder('voting-delay', Arguments, no_object) ->
    [VoteSeconds, DistSeconds] = onion_document_utils:binaries2integers(onion_document_utils:sp_split(Arguments)),
    {VoteSeconds, DistSeconds};

preamble_decoder('client-versions', VersionList, no_object) ->
    % FIXME (lga) No validation on client/server-versions.
    binary:split(VersionList, <<",">>, [global]);

preamble_decoder('server-versions', VersionList, no_object) ->
    binary:split(VersionList, <<",">>, [global]);

preamble_decoder(package, PackageInfoRaw, no_object) ->
    % FIXME (lga) simple parsing atm. This item-type is not seen in production consensuses atm.
    [Name, Version, URL | Digests] = onion_document_utils:sp_split(PackageInfoRaw),
    {Name, Version, URL, Digests};

preamble_decoder('known-flags', Flags, no_object) ->
    onion_document_utils:sp_split(Flags);

preamble_decoder('recommended-client-protocols', Protocols, no_object) ->
    onion_document_utils:sp_split(Protocols);

preamble_decoder('recommended-relay-protocols', Protocols, no_object) ->
    onion_document_utils:sp_split(Protocols);

preamble_decoder('required-client-protocols', Protocols, no_object) ->
    onion_document_utils:sp_split(Protocols);

preamble_decoder('required-relay-protocols', Protocols, no_object) ->
    onion_document_utils:sp_split(Protocols);

preamble_decoder(params, Params, no_object) ->
    lists:map(fun (Param) ->
                 [Key, Value] = binary:split(Param, <<"=">>, [global]),
                 {Key, Value}
              end, onion_document_utils:sp_split(Params));

preamble_decoder('shared-rand-previous-value', Arguments, no_object) ->
    [NumReveals, Value] = binary:split(Arguments, <<" ">>, [global]),
    {NumReveals, Value};

preamble_decoder('shared-rand-current-value', Arguments, no_object) ->
    [NumReveals, Value] = binary:split(Arguments, <<" ">>, [global]),
    {NumReveals, Value};

preamble_decoder(_Keyword, _Arguments, _Object) ->
    {error, unkown_keyword}.


%% @private
authority_decoder('dir-source', DirSource, no_object) ->
    %% FIXME (lga) Port and IP needs some validation
    [Nickname, Identity, Address, IP, DirPort, ORPort | _Rest] = onion_document_utils:sp_split(DirSource),
    {Nickname, Identity, Address, IP, DirPort, ORPort};

authority_decoder(contact, Contact, no_object) ->
    Contact;

authority_decoder('vote-digest', Digest, no_object) ->
    Digest;

authority_decoder(_Keyword, _Arguments, _Object) ->
    throw({error, unkown_keyword}).


%% @private
router_decoder(r, Router, no_object) ->
    %% FIXME (lga) figure out if we allow additional arguments or not
    % nickname SP identity SP digest SP publication SP IP SP ORPort SP DirPort
    [Nickname, Identity, Digest, Publication, IP, ORPort, DirPort | _Rest] = onion_document_utils:sp_split(Router),

    %% FIXME (lga) Port and IP needs some validation
    {Nickname, Identity, Digest, Publication, IP, ORPort, DirPort};

router_decoder(a, AddressRaw, no_object) ->
    %% This is [ipv6addr]:portnumber
    AddressRaw;

router_decoder(s, Flags, no_object) ->
    onion_document_utils:sp_split(Flags);

router_decoder(v, Version, no_object) ->
    Version;

router_decoder(pr, Entries, no_object) ->
    Entries;

router_decoder(w, Bandwidth, no_object) ->
    onion_document_utils:sp_split(Bandwidth);

router_decoder(p, Policy, no_object) ->
    [PolicyType, PortLIst] = onion_document_utils:sp_split(Policy),
    {PolicyType, onion_document_utils:parse_portlist(PortLIst)};

router_decoder(_Keyword, _, _) ->
    throw({error, unkown_keyword}).


%% @private
footer_decoder('directory-footer', no_arguments, no_object) ->
    true;

footer_decoder('bandwidth-weights', BWWeights, no_object) ->
    %% Example:
    %% bandwidth-weights Wbd=0 Wbe=0 Wbg=4130 Wbm=10000 Wdb=10000 Web=10000 Wed=10000 Wee=10000 Weg=10000 Wem=10000
    %% Wgb=10000 Wgd=0 Wgg=5870 Wgm=5870 Wmb=10000 Wmd=0 Wme=0 Wmg=4130 Wmm=10000
    onion_document_utils:sp_split(BWWeights);

footer_decoder('directory-signature', Arguments, {<<"SIGNATURE">>, SignatureB64}) ->
    %% Here the exact number of arguments is required.
    ArgsList = onion_document_utils:sp_split(Arguments),
    [Algorithm, IdentityB16, SigKeyDigestB16] = case length(ArgsList) of
        3 ->
            ArgsList;

        2 ->
            [sha | ArgsList]
    end,
    {ok, SigKeyDigest} = onion_base16:decode(SigKeyDigestB16),
    {ok, Identity} = onion_base16:decode(IdentityB16),
    {ok, Signature} = onion_base64:decode(SignatureB64),

    {Algorithm, Identity, SigKeyDigest, Signature};

footer_decoder(_Keyword, _, _) ->
    throw({error, unkown_keyword}).
