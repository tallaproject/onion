%%%
%%% Copyright (c) 2017 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Lasse Grinderslev Andersen <lasse@etableret.dk>
%%% @doc Key certificate API.
%%%
%%% This module contains functions for working with key certificate documents.
%%% For information about the key certificate format see section 3.1. of Tor's
%%% dir-spec.txt
%%%
%%% @end
%%% -----------------------------------------------------------

-module(onion_key_certificate).

-export([decode/1,
         decode_all/1]).

%% @doc Decode a concatenated list of key certificate documents.
%%
%% Decodes a list of key certificate documents from, e.g.,
%% http://<hostname>/tor/keys/all.z
%%
%% @end
-spec decode_all(Documents) -> {ok, [ParsedItems]}
    when
      Documents   :: binary(),
      ParsedItems :: [term()].
decode_all(Documents) ->
    decode_all_(Documents, []).

%% @private
-spec decode_all_(Documents, ParsedItems) -> {ok, [ParsedItems]}
   when
      Documents   :: binary(),
      ParsedItems :: [term()].
decode_all_(Documents, ParsedDocuments) ->
    FirstItem = <<"dir-key-certificate-version 3">>,
    case binary:split(Documents, FirstItem) of
        %% First document in documents
        [<<"">>, RestDocuments] ->
            decode_all_(RestDocuments, ParsedDocuments);

        [Document, RestDocuments] ->
            {ok, ParsedItems} = decode(<<FirstItem/binary, Document/binary>>),
            decode_all_(RestDocuments, [ParsedItems | ParsedDocuments]);

        %% Last document in documents
        [Document] ->
            {ok, ParsedItems} = decode(<<FirstItem/binary, Document/binary>>),
            {ok, [ParsedItems | ParsedDocuments]}
    end.


%% @doc Parse and verify a key certificate document.
-spec decode(Document) -> {ok, ParsedItems}
    when
      Document    :: binary(),
      ParsedItems :: [term()].
decode(Document) ->
    %% NOTE this document is order invariant!
    ServerDescriptorSpec = [
                            {'dir-key-certificate-version', exactly_once},
                            {'dir-address',                 at_most_once},
                            {fingerprint,                   exactly_once},
                            {'dir-identity-key',            exactly_once},
                            {'dir-key-published',           exactly_once},
                            {'dir-key-expires',             exactly_once},
                            {'dir-signing-key',             exactly_once},
                            {'dir-key-crosscert',           exactly_once},
                            {'dir-key-certification',       at_most_once}
                           ],

    %% Basic decoding & verification
    {ok, Items} = onion_document:decode(Document),
    {ParsedItems, _DocumentLength, ItemOrder} = onion_document_utils:decode_items(Items, fun item_decoder/3),
    ok = onion_document_utils:verify_existence_properties(ServerDescriptorSpec, ItemOrder),
    ok = verify_crosscert(ParsedItems),
    ok = verify_document_signature(ParsedItems, Document),
    {ok, ParsedItems}.


%% @private
-spec item_decoder(Keyword, Arguments, Object) -> ParsedItem
  when
    Keyword    :: atom() | binary(),
    Arguments  :: binary(),
    Object     :: binary(),
    ParsedItem :: term().
item_decoder('dir-key-certificate-version', <<"3">>, no_object) ->
    3;

item_decoder('dir-address', IPPort, no_object) ->
    % FIXME need decoding
    IPPort;

item_decoder(fingerprint, FingerprintHex, no_object) ->
    {ok, Fingerprint} = onion_base16:decode(FingerprintHex),
    Fingerprint;

item_decoder('dir-key-published', DateTime, no_object) ->
    onion_document_utils:parse_datetime(DateTime);

item_decoder('dir-key-expires', DateTime, no_object) ->
    onion_document_utils:parse_datetime(DateTime);

item_decoder('dir-identity-key', no_arguments, {<<"RSA PUBLIC KEY">>, IdentityKeyPEM}) ->
    {ok, IdentityKeyDER} = onion_base64:decode(IdentityKeyPEM),

    %% This key SHOULD be at least 2048 bits long; it MUST NOT be shorter than 1024 bits.
    {ok, IdentityKey} = onion_rsa:der_decode_public_key(IdentityKeyDER),
    true = (1024 =< onion_rsa:key_size(IdentityKey)),

    IdentityKeyDER;

item_decoder('dir-signing-key', no_arguments, {<<"RSA PUBLIC KEY">>, PublicKeyPEM}) ->
    {ok, PublicKeyDER} = onion_base64:decode(PublicKeyPEM),

    %% This key MUST be at least 1024 bits, and MAY be longer.
    {ok, PublicKey} = onion_rsa:der_decode_public_key(PublicKeyDER),
    true = (1024 =< onion_rsa:key_size(PublicKey)),
    PublicKey;

item_decoder('dir-key-crosscert', no_arguments, {<<"SIGNATURE">>, CrossSignaturePEM}) ->
    %% According to spec both "SIGNATURE" and "ID SIGNATURE" is valid here.
    item_decoder('dir-key-crosscert', no_arguments, {<<"ID SIGNATURE">>, CrossSignaturePEM});

item_decoder('dir-key-crosscert', no_arguments, {<<"ID SIGNATURE">>, CrossSignaturePEM}) ->
    {ok, CrossSignature} = onion_base64:decode(CrossSignaturePEM),
    CrossSignature;

item_decoder('dir-key-certification', no_arguments, {<<"SIGNATURE">>, DocumentSignaturePEM}) ->
    {ok, DocumentSignature} = onion_base64:decode(DocumentSignaturePEM),
    DocumentSignature;

item_decoder(Keyword, Arguments, Object) ->
    io:format("WHILE DEBUGGING: Error unkown item: ~p ~p ~p", [Keyword, Arguments, Object]),
    throw({error, unkown_keyword}).

%% @private
-spec verify_crosscert([Item]) -> ok
    when
      Item :: term().
verify_crosscert(Items) ->
    %% Extract keys and signature
    CrossSignature = proplists:get_value('dir-key-crosscert', Items),
    IdentityKey = proplists:get_value('dir-identity-key', Items),
    SigningKey = proplists:get_value('dir-signing-key', Items),

    %% Verify signature
    IdentityDigest = crypto:hash(sha, IdentityKey),
    DecryptedData = onion_rsa:public_decrypt(CrossSignature, SigningKey, rsa_no_padding),
    CommonSuffix = binary:longest_common_suffix([IdentityDigest, DecryptedData]),
    CommonSuffix = size(IdentityDigest),
    ok.

%% @private
-spec verify_document_signature([Item], Document) -> ok
    when
      Item     :: term(),
      Document :: binary().
verify_document_signature(Items, Document) ->
    %% Extract public key & signature
    IdentityKeyDER = proplists:get_value('dir-identity-key', Items),
    {ok, SigningKey} = onion_rsa:der_decode_public_key(IdentityKeyDER),
    Signature = proplists:get_value('dir-key-certification', Items),

    %% Create message digest
    {Offset, Length} = binary:match(Document, <<"\ndir-key-certification\n">>),
    Message = binary:part(Document, {0, Offset + Length}),
    MessageDigest = crypto:hash(sha, Message),

    %% Verify that signature is encrypted message digest
    MessageDigest = onion_rsa:public_decrypt(Signature, SigningKey, rsa_pkcs1_padding),
    ok.
