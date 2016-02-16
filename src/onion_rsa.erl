%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc RSA Cryptosystem API
%%%
%%% This helper module contains utilities for working with the RSA
%%% cryptosystem. It's made to simplify the usage of Erlang's `public_key`
%%% and `crypto` applications for our specific use case.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_rsa).

%% API.
-export([keypair/1,
         keypair/2,

         secret_key_to_public_key/1,

         der_encode/1,
         der_decode_secret_key/1,
         der_decode_public_key/1,

         pem_encode/1,
         pem_decode/1,

         private_encrypt/2,
         private_encrypt/3,
         private_decrypt/2,
         private_decrypt/3,

         public_encrypt/2,
         public_encrypt/3,
         public_decrypt/2,
         public_decrypt/3,

         sign/3,
         verify/4,

         key_size/1
        ]).

%% Types.
-export_type([secret_key/0,
              public_key/0,
              key/0,
              keypair/0]).

-type secret_key() :: public_key:rsa_private_key().
-type public_key() :: public_key:rsa_public_key().
-type key()        :: secret_key() | public_key().
-type keypair()    :: #{ secret => secret_key(),
                         public => public_key() }.

-include_lib("public_key/include/public_key.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-endif.

-spec keypair(Bits) -> {ok, KeyPair} | {error, Reason}
    when
        Bits           :: pos_integer(),
        KeyPair        :: keypair(),
        Reason         :: term().
keypair(Bits) ->
    keypair(Bits, 65537).

-spec keypair(Bits, PublicExponent) -> {ok, KeyPair} | {error, Reason}
    when
        Bits           :: pos_integer(),
        PublicExponent :: pos_integer(),
        KeyPair        :: keypair(),
        Reason         :: term().
keypair(Bits, PublicExponent) ->
    case onion_nif:rsa_generate_private_key(Bits, PublicExponent) of
        SecretKeyPEM when is_binary(SecretKeyPEM) ->
            {ok, SecretKey} = pem_decode(SecretKeyPEM),
            {ok, #{ secret => SecretKey, public => secret_key_to_public_key(SecretKey) }};

        {error, _} = Error ->
            Error
    end.

-spec secret_key_to_public_key(SecretKey) -> PublicKey
    when
        SecretKey :: secret_key(),
        PublicKey :: public_key().
secret_key_to_public_key(#'RSAPrivateKey'{ modulus = N, publicExponent = E }) ->
    #'RSAPublicKey'{
            modulus        = N,
            publicExponent = E
        }.

-spec der_encode(Key) -> {ok, Bytes} | {error, Reason}
    when
        Key    :: key(),
        Bytes  :: binary(),
        Reason :: term().
der_encode(#'RSAPrivateKey' {} = SecretKey) ->
    try
        {ok, public_key:der_encode('RSAPrivateKey', SecretKey)}
    catch _:_ ->
        {error, invalid_secret_key}
    end;

der_encode(#'RSAPublicKey' {} = PublicKey) ->
    try
        {ok, public_key:der_encode('RSAPublicKey', PublicKey)}
    catch _:_ ->
        {error, invalid_public_key}
    end.

-spec der_decode_secret_key(Bytes) -> {ok, SecretKey} | {error, Reason}
    when
        Bytes     :: binary(),
        SecretKey :: secret_key(),
        Reason    :: term().
der_decode_secret_key(Bytes) when is_binary(Bytes) ->
    try
        {ok, public_key:der_decode('RSAPrivateKey', Bytes)}
    catch _:_ ->
        {error, invalid_secret_key}
    end.

-spec der_decode_public_key(Bytes) -> {ok, PublicKey} | {error, Reason}
    when
        Bytes     :: binary(),
        PublicKey :: public_key(),
        Reason    :: term().
der_decode_public_key(Bytes) when is_binary(Bytes) ->
    try
        {ok, public_key:der_decode('RSAPublicKey', Bytes)}
    catch _:_ ->
        {error, invalid_public_key}
    end.

-spec pem_encode(Key) -> {ok, Bytes} | {error, Reason}
    when
        Key    :: key(),
        Bytes  :: binary(),
        Reason :: term().
pem_encode(#'RSAPrivateKey'{} = SecretKey) ->
    PEM = strip_trailing_newline(public_key:pem_encode([public_key:pem_entry_encode('RSAPrivateKey', SecretKey)])),
    {ok, PEM};

pem_encode(#'RSAPublicKey'{} = PublicKey) ->
    PEM = strip_trailing_newline(public_key:pem_encode([public_key:pem_entry_encode('RSAPublicKey', PublicKey)])),
    {ok, PEM};

pem_encode(_) ->
    {error, invalid_rsa_key}.

-spec pem_decode(Bytes) -> {ok, Key} | {error, Reason}
    when
        Bytes  :: binary(),
        Key    :: key(),
        Reason :: term().
pem_decode(Bytes) when is_binary(Bytes) ->
    case public_key:pem_decode(Bytes) of
        [{'RSAPrivateKey', _, not_encrypted} = SecretKey] ->
            {ok, public_key:pem_entry_decode(SecretKey)};

        [{'RSAPublicKey', _, not_encrypted} = PublicKey] ->
            {ok, public_key:pem_entry_decode(PublicKey)};

        _ ->
            {error, invalid_rsa_key}
    end.

-spec private_encrypt(PlainText, SecretKey) -> CipherText
    when
        PlainText  :: binary(),
        SecretKey  :: secret_key(),
        CipherText :: binary().
private_encrypt(PlainText, SecretKey) when is_binary(PlainText) ->
    private_encrypt(PlainText, SecretKey, rsa_pkcs1_padding).

-spec private_encrypt(PlainText, SecretKey, Padding) -> CipherText
    when
        PlainText  :: binary(),
        SecretKey  :: secret_key(),
        Padding    :: rsa_pkcs1_padding | rsa_no_padding,
        CipherText :: binary().
private_encrypt(PlainText, SecretKey, Padding) when is_binary(PlainText), is_atom(Padding) ->
    crypto:private_encrypt(rsa, PlainText, secret_key_to_list(SecretKey), Padding).

-spec private_decrypt(CipherText, SecretKey) -> PlainText
    when
        CipherText :: binary(),
        SecretKey  :: secret_key(),
        PlainText  :: binary().
private_decrypt(CipherText, SecretKey) when is_binary(CipherText) ->
    private_decrypt(CipherText, SecretKey, rsa_pkcs1_padding).

-spec private_decrypt(CipherText, SecretKey, Padding) -> PlainText
    when
        CipherText :: binary(),
        SecretKey  :: secret_key(),
        Padding    :: rsa_pkcs1_padding | rsa_pkcs1_oaep_padding | rsa_no_padding,
        PlainText  :: binary().
private_decrypt(CipherText, SecretKey, Padding) when is_binary(CipherText), is_atom(Padding) ->
    crypto:private_decrypt(rsa, CipherText, secret_key_to_list(SecretKey), Padding).

-spec public_encrypt(PlainText, PublicKey) -> CipherText
    when
        PlainText  :: binary(),
        PublicKey  :: public_key(),
        CipherText :: binary().
public_encrypt(PlainText, PublicKey) when is_binary(PlainText) ->
    public_encrypt(PlainText, PublicKey, rsa_pkcs1_padding).

-spec public_encrypt(PlainText, PublicKey, Padding) -> CipherText
    when
        PlainText  :: binary(),
        PublicKey  :: public_key(),
        Padding    :: rsa_pkcs1_padding | rsa_pkcs1_oaep_padding | rsa_no_padding,
        CipherText :: binary().
public_encrypt(PlainText, PublicKey, Padding) ->
    crypto:public_encrypt(rsa, PlainText, public_key_to_list(PublicKey), Padding).

-spec public_decrypt(CipherText, PublicKey) -> PlainText
    when
        CipherText :: binary(),
        PublicKey  :: public_key(),
        PlainText  :: binary().
public_decrypt(CipherText, PublicKey) when is_binary(CipherText) ->
    public_decrypt(CipherText, PublicKey, rsa_pkcs1_padding).

-spec public_decrypt(CipherText, PublicKey, Padding) -> PlainText
    when
        CipherText :: binary(),
        PublicKey  :: public_key(),
        Padding    :: rsa_pkcs1_padding | rsa_no_padding,
        PlainText  :: binary().
public_decrypt(CipherText, PublicKey, Padding) when is_binary(CipherText), is_atom(Padding) ->
    crypto:public_decrypt(rsa, CipherText, public_key_to_list(PublicKey), Padding).

-spec sign(DigestType, Message, SecretKey) -> Result
    when
        DigestType :: crypto:digest_type(),
        Message    :: binary() | {digest, binary()},
        SecretKey  :: secret_key(),
        Result     :: binary().
sign(DigestType, Message, SecretKey) ->
    crypto:sign(rsa, DigestType, Message, secret_key_to_list(SecretKey)).

-spec verify(DigestType, Message, Signature, PublicKey) -> boolean()
    when
        DigestType :: crypto:digest_type(),
        Message    :: binary() | {digest, binary()},
        Signature  :: binary(),
        PublicKey  :: public_key().
verify(DigestType, Message, Signature, PublicKey) ->
    crypto:verify(rsa, DigestType, Message, Signature, public_key_to_list(PublicKey)).

-spec key_size(Key) -> BitSize
    when
        Key     :: key(),
        BitSize :: non_neg_integer().
key_size(#'RSAPrivateKey'{} = SecretKey) ->
    key_size(secret_key_to_public_key(SecretKey));

key_size(#'RSAPublicKey'{ modulus = N }) ->
    %% FIXME(ahf): bit_size(<<0>>) =:= 8 - this is wrong.
    bit_size(binary:encode_unsigned(N)).

%% @private
-spec strip_trailing_newline(Data) -> Result
    when
        Data   :: binary(),
        Result :: binary().
strip_trailing_newline(Data) ->
    ResultSize = byte_size(Data) - 1,
    <<Result:ResultSize/binary, "\n">> = Data,
    Result.

%% @private
-spec secret_key_to_list(SecretKey) -> [term()]
    when
        SecretKey :: secret_key().
secret_key_to_list(SecretKey) ->
    #'RSAPrivateKey'{
            publicExponent  = E,
            modulus         = N,
            privateExponent = D,
            prime1          = P1,
            prime2          = P2,
            exponent1       = E1,
            exponent2       = E2,
            coefficient     = C
        } = SecretKey,
    [E, N, D, P1, P2, E1, E2, C].

%% @private
-spec public_key_to_list(PublicKey) -> [term()]
    when
        PublicKey :: public_key().
public_key_to_list(PublicKey) ->
    #'RSAPublicKey'{
            publicExponent = E,
            modulus        = N
        } = PublicKey,
    [E, N].

-ifdef(TEST).
key_size() ->
    oneof([1024, 2048, 4096]).

test_keypair(Size) ->
    {ok, #{ secret := S, public := P }} = keypair(Size),
    {S, P}.

prop_bit_size() ->
    ?FORALL(RSAKeySize, key_size(),
        ?LET({S, _}, test_keypair(RSAKeySize),
            begin
                #'RSAPrivateKey'{ modulus = N } = S,
                bit_size(binary:encode_unsigned(N)) =:= RSAKeySize
            end)).

prop_der_iso() ->
    ?FORALL(RSAKeySize, key_size(),
        ?LET({S, P}, test_keypair(RSAKeySize),
            begin
                {ok, SecretDer} = der_encode(S),
                {ok, S2} = der_decode_secret_key(SecretDer),

                {ok, PublicDer} = der_encode(P),
                {ok, P2} = der_decode_public_key(PublicDer),

                S =:= S2 andalso P =:= P2
            end)).

prop_pem_iso() ->
    ?FORALL(RSAKeySize, key_size(),
        ?LET({S, P}, test_keypair(RSAKeySize),
            begin
                {ok, SecretPem} = pem_encode(S),
                {ok, S2} = pem_decode(SecretPem),

                {ok, PublicPem} = pem_encode(P),
                {ok, P2} = pem_decode(PublicPem),

                S =:= S2 andalso P =:= P2
            end)).

prop_sign_verify() ->
    ?FORALL({RSAKeySize, Message}, {key_size(), binary()},
        ?LET({S, P}, test_keypair(RSAKeySize),
            begin
                Signature = sign(sha, Message, S),
                verify(sha, Message, Signature, P)
            end)).

prop_sign_verify_failure() ->
    ?FORALL({RSAKeySize, Message}, {key_size(), binary()},
        ?LET({S, P}, test_keypair(RSAKeySize),
            begin
                Signature = sign(sha, Message, S),
                not verify(sha, <<"foo", Message/binary>>, Signature, P)
            end)).
-endif.