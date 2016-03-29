%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Onion RSA API
%%%
%%% This module contains utilities for working with RSA.
%%% It's made to simplify the use of Erlang's public_key
%%% and crypto applications for our specific use cases.
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

-include("onion_test.hrl").

%% @doc keypair/1 creates a new RSA keypair of a given bit-size.
%%
%% Generates and returns a new RSA keypair. The return value upon success is a
%% map to avoid using the public key as the secret key and vice versa.
%%
%% @end
-spec keypair(Bits) -> {ok, KeyPair} | {error, Reason}
    when
        Bits           :: pos_integer(),
        KeyPair        :: keypair(),
        Reason         :: term().
keypair(Bits) ->
    keypair(Bits, 65537).

%% @doc keypair/1 creates a new keypair of a given bit-size and with a specified public exponent.
%%
%% Generates and returns a new RSA keypair. The return value upon success is a
%% map to avoid using the public key as the secret key and vice versa.
%%
%% @end
-spec keypair(Bits, PublicExponent) -> {ok, KeyPair} | {error, Reason}
    when
        Bits           :: pos_integer(),
        PublicExponent :: pos_integer(),
        KeyPair        :: keypair(),
        Reason         :: term().
keypair(Bits, PublicExponent) ->
    case onion_nif:rsa_generate_private_key(Bits, PublicExponent) of
        SecretKeyDER when is_binary(SecretKeyDER) ->
            {ok, SecretKey} = der_decode_secret_key(SecretKeyDER),
            {ok, #{ secret => SecretKey,
                    public => secret_key_to_public_key(SecretKey) }};

        {error, _} = Error ->
            Error
    end.

%% @doc secret_key_to_public_key/1 creates a public key from a given secret key.
%%
%% Creates an RSA public key record from a given RSA secret key record by
%% copying the RSA modulus and public exponent of the secret key.
%%
%% @end
-spec secret_key_to_public_key(SecretKey) -> PublicKey
    when
        SecretKey :: secret_key(),
        PublicKey :: public_key().
secret_key_to_public_key(#'RSAPrivateKey'{ modulus = N, publicExponent = E }) ->
    #'RSAPublicKey'{
            modulus        = N,
            publicExponent = E
        }.

%% @doc der_encode/1 DER encodes a given public key or secret key.
%%
%% DER encodes a given public key or secret key and returns the key as a
%% binary() if the encoding was succesful.
%%
%% @end
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

%% @doc der_decode_secret_key/1 decodes a given binary into a secret key.
%%
%% Decodes a given binary into a secret key. This function will return an error
%% tuple if given an invalid DER encoded RSA secret key.
%%
%% @end
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

%% @doc der_decode_public_key/1 decodes a given binary into a public key.
%%
%% Decodes a given binary into a public key. This function will return an error
%% tuple if given an invalid DER encoded RSA public key.
%%
%% @end
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
    public_key:encrypt_private(PlainText, SecretKey, [{rsa_pad, Padding}]).

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
    public_key:decrypt_private(CipherText, SecretKey, [{rsa_pad, Padding}]).

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
    public_key:encrypt_public(PlainText, PublicKey, [{rsa_pad, Padding}]).

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
    public_key:decrypt_public(CipherText, PublicKey, [{rsa_pad, Padding}]).

-spec sign(DigestType, Message, SecretKey) -> Result
    when
        DigestType :: crypto:digest_type(),
        Message    :: binary() | {digest, binary()},
        SecretKey  :: secret_key(),
        Result     :: binary().
sign(DigestType, Message, SecretKey) ->
    public_key:sign(Message, DigestType, SecretKey).

-spec verify(DigestType, Message, Signature, PublicKey) -> boolean()
    when
        DigestType :: crypto:digest_type(),
        Message    :: binary() | {digest, binary()},
        Signature  :: binary(),
        PublicKey  :: public_key().
verify(DigestType, Message, Signature, PublicKey) ->
    public_key:verify(Message, DigestType, Signature, PublicKey).

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
