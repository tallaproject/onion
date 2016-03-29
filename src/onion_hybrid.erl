%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Tor Hybrid Encryption using RSA and AES
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_hybrid).

%% API.
-export([encrypt/2,
         decrypt/2
        ]).

-include("onion_test.hrl").

%% @doc Encrypt a given message.
-spec encrypt(PlainText, PublicKey) -> CipherText
    when
        PlainText  :: binary(),
        PublicKey  :: onion_rsa:public_key(),
        CipherText :: binary().
encrypt(PlainText, PublicKey) ->
    %% If M is less than PK_ENC_LEN-PK_PAD_LEN.
    PlainTextSize = byte_size(PlainText),
    case PlainTextSize < (128 - 42) of
        true ->
            %% Pad and encrypt M with PK.
            onion_rsa:public_encrypt(PlainText, PublicKey, rsa_pkcs1_oaep_padding);

        false ->
            %% Generate a KEY_LEN byte random key K.
            Key = onion_random:bytes(16),

            %% Let M1 = the first PK_ENC_LEN - PK_PAD_LEN - KEY_LEN bytes of M
            %% Let M2 = the rest of M.
            <<M1:70/binary, M2/binary>> = PlainText,

            %% Pad and encrypt K|M1 with PK.
            A = onion_rsa:public_encrypt(<<Key/binary, M1/binary>>, PublicKey, rsa_pkcs1_oaep_padding),

            %% Encrypt M2 with our stream cipher, using the key K.
            Stream = onion_aes:init(Key),
            {_, B} = onion_aes:encrypt(Stream, M2),

            %% Concatenate these encrypted values.
            <<A/binary, B/binary>>
    end.

%% @doc Decrypt a given message.
-spec decrypt(CipherText, SecretKey) -> PlainText
    when
        CipherText :: binary(),
        SecretKey  :: onion_rsa:secret_key(),
        PlainText  :: binary().
decrypt(CipherText, SecretKey) ->
    CipherTextSize = byte_size(CipherText),
    case CipherTextSize =< 128 of
        true ->
            onion_rsa:private_decrypt(CipherText, SecretKey, rsa_pkcs1_oaep_padding);

        false ->
            <<M1:128/binary, M2/binary>> = CipherText,
            <<Key:16/binary, A/binary>> = onion_rsa:private_decrypt(M1, SecretKey, rsa_pkcs1_oaep_padding),

            Stream = onion_aes:init(Key),
            {_, B} = onion_aes:decrypt(Stream, M2),

            <<A/binary, B/binary>>
    end.

-ifdef(TEST).

encrypt_decrypt_small_test() ->
    {ok, #{ secret := Secret, public := Public }} = onion_rsa:keypair(1024),
    Message = <<"Hello world!">>,
    [
        ?assertEqual(Message, decrypt(encrypt(Message, Public), Secret))
    ].

encrypt_decrypt_large_test() ->
    {ok, #{ secret := Secret, public := Public }} = onion_rsa:keypair(1024),
    Message = onion_random:bytes(2048),
    [
        ?assertEqual(Message, decrypt(encrypt(Message, Public), Secret))
    ].

-endif.
