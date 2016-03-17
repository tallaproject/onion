%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc x25519 Diffie-Hellman API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_x25519).

%% API.
-export([keypair/0,
         secret_key_to_public_key/1,
         shared_secret/2,
         scalarmult/2
        ]).

%% Types.
-export_type([secret_key/0,
              public_key/0,
              keypair/0
             ]).

-type secret_key() :: binary().
-type public_key() :: binary().
-type keypair()    :: #{ secret => secret_key(),
                         public => public_key() }.

-include("onion_test.hrl").

%% @doc Creates a new x25519 Diffie-Hellman keypair.
%%
%% Generates and returns a new x25519 Diffie-Hellman keypair. The return value
%% is a map to avoid using the public key as the secret key and vice versa.
%%
%% @end
-spec keypair() -> KeyPair
    when
        KeyPair :: keypair().
keypair() ->
    enacl_ext:curve25519_keypair().

%% @doc Creates a PublicKey from a given SecretKey.
%%
%% This function creates an x25519 public key from a given x25519 secret key.
%%
%% @end
-spec secret_key_to_public_key(SecretKey) -> PublicKey
    when
        SecretKey :: secret_key(),
        PublicKey :: public_key().
secret_key_to_public_key(SecretKey) when is_binary(SecretKey) ->
    enacl_ext:curve25519_public_key(SecretKey).

%% @doc Computes the shared secret between a SecretKey and PublicKey.
%%
%% This function computes the shared secret between a given SecretKey and
%% PublicKey.
%%
%% @end
-spec shared_secret(SecretKey, PublicKey) -> SharedSecret
    when
        SecretKey    :: secret_key(),
        PublicKey    :: public_key(),
        SharedSecret :: binary().
shared_secret(SecretKey, PublicKey) ->
    enacl_ext:curve25519_shared(SecretKey, PublicKey).

%% @doc Computes the scalar multiplication between SecretKey and BasePoint.
%%
%% This function computes the scalar multiplication between a given SecretKey
%% and a given BasePoint.
%%
%% @end
-spec scalarmult(SecretKey, BasePoint) -> Result
    when
        SecretKey :: secret_key(),
        BasePoint :: binary(),
        Result    :: binary().
scalarmult(SecretKey, BasePoint) ->
    enacl:curve25519_scalarmult(SecretKey, BasePoint).

-ifdef(TEST).
base16_decode(V) ->
    {ok, Data} = onion_base16:decode(list_to_binary(V)),
    Data.

nacl_test() ->
    AliceSecretKey = base16_decode("77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"),
    AlicePublicKey = base16_decode("8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"),

    BobSecretKey = base16_decode("5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"),
    BobPublicKey = base16_decode("de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"),

    Shared = base16_decode("4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742"),
    [
        ?assertEqual(AlicePublicKey, secret_key_to_public_key(AliceSecretKey)),
        ?assertEqual(BobPublicKey, secret_key_to_public_key(BobSecretKey)),
        ?assertEqual(Shared, shared_secret(AliceSecretKey, BobPublicKey)),
        ?assertEqual(Shared, shared_secret(BobSecretKey, AlicePublicKey))
    ].

-endif.
