%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Ed25519 API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_ed25519).

%% API.
-export([keypair/0,
         sign/2,
         open/3,
         public_key_from_secret_key/1,
         public_key_from_x25519_public_key/2,
         keypair_from_x25519_keypair/1
        ]).

%% Types.
-export_type([secret_key/0,
              public_key/0,
              keypair/0
             ]).

-type secret_key() :: ed25519_ref10:secret_key().
-type public_key() :: ed25519_ref10:public_key().
-type keypair()    :: ed25519_ref10:keypair().

-include("onion_test.hrl").

%% @doc Creates a new Ed25519 keypair.
%%
%% Generates and returns a new Ed25519 keypair. The return value is a map to
%% avoid using the public key as the secret key and vice versa.
%%
%% @end
-spec keypair() -> KeyPair
    when
        KeyPair :: keypair().
keypair() ->
    ed25519_ref10:keypair().

%% @doc Sign a given Message using a given SecretKey
%%
%% Returns a detached signature of a given Message using the given SecretKey.
%%
%% @end
-spec sign(Message, SecretKey) -> Signature
    when
        Message   :: iolist(),
        SecretKey :: secret_key(),
        Signature :: binary().
sign(Message, SecretKey) ->
    ed25519_ref10:sign(Message, SecretKey).


%% @doc Verify a Signature of a Message using the PublicKey
%%
%% Verifies a given Signature and Message using the given PublicKey.
%%
%% @end
-spec open(Message, Signature, PublicKey) -> boolean()
    when
        Signature :: binary(),
        Message   :: binary(),
        PublicKey :: public_key().
open(Signature, Message, PublicKey) ->
    ed25519_ref10:open(Signature, Message, PublicKey).

%% @doc Compute the public key from a given secret key.
%%
%% Computes an Ed 25519 public key from a given secret key.
%%
%% @end
-spec public_key_from_secret_key(SecretKey) -> PublicKey
    when
        SecretKey :: secret_key(),
        PublicKey :: public_key().
public_key_from_secret_key(SecretKey) ->
    ed25519_ref10:public_key(SecretKey).

%% @doc Return the matching Ed25519 public key from an x25519 public key.
%% @end
-spec public_key_from_x25519_public_key(X25519PublicKey, SignBit) -> PublicKey
    when
        X25519PublicKey :: onion_x25519:public_key(),
        SignBit         :: 0 | 1,
        PublicKey       :: public_key().
public_key_from_x25519_public_key(X25519PublicKey, SignBit) ->
    ed25519_ref10:public_key_from_x25519_public_key(X25519PublicKey, SignBit).

%% @doc Return the matching Ed25519 keypair from an x25519 keypair.
%% @end
-spec keypair_from_x25519_keypair(X25519KeyPair) -> {KeyPair, SignBit}
    when
        X25519KeyPair  :: onion_x25519:keypair(),
        KeyPair        :: keypair(),
        SignBit        :: 0 | 1.
keypair_from_x25519_keypair(X25519KeyPair) ->
    ed25519_ref10:keypair_from_x25519_keypair(X25519KeyPair).

-ifdef(TEST).
x25519_public_key_conversion_test() ->
    [
        ?assertEqual(base16_decode("6FC3E6E155EB221A3AB21AD60A04242B0805F73B5923C8C308FA6A502277DACA"),
                     public_key_from_x25519_public_key(base16_decode("F0A8D229CF1861B19A9532244E2C65042AA36D5F121BC2449F51D1AAD30B3328"), 1)),
        ?assertEqual(base16_decode("FE948C40083AC46897E8C2BFB1FCE1A9461A420AA3F55A964C9C2F67774017B6"),
                     public_key_from_x25519_public_key(base16_decode("0A1F5E2C0D2A4A7FDD94DA0882FDA5A8CFB850FC987436B093A420368441AC15"), 1)),
        ?assertEqual(base16_decode("7135EE6C966ECDD771094ABDD0D4A71C2884513922DD4E8A3E4125CE10F6B3F4"),
                     public_key_from_x25519_public_key(base16_decode("524962C4A3E20558307D93175CD634B05F34F50291F168894B9EEC270B556D6F"), 1))
    ].

x25519_keypair_conversion_1_test() ->
    X25519KeyPair = #{ secret => base16_decode("70CFF2118989237E0D923D2CD404F3E51F785E0687FC63A3A74B21985666E77E"),
                       public => base16_decode("36ECF106998BF6EF5DAC2A630DD2A8FE4F90625090E5D32D2EE5E65375347C25") },
    Ed25519KeyPair = #{ secret => base16_decode("70CFF2118989237E0D923D2CD404F3E51F785E0687FC63A3A74B21985666E77E68CC3C08312ADD636FF497B329BF5E37D7419A4FED50A0C9C79CF73357845F58"),
                        public => base16_decode("851B859F1EC9E2C87F36F943EFBC48BF7BBEEABA6ED3C2510C9AA95530F0678F") },
    SignBit = 1,
    [
        ?assertMatch({Ed25519KeyPair, SignBit}, keypair_from_x25519_keypair(X25519KeyPair)),
        ?assertEqual(maps:get(public, Ed25519KeyPair), public_key_from_x25519_public_key(maps:get(public, X25519KeyPair), SignBit))
    ].

x25519_keypair_conversion_2_test() ->
    X25519KeyPair = #{ secret => base16_decode("3840718865390DA1E93AF12E3293D57BBCC1E4F529C30B9B62F0E1ABEBD02157"),
                       public => base16_decode("453A6975E5E2E18FB248C3A5AC8163B41131D346BB95C12B313F3CBFD063E858") },
    Ed25519KeyPair = #{ secret => base16_decode("3840718865390DA1E93AF12E3293D57BBCC1E4F529C30B9B62F0E1ABEBD021572B1CDC671ECB8D3BDABB1109DF9E8F3221EE5882DE6848836BCA67A7C94E9B2D"),
                        public => base16_decode("6BEF0C559B20AAD38F59B7111F46C0E17A12686D36F9F6153D5F32441CD33112") },
    SignBit = 0,
    [
        ?assertMatch({Ed25519KeyPair, SignBit}, keypair_from_x25519_keypair(X25519KeyPair)),
        ?assertEqual(maps:get(public, Ed25519KeyPair), public_key_from_x25519_public_key(maps:get(public, X25519KeyPair), SignBit))
    ].

-endif. %% TEST.
