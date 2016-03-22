%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Property Tests for onion_ed25519.
%%% @end
%%% -----------------------------------------------------------
-module(prop_ed25519).

%% Properties.
-export([prop_sign_open/0,
         prop_x25519_key_conversion/0,
         prop_x25519_keypair_sign_open/0
        ]).

-include_lib("onion/include/onion_test.hrl").

-spec prop_sign_open() -> term().
prop_sign_open() ->
    ?FORALL({{S, P}, M}, {test_keypair(), binary()},
       begin
           Signature = onion_ed25519:sign(M, S),
           onion_ed25519:open(Signature, M, P)
       end).

-spec prop_x25519_key_conversion() -> term().
prop_x25519_key_conversion() ->
    ?FORALL(X25519KeyPair, onion_x25519:keypair(),
       begin
            {#{ public := Ed25519PublicKey }, SignBit} = onion_ed25519:keypair_from_x25519_keypair(X25519KeyPair),
            X25519PublicKey = maps:get(public, X25519KeyPair),
            Ed25519PublicKey =:= onion_ed25519:public_key_from_x25519_public_key(X25519PublicKey, SignBit) andalso
            binary:at(Ed25519PublicKey, 31) bsr 7 == SignBit
       end).

-spec prop_x25519_keypair_sign_open() -> term().
prop_x25519_keypair_sign_open() ->
    ?FORALL({X25519KeyPair, M}, {onion_x25519:keypair(), binary()},
       begin
           {#{ secret := S, public := P }, _SignBit} = onion_ed25519:keypair_from_x25519_keypair(X25519KeyPair),
           Signature = onion_ed25519:sign(M, S),
           onion_ed25519:open(Signature, M, P)
       end).

%% @private
-spec test_keypair() -> term().
test_keypair() ->
    #{ secret := SecretKey, public := PublicKey } = onion_ed25519:keypair(),
    {SecretKey, PublicKey}.
