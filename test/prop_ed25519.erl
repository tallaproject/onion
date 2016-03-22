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
-export([prop_point_on_curve/0,
         prop_point_coding_iso/0,
         prop_sign_verify/0,
         prop_x25519_key_conversion/0,
         prop_x25519_keypair_sign_verify/0
        ]).

-include_lib("onion/include/onion_test.hrl").

-spec prop_point_on_curve() -> term().
prop_point_on_curve() ->
    ?FORALL({_, PublicKey}, test_keypair(),
        begin
            Point = onion_ed25519:point_decode(PublicKey),
            onion_ed25519:point_on_curve(Point)
        end).

-spec prop_point_coding_iso() -> term().
prop_point_coding_iso() ->
    ?FORALL({_, PublicKey}, test_keypair(),
        begin
            Point = onion_ed25519:point_decode(PublicKey),
            PublicKey =:= onion_ed25519:point_encode(Point)
        end).

-spec prop_sign_verify() -> term().
prop_sign_verify() ->
    ?FORALL({{S, P}, M}, {test_keypair(), binary()},
       begin
           Signature = onion_ed25519:sign(M, S),
           onion_ed25519:verify(Signature, M, P)
       end).

-spec prop_x25519_key_conversion() -> term().
prop_x25519_key_conversion() ->
    ?FORALL(X25519KeyPair, onion_x25519:keypair(),
       begin
           #{ public := Ed25519PublicKey } = onion_ed25519:keypair_from_x25519_keypair(X25519KeyPair),
           X25519PublicKey = maps:get(public, X25519KeyPair),
           Ed25519PublicKey =:= onion_ed25519:public_key_from_x25519_public_key(X25519PublicKey,
                                                                                binary:at(Ed25519PublicKey, 31) bsr 7)
       end).

-spec prop_x25519_keypair_sign_verify() -> term().
prop_x25519_keypair_sign_verify() ->
    ?FORALL({X25519KeyPair, M}, {onion_x25519:keypair(), binary()},
       begin
           #{ public := _P, secret := S } = onion_ed25519:keypair_from_x25519_keypair(X25519KeyPair),
           P = enacl:crypto_sign_ed25519_sk_to_pk(S),
           Signature = onion_ed25519:sign(M, S),
           onion_ed25519:verify(Signature, M, P)
       end).

%% @private
-spec test_keypair() -> term().
test_keypair() ->
    #{ secret := SecretKey, public := PublicKey } = onion_ed25519:keypair(),
    {SecretKey, PublicKey}.
