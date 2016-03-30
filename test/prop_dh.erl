%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Property Tests for onion_dh.
%%% @end
%%% -----------------------------------------------------------
-module(prop_dh).

%% Properties.
-export([prop_shared_secret/0,
         prop_degenerate/0,
         prop_public_key_not_degenerate/0
        ]).

-include_lib("onion/include/onion_test.hrl").

-spec prop_shared_secret() -> term().
prop_shared_secret() ->
    ?FORALL({{AS, AP}, {BS, BP}}, {test_keypair(), test_keypair()},
        begin
            {ok, SharedA} = onion_dh:shared_secret(AS, BP),
            {ok, SharedB} = onion_dh:shared_secret(BS, AP),
            SharedA =:= SharedB
        end).

-spec prop_degenerate() -> term().
prop_degenerate() ->
    ?FORALL(BadPublicKey, bad_public_key(),
        begin
            onion_dh:is_degenerate(BadPublicKey)
        end).

-spec prop_public_key_not_degenerate() -> term().
prop_public_key_not_degenerate() ->
    ?FORALL({_, P}, test_keypair(),
        begin
            not onion_dh:is_degenerate(P)
        end).

%% @private
-spec bad_public_key() -> term().
bad_public_key() ->
    ?LET(P, p(),
        begin
            oneof([
                neg_integer(),
                integer(0, 1),
                integer(P - 1, inf)
            ])
        end).

%% @private
-spec p() -> term().
p() ->
    ?LET(L, onion_dh:params(), hd(L)).

%% @private
-spec test_keypair() -> term().
test_keypair() ->
    #{ secret := SecretKey, public := PublicKey } = onion_dh:keypair(),
    {SecretKey, PublicKey}.
