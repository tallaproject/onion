%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Property Tests for onion_x25519.
%%% @end
%%% -----------------------------------------------------------
-module(prop_x25519).

%% Properties.
-export([prop_shared_secret/0]).

-include_lib("proper/include/proper.hrl").

-spec prop_shared_secret() -> term().
prop_shared_secret() ->
    ?FORALL({{AS, AP}, {BS, BP}}, {test_keypair(), test_keypair()},
        begin
            SharedA = onion_x25519:shared_secret(AS, BP),
            SharedB = onion_x25519:shared_secret(BS, AP),
            SharedA =:= SharedB
        end).

%% @private
-spec test_keypair() -> term().
test_keypair() ->
    #{ secret := SecretKey, public := PublicKey } = onion_x25519:keypair(),
    {SecretKey, PublicKey}.
