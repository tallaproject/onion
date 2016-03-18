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
-export([prop_sign_verify/0]).

-include_lib("onion/include/onion_test.hrl").

-spec prop_sign_verify() -> term().
prop_sign_verify() ->
    ?FORALL({{S, P}, M}, {test_keypair(), binary()},
       begin
           Signature = onion_ed25519:sign(M, S),
           onion_ed25519:verify(Signature, M, P)
       end).

%% @private
-spec test_keypair() -> term().
test_keypair() ->
    #{ secret := SecretKey, public := PublicKey } = onion_ed25519:keypair(),
    {SecretKey, PublicKey}.
