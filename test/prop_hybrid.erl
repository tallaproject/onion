%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Property Tests for onion_hybrid.
%%% @end
%%% -----------------------------------------------------------
-module(prop_hybrid).

%% Properties.
-export([prop_iso/0]).

-include_lib("onion/include/onion_test.hrl").

-spec prop_iso() -> term().
prop_iso() ->
    ?FORALL({{S, P}, Message}, {test_keypair(), binary()},
        %% Comes from the same issue in prop_rsa:prop_encrypt_decrypt/0.
        ?IMPLIES(Message =/= <<>>,
            begin
                Encrypted = onion_hybrid:encrypt(Message, P),
                Decrypted = onion_hybrid:decrypt(Encrypted, S),
                Message =:= Decrypted
            end)).

%% @private
-spec test_keypair() -> term().
test_keypair() ->
    {ok, #{ secret := Secret, public := Public }} = onion_rsa:keypair(1024),
    {Secret, Public}.
