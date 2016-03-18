%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Property Tests for onion_base32
%%% @end
%%% -----------------------------------------------------------
-module(prop_base32).

%% Properties.
-export([prop_base32_iso/0]).

-include_lib("onion/include/onion_test.hrl").

-spec prop_base32_iso() -> term().
prop_base32_iso() ->
    ?FORALL(Data, binary(),
        begin
            Encoded = onion_base32:encode(Data),
            true = onion_base32:valid(Encoded),
            {ok, Decoded} = onion_base32:decode(Encoded),
            Data =:= Decoded
        end).
