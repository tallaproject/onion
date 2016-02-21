%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Math API.
%%% @end
%%% -----------------------------------------------------------
-module(onion_math).

%% API.
-export([ceil/1]).

-include("onion_test.hrl").

%% @doc Round a given number upwards towards to the nearest integer.
%% @end
-spec ceil(Value) -> integer()
    when
        Value :: number().
ceil(X) when X < 0 ->
    trunc(X);

ceil(X) ->
    V = trunc(X),
    case X - V == 0 of
        true ->
            V;

        false ->
            V + 1
    end.

-ifdef(TEST).
ceil_test() ->
    [
        ?assertEqual(ceil(-100.231), -100),
        ?assertEqual(ceil(-1.0001), -1),
        ?assertEqual(ceil(0.5), 1),
        ?assertEqual(ceil(2.00), 2),
        ?assertEqual(ceil(2.5), 3),
        ?assertEqual(ceil(0.1), 1)
    ].

-endif.
