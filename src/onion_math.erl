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
-export([ceil/1,
         floor/1,
         pow/2,
         mod/2,
         mod_pow/3
        ]).

-include("onion_test.hrl").

%% @doc Round a given number upwards towards to the nearest integer.
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

%% @doc Round a given number downwards towards the nearest integer.
-spec floor(Value) -> integer()
    when
        Value :: number().
floor(X) when X < 0 ->
    V = trunc(X),
    case X - V == 0 of
        true ->
            V;

        false ->
            V - 1
    end;

floor(X) ->
    trunc(X).

%% @doc Return X^N.
-spec pow(X, N) -> integer()
    when
        X :: integer(),
        N :: integer().
pow(X, N) ->
    trunc(math:pow(X, N)).

%% @doc Return X mod Y.
-spec mod(X, Y) -> integer()
    when
        X :: integer(),
        Y :: integer().
mod(X, Y) ->
    trunc(X rem Y).

%% @doc Return B^E mod M.
-spec mod_pow(Base, Exponent, Modulus) -> Result
    when
        Base     :: number(),
        Exponent :: number(),
        Modulus  :: number(),
        Result   :: number().
mod_pow(_, _, 1) ->
    0;

mod_pow(B, E, M) ->
    Result = 1,
    Base   = mod(B, M),
    do_mod_pow(Base, E, M, Result).

%% @private
-spec do_mod_pow(Base, Exponent, Modulus, Result) -> Result
    when
        Base     :: number(),
        Exponent :: number(),
        Modulus  :: number(),
        Result   :: number().
do_mod_pow(B, E, M, Result) when E > 0 ->
    NewB      = mod(B * B, M),
    NewE      = E bsr 1,
    NewResult = case mod(E, 2) of
                    1 ->
                        mod(Result * B, M);

                    _ ->
                        Result
                end,
    do_mod_pow(NewB, NewE, M, NewResult);

do_mod_pow(_, _, _, Result) ->
    Result.

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

floor_test() ->
    [
        ?assertEqual(floor(-100.231), -101),
        ?assertEqual(floor(-1.0001), -2),
        ?assertEqual(floor(0.5), 0),
        ?assertEqual(floor(2.00), 2),
        ?assertEqual(floor(2.5), 2),
        ?assertEqual(floor(0.1), 0)
    ].

pow_test() ->
    [
        ?assertEqual(pow(1, 0), 1),
        ?assertEqual(pow(2, 2), 4),
        ?assertEqual(pow(2, 255), 57896044618658097711785492504343953926634992332820282019728792003956564819968)
    ].

mod_test() ->
    [
        ?assertEqual(mod(2, 2), 0),
        ?assertEqual(mod(2, 4), 2),
        ?assertEqual(mod(2, 3), 2),
        ?assertEqual(mod(3, 9), 3)
    ].

-endif.
