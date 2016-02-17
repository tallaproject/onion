%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Utilities for working with random data.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_random).

%% API.
-export([bytes/1,
         bytes_unsigned/1,
         time_range/2,
         pick/1,
         coin_toss/0,
         hostname/4
        ]).

-include("onion_test.hrl").

%% @doc Return N random bytes.
-spec bytes(N) -> RandomBytes
    when
        N           :: non_neg_integer(),
        RandomBytes :: binary().
bytes(N) when N >= 0 ->
    enacl:randombytes(N).

%% @doc Return N random bytes as an unsigned integer.
-spec bytes_unsigned(N) -> RandomUnsigned
    when
        N              :: non_neg_integer(),
        RandomUnsigned :: non_neg_integer().
bytes_unsigned(N) when N >= 0 ->
    binary:decode_unsigned(bytes(N)).

%% @doc Return an UNIX epoch within Start and End.
-spec time_range(Min, Max) -> Result
    when
        Min    :: non_neg_integer(),
        Max    :: non_neg_integer(),
        Result :: non_neg_integer().
time_range(Min, Max) when Min < Max ->
    crypto:rand_uniform(Min, Max).

%% @doc Return a random element from a given list.
-spec pick(List) -> Element
    when
        Element :: term(),
        List    :: [Element].
pick(List) when is_list(List) ->
    N = crypto:rand_uniform(1, length(List) + 1),
    lists:nth(N, List).

%% @doc Toss a coin.
-spec coin_toss() -> head | tail.
coin_toss() ->
    case crypto:rand_uniform(0, 2) of
        1 ->
            head;
        0 ->
            tail
    end.

%% @doc Return random hostname.
-spec hostname(Min, Max, Prefix, Suffix) -> inet:hostname()
    when
        Min    :: non_neg_integer(),
        Max    :: non_neg_integer(),
        Prefix :: string(),
        Suffix :: string().
hostname(Min, Max, Prefix, Suffix) ->
    RandomLength = crypto:rand_uniform(Min, Max + 1),
    BytesRandomLength = ((RandomLength * 5) + 7) div 8,
    Bytes = case BytesRandomLength rem 5 of
                0 ->
                    bytes(BytesRandomLength);
                _ ->
                    bytes(BytesRandomLength + (5 - (BytesRandomLength rem 5)))
            end,
    lists:flatten([Prefix, binary_to_list(onion_base32:encode(Bytes)), Suffix]).

-ifdef(TEST).
prop_time_range() ->
    ?FORALL({Min, Max}, {non_neg_integer(), non_neg_integer()},
        begin
            Value = time_range(Min, Min + Max),
            Min =< Value andalso (Min + Max - 1) >= Value
        end).
-endif.
