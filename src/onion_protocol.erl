%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Protocol Utilities.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_protocol).

%% API.
-export([supported/0,

         shared_protocol/1,
         shared_protocol/2
        ]).

%% Types.
-export_type([version/0]).

-type version() :: 3 | 4.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-endif.

-spec supported() -> [version()].
supported() ->
    [3, 4].

-spec shared_protocol(Other) -> {ok, version()} | {error, Reason}
    when
        Other  :: [version()],
        Reason :: term().
shared_protocol(Other) ->
    shared_protocol(Other, supported()).

-spec shared_protocol(Other, Self) -> {ok, version()} | {error, Reason}
    when
        Other  :: [version()],
        Self   :: [version()],
        Reason :: term().
shared_protocol(Other, Self) ->
    A = ordsets:from_list(Other),
    B = ordsets:from_list(Self),
    case ordsets:intersection(A, B) of
        [] ->
            {error, version_mismatch};

        BestProtocols ->
            {ok, lists:last(BestProtocols)}
    end.

-ifdef(TEST).
shared_protocol_test() ->
    [
        ?assertEqual(shared_protocol([3, 4], [3, 4]), {ok, 4}),
        ?assertEqual(shared_protocol([3, 4], [3, 3]), {ok, 3}),
        ?assertEqual(shared_protocol([3, 3], [3, 4]), {ok, 3})
    ].

shared_protocol_error_test() ->
    [
        ?assertEqual(shared_protocol([], []), {error, version_mismatch}),
        ?assertEqual(shared_protocol([3, 3], [4, 4]), {error, version_mismatch})
    ].
-endif.
