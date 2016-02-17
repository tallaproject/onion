%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc List Utility API
%%%
%%% This module contains various utility functions that are
%%% found useful when working with lists.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_lists).

%% API.
-export([intersperse/2]).

-include("onion_test.hrl").

%% @doc Intersperse List with Element.
-spec intersperse(Element :: term(), List :: [term()]) -> [term()].
intersperse(_, []) ->
    [];

intersperse(_, [X]) ->
    [X];

intersperse(Element, [X | Rest]) ->
    [X, Element | intersperse(Element, Rest)].

-ifdef(TEST).
intersperse_basic_test() ->
    [
        ?assertEqual(intersperse(foobar, []), []),
        ?assertEqual(intersperse(foobar, [a]), [a]),
        ?assertEqual(intersperse(foobar, [a, b]), [a, foobar, b]),
        ?assertEqual(intersperse(foobar, [a, b, c]), [a, foobar, b, foobar, c])
    ].
-endif.
