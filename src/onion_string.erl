%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc String Utility API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_string).

%% API.
-export([format/2,
         valid/2
        ]).

-include("onion_test.hrl").

-spec format(Format, Data) -> string()
    when
        Format :: string(),
        Data   :: [term()].
format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

%% @doc Check a given string if it validates against a given alphabet.
%%
%% This function checks a given string if all of its characters are a member
%% of a given alphabet.
%%
%% @end
-spec valid(String, Alphabet) -> boolean()
    when
        String   :: string(),
        Alphabet :: string().
valid(String, Alphabet) ->
    Set = ordsets:from_list(Alphabet),
    lists:all(fun (Character) ->
                  ordsets:is_element(Character, Set)
              end, String).

-ifdef(TEST).

valid_test() ->
    [
        ?assert(valid("", "")),
        ?assert(valid("", "abc")),
        ?assert(valid("a", "a")),
        ?assert(valid("a", "aa")),
        ?assert(valid("aa", "a")),
        ?assert(valid("abc", "abc")),
        ?assert(valid("abc", "abcdefg"))
    ].

valid_error_test() ->
    [
        ?assertNot(valid("a", "")),
        ?assertNot(valid("ab", "")),
        ?assertNot(valid("abc", "")),
        ?assertNot(valid("abc", "ab"))
    ].

-endif.
