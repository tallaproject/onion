%%%
%%% Copyright (c) 2017 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Lasse Grinderslev Andersen <lasse@etableret.dk>
%%% @doc Grammar file for Tor directory documents parser.
%%%
%%% @end
%%% -----------------------------------------------------------

Nonterminals 
document item items.

Terminals
object keyword keyword_ws_arguments.

Rootsymbol document.

document -> items : '$1'.

item -> keyword object              : item_record('$1', '$2'). 
item -> keyword_ws_arguments object : item_record('$1', '$2').
item -> keyword                     : item_record('$1', no_object).
item -> keyword_ws_arguments        : item_record('$1', no_object). 

items -> item       : ['$1'].
items -> item items : ['$1' | '$2'].


Erlang code.

-type token_line() :: non_neg_integer().

-define(KEYWORD_ARGS(KeywordArgs), {keyword_ws_arguments, _, KeywordArgs}).
-define(KEYWORD(Keyword), {keyword, _, Keyword}).
-define(OBJECT(Object), {object, _, Object}).

-spec item_record(KeywordArgs, ObjectToken) -> {Keyword, Arguments, Object}
    when
        KeywordArgs :: {keyword, token_line(), Keyword} |
                       {keyword_ws_arguments, token_line(), {Keyword, binary()}},
        ObjectToken :: {object, token_line(), binary()} | no_object,
        Keyword     :: binary() | atom(),
        Arguments   :: binary() | no_arguments,
        Object      :: binary() | no_object.
item_record(?KEYWORD(Keyword), ?OBJECT(Object)) ->
    {Keyword, no_arguments, Object};

item_record(?KEYWORD_ARGS({Keyword, Arguments}), ?OBJECT(Object)) ->
    {Keyword, Arguments, Object};

item_record(?KEYWORD(Keyword), no_object) ->
    {Keyword, no_arguments, no_object};

item_record(?KEYWORD_ARGS({Keyword, Arguments}), no_object) ->
    {Keyword, Arguments, no_object}.
