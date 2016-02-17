%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Document API.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_document).

%% API.
-export([get_item/2,
         split/2,
         decode/1]).

-include("onion_test.hrl").

-spec get_item(Keyword, Document) -> Item | not_found
    when
        Keyword   :: binary(),
        Arguments :: [binary()],
        Object    :: [binary()],
        Item      :: {Keyword, Arguments} | {Keyword, Arguments, Object},
        Document  :: [Item].
get_item(Keyword, Document) ->
    case lists:keysearch(keyword(Keyword), 1, Document) of
        false ->
            not_found;

        {value, Item} ->
            Item
    end.

-spec split(Document, Keyword) -> [Document]
    when
        Keyword   :: binary(),
        Arguments :: [binary()],
        Object    :: [binary()],
        Item      :: {Keyword, Arguments} | {Keyword, Arguments, Object},
        Document  :: [Item].
split(Document, Keyword) ->
    split(Document, Keyword, [], []).

split([], _Keyword, Data, Result) ->
    lists:reverse([lists:reverse(Data) | Result]);

split([{Keyword, _} = Item | Rest], Keyword, Data, Result) ->
    case Data of
        [] ->
            split(Rest, Keyword, [Item], Result);

        _ ->
            split(Rest, Keyword, [Item], [lists:reverse(Data) | Result])
    end;

split([{Keyword, _, _} = Item | Rest], Keyword, Data, Result) ->
    case Data of
        [] ->
            split(Rest, Keyword, [Item], Result);

        _ ->
            split(Rest, Keyword, [Item], [lists:reverse(Data) | Result])
    end;

split([Item | Rest], Keyword, Data, Result) ->
    split(Rest, Keyword, [Item | Data], Result).

-spec decode(Data) -> {ok, [Item]} | {error, term()}
    when
        Data      :: binary(),
        Item      :: {Keyword, Arguments} | {Keyword, Arguments, Object},
        Keyword   :: binary(),
        Arguments :: [binary()],
        Object    :: [binary()].
decode(Data) when is_binary(Data) ->
    Lines = binary:split(Data, <<"\n">>, [global, trim]),
    decode_lines(Lines, none, []).

decode_lines([], Item, Items) ->
    case Item of
        none ->
            {ok, lists:reverse(Items)};

        {_, _} = Item ->
            {ok, lists:reverse([Item | Items])};

        {_, _, _} ->
            %% A partial object.
            {error, invalid_document}
    end;

decode_lines([<<"-----BEGIN ", _/binary>> = ObjectBegin | Rest], {Keyword, Arguments}, Items) ->
    decode_lines(Rest, {Keyword, Arguments, [ObjectBegin]}, Items);

decode_lines([<<"-----END ", _/binary>> = ObjectEnd | Rest], {Keyword, Arguments, ObjectLines}, Items) ->
    decode_lines(Rest, none, [{Keyword, Arguments, lists:reverse([ObjectEnd | ObjectLines])} | Items]);

decode_lines([ObjectLine | Rest], {Keyword, Arguments, ObjectLines}, Items) ->
    decode_lines(Rest, {Keyword, Arguments, [ObjectLine | ObjectLines]}, Items);

decode_lines([Line | Rest], Item, Items) ->
    [Keyword | Arguments] = binary:split(Line, <<" ">>, [global]),
    case Item of
        none ->
            decode_lines(Rest, {Keyword, Arguments}, Items);

        Item ->
            decode_lines(Rest, {Keyword, Arguments}, [Item | Items])
    end.

%% @private
-spec keyword(term()) -> binary().
keyword(V) when is_atom(V) ->
    atom_to_binary(V, latin1);

keyword(V) when is_list(V) ->
    list_to_binary(V);

keyword(V) when is_binary(V) ->
    V.

-ifdef(TEST).
decode_basic_test() ->
    [
        ?assertEqual(decode(<<>>), {ok, []}),
        ?assertEqual(decode(<<"foobar">>), {ok, [{<<"foobar">>, []}]}),

        ?assertEqual(decode(<<"foobar\nfoobar\n">>), {ok, [
                                                           {<<"foobar">>, []},
                                                           {<<"foobar">>, []}
                                                          ]}),

        ?assertEqual(decode(<<"foo a b c\nbar d e f\n">>), {ok, [
                                                           {<<"foo">>, [<<"a">>, <<"b">>, <<"c">>]},
                                                           {<<"bar">>, [<<"d">>, <<"e">>, <<"f">>]}
                                                          ]}),

        ?assertEqual(decode(<<"foo\n-----BEGIN foobar-----\nblah\n-----END foobar------\n">>),
                     {ok, [{<<"foo">>, [], [<<"-----BEGIN foobar-----">>, <<"blah">>, <<"-----END foobar------">>]}]}),

        ?assertEqual(decode(<<"foo\n-----BEGIN foobar-----\nblah\n">>), {error, invalid_document})
    ].
-endif.
