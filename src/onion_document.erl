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
-export([encode/1,
         get_item/2,
         split/2,
         decode/1]).

-include("onion_test.hrl").

%% @doc Encode a given Document into an iolist().
%%
%% This function encodes a given Document into an iolist().
%%
%% @end
-spec encode(Document) -> Data
    when
        Document  :: [Item],
        Item      :: {Keyword, Arguments, Objects},
        Keyword   :: string() | atom() | binary(),
        Arguments :: [binary()],
        Objects   :: [Object],
        Object    :: term(),
        Data      :: iolist().
encode(Document) when is_list(Document) ->
    lists:map(fun encode_document_entry/1, Document).

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

keyword(V) when is_integer(V) ->
    integer_to_binary(V);

keyword(V) when is_binary(V) ->
    V.

%% @private
-spec encode_document_entry(DocumentEntry) -> iolist()
    when
        DocumentEntry :: {Keyword, Arguments, Objects},
        Keyword   :: string() | atom() | binary(),
        Arguments :: [binary()],
        Objects   :: [Object],
        Object    :: term().
encode_document_entry({Keyword, Arguments}) ->
    encode_document_entry({Keyword, Arguments, []});

encode_document_entry({Keyword, Arguments, Objects}) ->
    [onion_lists:intersperse(<<" ">>, lists:map(fun keyword/1, [keyword(Keyword) | Arguments])), <<"\n">>,
     lists:map(fun encode_object/1, Objects)].

%% @private
-spec encode_object(Object) -> iolist()
    when
        Object :: term().
encode_object({Type, Data}) ->
    [onion_pem:encode(Type, Data)].

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
