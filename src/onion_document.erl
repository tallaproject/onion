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
-export([decode/1,
         
         encode/1,
         get_item/2,
         split/2,
         ed25519_sign/2,
         rsa_sign/2]).

-include("onion_test.hrl").

%% @doc Decode a Tor Document.
%%
%% Decodes a document according to the format described in section 1.2
%% of dir-spec.txt.
%%
%% @end
-spec decode(Data) -> {ok, [Item]} | {error, invalid_document}
    when
        Data :: binary(),
        Item :: {Keyword, Arguments, Object},
        Keyword   :: atom() | binary(),
        Arguments :: binary(),
        Object    :: term().
decode(<<>>) ->
    {ok, []};

decode(Document) when is_binary(Document) ->
    decode(erlang:binary_to_list(Document));

decode(Document) ->
    try decode_(Document) of
        Items ->
            Items
    catch
        _Error:_WhatNow ->
            {error, invalid_document}
    end.

%% @private
-spec decode_(Document) -> {ok, [Item]}
    when
        Document :: binary(),
        Item     :: term().
decode_(Document) ->
    {ok, Tokens, _EndLine} = onion_document_lexer:string(Document),
    onion_document_parser:parse(Tokens).


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
        Data      :: binary().
encode(Document) when is_list(Document) ->
    iolist_to_binary(lists:map(fun encode_entry/1, Document)).

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

ed25519_sign(Document, SecretKey) ->
    EncodedDocument = encode(Document ++ [{'router-sig-ed25519', [<<>>]}]),
    Prefix = <<"Tor router descriptor signature v1">>,
    Signature = onion_ed25519:sign(<<Prefix/binary, EncodedDocument/binary>>, SecretKey),
    Document ++ [{'router-sig-ed25519', [onion_base64:encode(Signature)]}].

rsa_sign(Document, SecretKey) ->
    EncodedDocument = encode(Document ++ [{'router-signature', []}]),
    Hash = crypto:hash(sha, EncodedDocument),
    Signature = onion_rsa:private_encrypt(Hash, SecretKey, rsa_pkcs1_padding),
    Document ++ [{'router-signature', [], [{'SIGNATURE', Signature}]}].

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
encode_arguments(Arguments) ->
    encode_arguments(Arguments, []).

%% @private
encode_arguments([], Arguments) ->
    lists:reverse(Arguments);

encode_arguments([Argument | Arguments], Acc) ->
    encode_arguments(Arguments, [encode_argument(Argument) | Acc]).

%% @private
encode_argument({datetime, {{Year, Month, Day}, {Hour, Minute, Second}}}) ->
    onion_string:format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b",
                        [Year, Month, Day, Hour, Minute, Second]);

encode_argument(Argument) ->
    Argument.

%% @private
-spec encode_entry(DocumentEntry) -> iolist()
    when
        DocumentEntry :: {Keyword, Arguments, Objects},
        Keyword   :: string() | atom() | binary(),
        Arguments :: [binary()],
        Objects   :: [Object],
        Object    :: term().
encode_entry({Keyword, Arguments}) ->
    encode_entry({Keyword, Arguments, []});

encode_entry({Keyword, Arguments, Objects}) ->
    [onion_lists:intersperse(<<" ">>, lists:map(fun keyword/1, [keyword(Keyword) | encode_arguments(Arguments)])), <<"\n">>,
     lists:map(fun encode_object/1, Objects)];

encode_entry(List) when is_list(List) ->
    lists:map(fun encode_entry/1, List).

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
        ?assertEqual(decode(<<"foobar">>), {ok, [{foobar, no_arguments, no_object}]}),

        ?assertEqual(decode(<<"foobar\nfoobar\n">>), {ok, [
                                                           {foobar, no_arguments, no_object},
                                                           {foobar, no_arguments, no_object}
                                                          ]}),

        ?assertEqual(decode(<<"foo a b c\nbar d e f\n">>), {ok, [
                                                           {foo, <<"a b c">>, no_object},
                                                           {bar, <<"d e f">>, no_object}
                                                          ]}),

        ?assertEqual(decode(<<"foo\n-----BEGIN foobar-----\nblah\n-----END foobar-----\n">>),
                     {ok, [
                           {foo, no_arguments, {<<"foobar">>, <<"blah">>}}
                          ]}),

        ?assertEqual(decode(<<"?foo\n">>), {error, invalid_document})
    ].
-endif.
