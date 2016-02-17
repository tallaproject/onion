%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Base16 API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_base16).

%% API.
-export([encode/1,
         decode/1
        ]).

-include("onion_test.hrl").

-spec encode(Data) -> Base16Data
    when
        Data       :: binary(),
        Base16Data :: binary().
encode(Data) when is_binary(Data) ->
    iolist_to_binary([integer_to_list(X, 16) || <<X:4/integer>> <= Data]).

-spec decode(Base16Data) -> {ok, Data} | {error, Reason}
    when
        Base16Data :: binary(),
        Data       :: binary(),
        Reason     :: term().
decode(Base16Data) when is_binary(Base16Data) ->
    try
        {ok, decode_base16_binary(Base16Data)}
    catch _:_ ->
        {error, invalid_base16}
    end.

%% @private
-spec decode_base16_binary(Base16Data) -> Data
    when
        Base16Data :: binary(),
        Data       :: binary().
decode_base16_binary(Base16Data) when is_binary(Base16Data) ->
    case Base16Data of
        <<>> ->
            <<>>;

        <<A:8/integer>> ->
            <<(list_to_integer([A], 16))>>;

        <<A:8/integer, B:8/integer, Rest/binary>> ->
            <<(list_to_integer([A, B], 16)), (decode_base16_binary(Rest))/binary>>
    end.

-ifdef(TEST).
prop_base16_iso() ->
    ?FORALL(Data, binary(),
        begin
            Encoded = encode(Data),
            {ok, Decoded} = decode(Encoded),
            Data =:= Decoded
        end).
-endif.
