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
         decode/1,
         valid/1
        ]).

%% Types.
-export_type([base16_encoded/0]).

-type base16_encoded() :: binary().

-include("onion_test.hrl").

-define(BASE16_ALPHABET, lists:seq($0, $9) ++
                         lists:seq($a, $f) ++
                         lists:seq($A, $F)).

%% @doc Encode a given binary in Base16 (Hex).
-spec encode(Data) -> Encoded
    when
        Data    :: binary(),
        Encoded :: base16_encoded().
encode(Data) when is_binary(Data) ->
    iolist_to_binary([integer_to_list(X, 16) || <<X:4/integer>> <= Data]).

%% @doc Try to decode a given Base16 encoded binary.
-spec decode(Encoded) -> {ok, Decoded} | {error, Reason}
    when
        Encoded :: base16_encoded(),
        Decoded :: binary(),
        Reason  :: term().
decode(Encoded) when is_binary(Encoded) ->
    try
        {ok, decode_base16_binary(Encoded)}
    catch _:_ ->
        {error, invalid_base16}
    end.

%% @doc Check if a given binary is valid Base16.
-spec valid(Data) -> boolean()
    when
        Data :: binary() | string().
valid(Data) when is_list(Data) ->
    onion_string:valid(Data, ?BASE16_ALPHABET);

valid(Data) when is_binary(Data) ->
    valid(binary_to_list(Data)).

%% @private
-spec decode_base16_binary(Encoded) -> Data
    when
        Encoded :: base16_encoded(),
        Data    :: binary().
decode_base16_binary(Encoded) when is_binary(Encoded) ->
    case Encoded of
        <<>> ->
            <<>>;

        <<A:8/integer>> ->
            <<(list_to_integer([A], 16))>>;

        <<A:8/integer, B:8/integer, Rest/binary>> ->
            <<(list_to_integer([A, B], 16)), (decode_base16_binary(Rest))/binary>>
    end.
