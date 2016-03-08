%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Base32 wrapper API
%%%
%%% This module contains our wrapper API for base32 encoding
%%% and decoding.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_base32).

%% API.
-export([encode/1,
         decode/1,
         valid/1
        ]).

%% Types.
-export_type([base32_encoded/0]).

-type base32_encoded() :: binary().

-include("onion_test.hrl").

-define(BASE32_ALPHABET, lists:seq($0, $9) ++
                         lists:seq($a, $z) ++
                         lists:seq($A, $Z)).

%% @doc Encode a given binary in Base32.
-spec encode(Data) -> Encoded
    when
        Data    :: binary(),
        Encoded :: base32_encoded().
encode(Data) when is_binary(Data) ->
    base32:encode(Data, [lower]).

%% @doc Try to decode a given Base32 encoded binary.
-spec decode(Encoded) -> {ok, Decoded} | {error, Reason}
    when
        Encoded :: base32_encoded(),
        Decoded :: binary(),
        Reason  :: term().
decode(Encoded) when is_binary(Encoded) ->
    try
        {ok, base32:decode(Encoded)}
    catch _:_ ->
        {error, invalid_base32}
    end.

%% @doc Check if a given binary is valid Base32.
-spec valid(Data) -> boolean()
    when
        Data :: binary() | string().
valid(Data) when is_list(Data) ->
    onion_string:valid(string:strip(Data, right, $=), ?BASE32_ALPHABET);

valid(Data) when is_binary(Data) ->
    valid(binary_to_list(Data)).

-ifdef(TEST).
-endif.
