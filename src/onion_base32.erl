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
         decode/1
        ]).

-include("onion_test.hrl").

-spec encode(Data) -> Base32Data
    when
        Data       :: binary(),
        Base32Data :: binary().
encode(Data) when is_binary(Data) ->
    base32:encode(Data, [lower]).

-spec decode(Base32Data) -> {ok, Data} | {error, Reason}
    when
        Base32Data :: binary(),
        Data       :: binary(),
        Reason     :: term().
decode(Base32Data) when is_binary(Base32Data) ->
    try
        {ok, base32:decode(Base32Data)}
    catch _:_ ->
        {error, invalid_base32}
    end.

-ifdef(TEST).
prop_base32_iso() ->
    ?FORALL(Data, binary(),
        begin
            Encoded = encode(Data),
            {ok, Decoded} = decode(Encoded),
            Data =:= Decoded
        end).
-endif.
