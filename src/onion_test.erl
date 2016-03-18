%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @private
%%%
%%% This module contains functions that are automatically
%%% imported by the onion_test.hrl file.
%%%
%%% -----------------------------------------------------------
-module(onion_test).

-ifdef(TEST).

%% API.
-export([base16_encode/1,
         base16_decode/1,

         base32_encode/1,
         base32_decode/1,

         base64_encode/1,
         base64_decode/1
        ]).

-spec base16_encode(Data) -> Encoded
    when
        Data    :: iolist(),
        Encoded :: binary().
base16_encode(Data) ->
    onion_base16:encode(iolist_to_binary(Data)).

-spec base16_decode(Data) -> Decoded
    when
        Data    :: iolist(),
        Decoded :: binary().
base16_decode(Data) ->
    {ok, Decoded} = onion_base16:decode(iolist_to_binary(Data)),
    Decoded.

-spec base32_encode(Data) -> Encoded
    when
        Data    :: iolist(),
        Encoded :: binary().
base32_encode(Data) ->
    onion_base32:encode(iolist_to_binary(Data)).

-spec base32_decode(Data) -> Decoded
    when
        Data    :: iolist(),
        Decoded :: binary().
base32_decode(Data) ->
    {ok, Decoded} = onion_base32:decode(iolist_to_binary(Data)),
    Decoded.

-spec base64_encode(Data) -> Encoded
    when
        Data    :: iolist(),
        Encoded :: binary().
base64_encode(Data) ->
    onion_base64:encode(iolist_to_binary(Data)).

-spec base64_decode(Data) -> Decoded
    when
        Data    :: iolist(),
        Decoded :: binary().
base64_decode(Data) ->
    {ok, Decoded} = onion_base64:decode(iolist_to_binary(Data)),
    Decoded.

-endif.
