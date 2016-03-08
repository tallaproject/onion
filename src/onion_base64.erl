%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Base64 wrapper API
%%%
%%% This module contains a base64 API that allows us to encode
%%% and decode binaries without the ordinary Base64 padding.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_base64).

%% API.
-export([encode/1,
         decode/1,
         valid/1
        ]).

%% Types.
-export_type([base64_encoded/0]).

-type base64_encoded() :: binary().

-include("onion_test.hrl").

-define(BASE64_ALPHABET, lists:seq($0, $9) ++
                         lists:seq($a, $z) ++
                         lists:seq($A, $Z) ++
                         "+/").

-spec encode(Data) -> Encoded
    when
        Data    :: binary(),
        Encoded :: base64_encoded().
encode(Data) when is_binary(Data) ->
    onion_binary:trim(base64:encode(Data), <<"=">>).

-spec decode(Encoded) -> {ok, Decoded} | {error, Reason}
    when
        Encoded :: base64_encoded(),
        Decoded :: binary(),
        Reason  :: term().
decode(Encoded) when is_binary(Encoded) ->
    try
        case byte_size(Encoded) rem 4 of
            3 ->
                {ok, base64:decode(<<Encoded/binary, "=">>)};
            2 ->
                {ok, base64:decode(<<Encoded/binary, "==">>)};
            _ ->
                {ok, base64:decode(Encoded)}
        end
    catch _:_ ->
        {error, invalid_base64}
    end.

%% @doc Check if a given binary is valid Base64.
-spec valid(Data) -> boolean()
    when
        Data :: binary() | string().
valid(Data) when is_list(Data) ->
    onion_string:valid(Data, ?BASE64_ALPHABET);

valid(Data) when is_binary(Data) ->
    valid(binary_to_list(Data)).

-ifdef(TEST).
base64_rfc4648_encode_test() ->
    [
        ?assertEqual(encode(<<>>), <<>>),
        ?assertEqual(encode(<<"f">>), <<"Zg">>),
        ?assertEqual(encode(<<"fo">>), <<"Zm8">>),
        ?assertEqual(encode(<<"foo">>), <<"Zm9v">>),
        ?assertEqual(encode(<<"foob">>), <<"Zm9vYg">>),
        ?assertEqual(encode(<<"fooba">>), <<"Zm9vYmE">>),
        ?assertEqual(encode(<<"foobar">>), <<"Zm9vYmFy">>)
    ].

base64_rfc4648_decode_test() ->
    [
        ?assertEqual(decode(<<>>), {ok, <<>>}),
        ?assertEqual(decode(<<"Zg">>), {ok, <<"f">>}),
        ?assertEqual(decode(<<"Zm8">>), {ok, <<"fo">>}),
        ?assertEqual(decode(<<"Zm9v">>), {ok, <<"foo">>}),
        ?assertEqual(decode(<<"Zm9vYg">>), {ok, <<"foob">>}),
        ?assertEqual(decode(<<"Zm9vYmE">>), {ok, <<"fooba">>}),
        ?assertEqual(decode(<<"Zm9vYmFy">>), {ok, <<"foobar">>})
    ].
-endif.
