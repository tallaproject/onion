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
         decode/1
        ]).

-include("onion_test.hrl").

-spec encode(Data) -> Base64Data
    when
        Data       :: binary(),
        Base64Data :: binary().
encode(Data) when is_binary(Data) ->
    onion_binary:trim(base64:encode(Data), <<"=">>).

-spec decode(Base64Data) -> {ok, Data} | {error, Reason}
    when
        Base64Data :: binary(),
        Data       :: binary(),
        Reason     :: term().
decode(Base64Data) when is_binary(Base64Data) ->
    try
        case byte_size(Base64Data) rem 4 of
            3 ->
                {ok, base64:decode(<<Base64Data/binary, "=">>)};
            2 ->
                {ok, base64:decode(<<Base64Data/binary, "==">>)};
            _ ->
                {ok, base64:decode(Base64Data)}
        end
    catch _:_ ->
        {error, invalid_base64}
    end.

-ifdef(TEST).
prop_base64_iso() ->
    ?FORALL(Data, binary(),
        begin
            Encoded = encode(Data),
            {ok, Decoded} = decode(Encoded),
            Data =:= Decoded
        end).

prop_base64_strip_equal_sign() ->
    ?FORALL(Data, binary(),
        begin
            Encoded = encode(Data),
            binary:match(Encoded, <<"=">>) =:= nomatch
        end).

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
