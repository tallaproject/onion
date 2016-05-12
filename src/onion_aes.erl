%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc AES API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_aes).

%% API.
-export([init/1,
         init/2,
         encrypt/2,
         decrypt/2
        ]).

-type state() :: term().

-include("onion_test.hrl").

-spec init(Key) -> State
    when
        Key   :: binary(),
        State :: state().
init(Key) ->
    init(Key, <<0:128>>).

-spec init(Key, IV) -> State
    when
        Key   :: binary(),
        IV    :: binary(),
        State :: state().
init(Key, IV) ->
    crypto:stream_init(aes_ctr, Key, IV).

-spec encrypt(State, PlainText) -> {NewState, CipherText}
    when
        State      :: state(),
        PlainText  :: binary(),
        NewState   :: state(),
        CipherText :: binary().
encrypt(State, PlainText) ->
    crypto:stream_encrypt(State, PlainText).

-spec decrypt(State, CipherText) -> {NewState, PlainText}
    when
        State      :: state(),
        CipherText :: binary(),
        NewState   :: state(),
        PlainText  :: binary().
decrypt(State, CipherText) ->
    encrypt(State, CipherText).

-ifdef(TEST).

sp800_38a_test() ->
    %% From: NIST's sp800-38a.pdf.
    Key = base16_decode("2b7e151628aed2a6abf7158809cf4f3c"),
    IV  = base16_decode("f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"),
    PlainText = base16_decode(["6bc1bee22e409f96e93d7e117393172a",
                               "ae2d8a571e03ac9c9eb76fac45af8e51",
                               "30c81c46a35ce411e5fbc1191a0a52ef",
                               "f69f2445df4f9b17ad2b417be66c3710"]),
    CipherText = base16_decode(["874d6191b620e3261bef6864990db6ce",
                                "9806f66b7970fdff8617187bb9fffdff",
                                "5ae4df3edbd5d35e5b4f09020db03eab",
                                "1e031dda2fbe03d1792170a0f3009cee"]),
    State = init(Key, IV),
    [
        ?assertMatch({_, PlainText}, decrypt(State, CipherText))
    ].

-endif. %% TEST.
