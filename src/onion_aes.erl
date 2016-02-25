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
         encrypt/2,
         decrypt/2
        ]).

-type state() :: term().

-spec init(Key) -> State
    when
        Key   :: binary(),
        State :: state().
init(Key) ->
    crypto:stream_init(aes_ctr, Key, <<0:128>>).

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
