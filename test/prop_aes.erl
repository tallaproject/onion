%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Property Tests for onion_aes.
%%% @end
%%% -----------------------------------------------------------
-module(prop_aes).

%% Properties.
-export([prop_iso/0]).

-include_lib("proper/include/proper.hrl").

-spec prop_iso() -> term().
prop_iso() ->
    ?FORALL({Key, Message}, {binary(128), binary()},
        begin
            AES = onion_aes:init(Key),
            {_, CipherText} = onion_aes:encrypt(AES, Message),
            {_, PlainText}  = onion_aes:decrypt(AES, CipherText),

            CipherText =/= PlainText andalso PlainText =:= Message
        end).
