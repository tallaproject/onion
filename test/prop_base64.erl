%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Property Tests for onion_base64
%%% @end
%%% -----------------------------------------------------------
-module(prop_base64).

%% Properties.
-export([prop_base64_iso/0,
         prop_base64_strip_equal_sign/0
        ]).

-include_lib("proper/include/proper.hrl").

-spec prop_base64_iso() -> term().
prop_base64_iso() ->
    ?FORALL(Data, binary(),
        begin
            Encoded = onion_base64:encode(Data),
            true = onion_base64:valid(Encoded),
            {ok, Decoded} = onion_base64:decode(Encoded),
            Data =:= Decoded
        end).

-spec prop_base64_strip_equal_sign() -> term().
prop_base64_strip_equal_sign() ->
    ?FORALL(Data, binary(),
        begin
            Encoded = onion_base64:encode(Data),
            binary:match(Encoded, <<"=">>) =:= nomatch
        end).
