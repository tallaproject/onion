%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc String Utility API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_string).

%% API.
-export([format/2]).

-spec format(Format, Data) -> string()
    when
        Format :: string(),
        Data   :: [term()].
format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).
