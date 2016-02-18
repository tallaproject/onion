%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Property Tests for onion_random.
%%% @end
%%% -----------------------------------------------------------
-module(prop_random).

%% Properties.
-export([prop_time_range/0]).

-include_lib("proper/include/proper.hrl").

-spec prop_time_range() -> term().
prop_time_range() ->
    ?FORALL({Min, Max}, {pos_integer(), pos_integer()},
        begin
            Value = onion_random:time_range(Min, Min + Max),
            Min =< Value andalso (Min + Max - 1) >= Value
        end).
