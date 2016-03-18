%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Time Utility API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_time).

%% API.
-export([epoch/0,
         from_epoch/1,
         to_epoch/1
        ]).

-include("onion_test.hrl").

%% calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).
-define(EPOCH, 62167219200).

%% @doc Get current UNIX epoch.
-spec epoch() -> non_neg_integer().
epoch() ->
    to_epoch(calendar:now_to_datetime(erlang:timestamp())).

%% @doc Convert UNIX epoch to calendar:datetime().
-spec from_epoch(Timestamp :: non_neg_integer()) -> calendar:datetime().
from_epoch(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(?EPOCH + Timestamp).

%% @doc Convert calendar:datetime() to UNIX epoch.
-spec to_epoch(Now :: calendar:datetime()) -> non_neg_integer().
to_epoch(Now) ->
    calendar:datetime_to_gregorian_seconds(Now) - ?EPOCH.
