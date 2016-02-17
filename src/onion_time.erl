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
-export([unix_epoch/0,
         from_unix_epoch/1,
         to_unix_epoch/1
        ]).

-include("onion_test.hrl").

%% calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).
-define(UNIX_EPOCH, 62167219200).

-spec unix_epoch() -> non_neg_integer().
unix_epoch() ->
    to_unix_epoch(calendar:now_to_datetime(erlang:timestamp())).

-spec from_unix_epoch(Timestamp :: non_neg_integer()) -> calendar:datetime().
from_unix_epoch(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(?UNIX_EPOCH + Timestamp).

-spec to_unix_epoch(Now :: calendar:datetime()) -> non_neg_integer().
to_unix_epoch(Now) ->
    calendar:datetime_to_gregorian_seconds(Now) - ?UNIX_EPOCH.
