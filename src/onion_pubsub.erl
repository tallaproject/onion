%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Pub/Sub API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_pubsub).

%% API.
-export([subscribe/1,
         send/2
        ]).

%% @doc Subscribe to a given topic.
-spec subscribe(Topic) -> true
    when
        Topic :: term().
subscribe(Topic) ->
    gproc:reg({p, l, {?MODULE, Topic}}).

%% @doc Send a message to a given topic.
-spec send(Topic, Message) -> {pid(), Topic, Message}
    when
        Topic   :: term(),
        Message :: term().
send(Topic, Message) ->
    gproc:send({p, l, {?MODULE, Topic}}, {self(), Topic, Message}).
