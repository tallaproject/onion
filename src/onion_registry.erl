%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Process Registry API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_registry).

%% API.
-export([register/1,
         unregister/1,
         lookup/1,
         await/1,
         await/2
        ]).

%% @doc Register the current process under the given name.
-spec register(Name) -> any()
    when
        Name :: term().
register(Name) ->
    gproc:reg({n, l, Name}).

%% @doc Unregister the current process under the given name.
-spec unregister(Name) -> true
    when
        Name :: term().
unregister(Name) ->
    gproc:unreg({n, l, Name}).

%% @doc Lookup a given name and return the process identifier.
-spec lookup(Name) -> {ok, pid()} | {error, Reason}
    when
        Name   :: term(),
        Reason :: term().
lookup(Name) ->
    try
        Pid = gproc:lookup_pid({n, l, Name}),
        {ok, Pid}
    catch _:_ ->
        {error, not_found}
    end.

%% @doc Wait until a process have registered under the given name.
-spec await(Name) -> {ok, pid()} | {error, Reason}
    when
        Name    :: term(),
        Reason  :: term().
await(Name) ->
    await(Name, timer:seconds(5)).

%% @doc Wait until a process have registered under the given name.
-spec await(Name, Timeout) -> {ok, pid()} | {error, Reason}
    when
        Name    :: term(),
        Timeout :: non_neg_integer(),
        Reason  :: term().
await(Name, Timeout) ->
    try
        {Pid, undefined} = gproc:await({n, l, Name}, Timeout),
        {ok, Pid}
    catch _:_ ->
        {error, await_failed}
    end.
