%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Filesystem API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_file).

%% API.
-export([homedir/0,
         expand_tilde/1,
         touch/2
        ]).

%% @doc Get the UNIX home directory.
-spec homedir() -> file:filename().
homedir() ->
    os:getenv("HOME", "").

%% @doc Expand "~" (tilde) in a path with the users home directory.
-spec expand_tilde(Path :: file:filename()) -> file:filename().
expand_tilde(Path) ->
    re:replace(Path, "~", homedir(), [{return, list}]).

%% @doc Create an empty file with a given UNIX mode.
-spec touch(Filename, Mode) -> ok | {error, Reason}
    when
        Filename :: file:filename(),
        Mode     :: integer(),
        Reason   :: term().
touch(Filename, Mode) when is_list(Filename), is_integer(Mode) ->
    case file:write_file(Filename, <<>>) of
        ok ->
            file:change_mode(Filename, Mode);
        {error, _} = Error ->
            Error
    end.
