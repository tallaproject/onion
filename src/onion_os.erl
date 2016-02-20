%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Operating System API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_os).

%% API.
-export([name/0]).

%% @doc Get the name of the running operating system.
-spec name() -> string().
name() ->
    {_, NameAtom} = os:type(),
    case NameAtom of
        darwin ->
            "Darwin";

        freebsd ->
            "FreeBSD";

        linux ->
            "Linux";

        sunos ->
            "Solaris";

        NameAtom ->
            atom_to_list(NameAtom)
    end.
