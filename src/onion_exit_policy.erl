%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Exit Policy API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_exit_policy).

%% API.
-export([default/0,
         exit_allowed/3
        ]).

-include("onion_test.hrl").

%% @doc Returns a sensible default policy: block everything.
-spec default() -> [term()].
default() ->
    [{reject, "*:*"}].

%% @doc Check if a given Policy allows us to connect to the given Address and Port.
-spec exit_allowed(Policy, Address, Port) -> boolean()
    when
        Policy  :: term(),
        Address :: inet:ip_address(),
        Port    :: inet:port_number().
exit_allowed(_Policy, _Address, _Port) ->
    false.

-ifdef(TEST).

exit_to_default_policy_test() ->
    Policy = default(),
    [
        ?assertNot(exit_allowed(Policy, {127, 0, 0, 1}, 22)),
        ?assertNot(exit_allowed(Policy, {127, 0, 0, 1}, 80)),
        ?assertNot(exit_allowed(Policy, {127, 0, 0, 1}, 443))
    ].

-endif.
