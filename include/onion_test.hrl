%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").

property_test() ->
    {timeout, 300, ?_assert(check())}.

-endif.
