%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%

-ifdef(TEST).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-import(onion_test, [base16_encode/1,
                     base16_decode/1,

                     base32_encode/1,
                     base32_decode/1,

                     base64_encode/1,
                     base64_decode/1
                    ]).

-endif.
