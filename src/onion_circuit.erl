%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Circuit Utility Functions
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_circuit).

%% API.
-export([id/2]).

%% Types.
-export_type([id/0
             ]).

-type id() :: 0 .. 4294967295.

%% @doc Create a new randomly chosen Circuit ID
-spec id(ProtocolVersion, HighBit) -> {ok, id()} | {error, Reason}
    when
        ProtocolVersion :: onion_protocol:version(),
        HighBit         :: boolean(),
        Reason          :: term().
id(ProtocolVersion, HighBit) ->
    random_id(ProtocolVersion, HighBit).

%% @private
-spec random_id(ProtocolVersion, HighBit) -> {ok, id()} | {error, Reason}
    when
        ProtocolVersion :: onion_protocol:version(),
        HighBit         :: boolean(),
        Reason          :: term().
random_id(4, HighBit) ->
    ID = random_positive_integer(4, HighBit),
    {ok, ID};

random_id(3, HighBit) ->
    ID = random_positive_integer(2, HighBit),
    {ok, ID};

random_id(_ProtocolVersion, _HighBit) ->
    {error, unknown_protocol}.

%% @private
-spec random_positive_integer(Bytes, HighBit) -> pos_integer()
    when
        Bytes   :: non_neg_integer(),
        HighBit :: boolean().
random_positive_integer(Bytes, HighBit) ->
    Bits    = 1 bsl (Bytes * 8 - 1),
    BitMask = Bits - 1,
    case onion_random:bytes_unsigned(Bytes) band BitMask of
        0 ->
            random_positive_integer(Bytes, HighBit);

        N ->
            case HighBit of
                true  -> N bor Bits;
                false -> N
            end
    end.
