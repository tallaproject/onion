%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Property Tests for onion_circuit.
%%% @end
%%% -----------------------------------------------------------
-module(prop_circuit).

%% Properties.
-export([prop_high_bit/0]).

-include_lib("onion/include/onion_test.hrl").

-spec prop_high_bit() -> term().
prop_high_bit() ->
    ?FORALL({Protocol, HighBit}, {protocol_version(), high_bit()},
        begin
            Bit = case HighBit of
                      true  -> 1;
                      false -> 0
                  end,
            ProtocolLength = case Protocol of
                                 3 -> 16; %% Size in bits.
                                 4 -> 32  %% Size in bits.
                             end,
            {ok, CircuitID} = onion_circuit:id(Protocol, HighBit),
            CircuitIDBin    = <<CircuitID:ProtocolLength/integer>>,

            case CircuitIDBin of
                <<Bit:1/integer, _:31/integer>> -> true;
                <<Bit:1/integer, _:15/integer>> -> true;
                X                               -> false
            end
        end).

%% @private
protocol_version() ->
    oneof([3, 4]).

%% @private
high_bit() ->
    boolean().
