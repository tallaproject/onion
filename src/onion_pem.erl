%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc PEM API.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_pem).

%% API.
-export([encode/2,
         encode/3
        ]).

-spec encode(Type, Data) -> Result
    when
        Type   :: atom() | binary() | string(),
        Data   :: binary(),
        Result :: binary().
encode(Type, Data) ->
    encode(Type, Data, true).

-spec encode(Type, Data, Split) -> Result
    when
        Type   :: atom() | binary() | string(),
        Data   :: binary(),
        Split  :: boolean(),
        Result :: binary().
encode(Type, Data, Split) when is_atom(Type) ->
    encode(atom_to_list(Type), Data, Split);

encode(Type, Data, Split) when is_binary(Type) ->
    encode(binary_to_list(Type), Data, Split);

encode(Type, Data, Split) when is_list(Type) ->
    TypeString = string:to_upper(Type),
    iolist_to_binary([<<"-----BEGIN ">>, TypeString, <<"-----\n">>,
                      case Split of
                          true ->
                              base64_encode_and_split(Data);

                          false ->
                              onion_base64:encode(Data)
                      end,
                      <<"\n-----END ">>, TypeString, <<"-----\n">>]).
%% @private
-spec base64_encode_and_split(Data) -> iolist()
    when
        Data :: binary().
base64_encode_and_split(Data) ->
    split_lines(base64:encode(Data)).

split_lines(<<Data:64/binary>>) ->
    [Data];

split_lines(<<Data:64/binary, Rest/binary>>) ->
    [Data, $\n | split_lines(Rest)];

split_lines(Data) ->
    [Data].
