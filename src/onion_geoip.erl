%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc GeoIP API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_geoip).

%% API.
-export([parse_ipv4_file/1,
         parse_ipv6_file/1
        ]).

%% @doc Parse a GeoIP file containing IPv4 addresses.
-spec parse_ipv4_file(Filename) -> {ok, [{inet:ip4_address(), inet:ip4_address(), binary()}]} | {error, Reason}
    when
        Filename :: file:filename(),
        Reason   :: term().
parse_ipv4_file(Filename) ->
    parse_file(Filename, fun parse_ipv4_line/1).

%% @doc Parse a GeoIP file containing IPv6 addresses.
-spec parse_ipv6_file(Filename) -> {ok, [{inet:ip6_address(), inet:ip6_address(), binary()}]} | {error, Reason}
    when
        Filename :: file:filename(),
        Reason   :: term().
parse_ipv6_file(Filename) ->
    parse_file(Filename, fun parse_ipv6_line/1).

%% @private
parse_file(Filename, LineParser) ->
    case file:read_file(Filename) of
        {ok, FileContent} ->
            {ok, parse_lines(binary:split(FileContent, <<"\n">>, [global, trim]), [], LineParser)};

        {error, _} = Error ->
            Error
    end.

%% @private
parse_lines([], Result, _FileParser) ->
    lists:reverse(Result);

parse_lines([<<"#", _/binary>> | Rest], Result, FileParser) ->
    parse_lines(Rest, Result, FileParser);

parse_lines([Line | Rest], Result, FileParser) ->
    Tokens = binary:split(Line, <<",">>, [global, trim]),
    parse_lines(Rest, [FileParser(Tokens) | Result], FileParser).

%% @private
parse_ipv4_line([Start, End, Country]) ->
    StartInteger = binary_to_integer(Start),
    EndInteger   = binary_to_integer(End),
    StartAddress = {(StartInteger bsr 24) band 16#ff, (StartInteger bsr 16) band 16#ff, (StartInteger bsr 8) band 16#ff, StartInteger band 16#ff},
    EndAddress   = {(EndInteger bsr 24) band 16#ff, (EndInteger bsr 16) band 16#ff, (EndInteger bsr 8) band 16#ff, EndInteger band 16#ff},
    {{StartAddress, EndAddress}, Country}.

%% @private
parse_ipv6_line([Start, End, Country]) ->
    {ok, StartAddress} = inet:parse_ipv6strict_address(binary_to_list(Start)),
    {ok, EndAddress}   = inet:parse_ipv6strict_address(binary_to_list(End)),
    {{StartAddress, EndAddress}, Country}.
