%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Binary Utility API
%%%
%%% This module contains various utility functions that are
%%% found useful when working with binaries.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_binary).

%% API.
-export([trim/2,

         bit/2,
         bits/1,

         fingerprint/1,
         fingerprint/2
        ]).

-include("onion_test.hrl").

-spec trim(Subject, Pattern) -> Result
    when
        Subject :: binary(),
        Pattern :: binary() | [binary()],
        Result  :: binary().
trim(Subject, Pattern) when is_binary(Subject) ->
    iolist_to_binary(binary:split(Subject, Pattern, [global, trim_all])).

%% @doc Get the bit value at a given position of a binary.
-spec bit(Subject, Position) -> 0 | 1
    when
        Subject  :: binary(),
        Position :: non_neg_integer().
bit(Subject, Position) ->
    Byte = binary:at(Subject, Position div 8),
    (Byte bsr (onion_math:mod(Position, 8))) band 1.

%% @doc Show the binary representation of a given binary.
-spec bits(Subject) -> [Bit]
    when
        Subject :: binary(),
        Bit     :: 0 | 1.
bits(Subject) ->
    [Bit || <<Bit:1/integer>> <= Subject].

%% @doc Get the fingerprint of a given binary.
-spec fingerprint(Data) -> binary()
    when
        Data :: binary().
fingerprint(Data) ->
    Elements = [[integer_to_list(A, 16),
                 integer_to_list(B, 16),
                 integer_to_list(C, 16),
                 integer_to_list(D, 16)
                ] || <<A:4/integer,
                       B:4/integer,
                       C:4/integer,
                       D:4/integer>> <= Data],
    iolist_to_binary(onion_lists:intersperse(<<" ">>, Elements)).


%% @doc Apply the hash algorithm to the input and get the fingerprint.
-spec fingerprint(Hash, Data) -> binary()
    when
        Hash :: crypto:hash_algorithm(),
        Data :: binary().
fingerprint(Hash, Data) when is_atom(Hash) ->
    fingerprint(crypto:hash(Hash, Data)).

-ifdef(TEST).
trim_basic_test() ->
    [
        ?assertEqual(trim(<<>>, <<>>), <<>>),
        ?assertEqual(trim(<<"foo bar baz">>, <<" ">>), <<"foobarbaz">>),
        ?assertEqual(trim(<<"foo bar baz">>, [<<" ">>]), <<"foobarbaz">>),
        ?assertEqual(trim(<<"foo bar\nbaz">>, [<<" ">>, <<"\n">>]), <<"foobarbaz">>),
        ?assertEqual(trim(<<"  foo   bar\n \n  \nbaz   ">>, [<<" ">>, <<"\n">>]), <<"foobarbaz">>)
    ].

bit_test() ->
    [
        ?assertEqual(bit(<<255>>, 0), 1),
        ?assertEqual(bit(<<255>>, 1), 1),
        ?assertEqual(bit(<<255>>, 2), 1),
        ?assertEqual(bit(<<255>>, 7), 1),

        ?assertEqual(bit(<<0>>, 0), 0),
        ?assertEqual(bit(<<0>>, 1), 0),
        ?assertEqual(bit(<<0>>, 2), 0),
        ?assertEqual(bit(<<0>>, 7), 0)
    ].

bits_test() ->
    [
        ?assertEqual(bits(<<0>>), [0, 0, 0, 0, 0, 0, 0, 0]),
        ?assertEqual(bits(<<0, 0>>), [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]),
        ?assertEqual(bits(<<255, 3>>), [1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1])
    ].

fingerprint_basic_test() ->
    [
        ?assertEqual(fingerprint(<<>>), <<>>),
        ?assertEqual(fingerprint(<<"foobar">>), <<"666F 6F62 6172">>),
        ?assertEqual(fingerprint(<<255, 255>>), <<"FFFF">>),
        ?assertEqual(fingerprint(<<0, 0>>), <<"0000">>),
        ?assertEqual(fingerprint(<<0, 255, 0, 255>>), <<"00FF 00FF">>)
    ].

fingerprint_hash_basic_test() ->
    [
        ?assertEqual(fingerprint(sha, <<>>), <<"DA39 A3EE 5E6B 4B0D 3255 BFEF 9560 1890 AFD8 0709">>),
        ?assertEqual(fingerprint(sha, <<"foobar">>), <<"8843 D7F9 2416 211D E9EB B963 FF4C E281 2593 2878">>),
        ?assertEqual(fingerprint(sha256, <<>>), <<"E3B0 C442 98FC 1C14 9AFB F4C8 996F B924 27AE 41E4 649B 934C A495 991B 7852 B855">>),
        ?assertEqual(fingerprint(sha256, <<"foobar">>), <<"C3AB 8FF1 3720 E8AD 9047 DD39 466B 3C89 74E5 92C2 FA38 3D4A 3960 714C AEF0 C4F2">>)
    ].
-endif.
