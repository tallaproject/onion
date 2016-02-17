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
