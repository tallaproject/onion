%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Common Test suite for onion_ed25519.
%%% @end
%%% -----------------------------------------------------------
-module(ed25519_SUITE).

%% Test Cases.
-export([nacl_test_vectors/1]).

%% Common Test API
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, basic_group}].

groups() ->
    [{basic_group, [], [nacl_test_vectors]}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(onion),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(onion),
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

nacl_test_vectors(Config) ->
    DataDir = ?config(data_dir, Config),
    File = filename:join([DataDir, "sign.input"]),
    {ok, Data} = file:read_file(File),
    TestVectors = binary:split(Data, <<"\n">>, [global, trim]),
    check_test_vectors(TestVectors).

check_test_vectors([]) ->
    ok;

check_test_vectors([Line | Rest]) ->
    [A, B, C, D] = binary:split(Line, <<":">>, [global, trim]),

    {ok, <<SecretKey:64/binary>>} = onion_base16:decode(A),
    {ok, <<PublicKey:32/binary>>} = onion_base16:decode(B),

    {ok, Message}   = onion_base16:decode(C),

    {ok, <<Signature:64/binary, Message/binary>>} = onion_base16:decode(D),

    true = onion_ed25519:verify(Signature, Message, PublicKey),
    Signature = onion_ed25519:sign(Message, SecretKey),

    check_test_vectors(Rest).
