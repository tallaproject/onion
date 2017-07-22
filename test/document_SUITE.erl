%%%
%%% Copyright (c) 2017 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Lasse Grinderslev Andersen <lasse@etableret.dk>
%%% @doc Tests for Tor document routines.
%%% @end
%%% -----------------------------------------------------------
-module(document_SUITE).


%% API
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([
         parse_consensus/1,
         cached_consensus/1,
         cached_key_certificates/1,
         cached_consensus_authority_signatures/1,
         cached_server_descriptors/1,
         testing_tors_test_server_descriptors/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROPTEST(M,F), true = proper:quickcheck(M:F())).

all() ->
    [
     {group, parser},
     {group, consensus},
     {group, server_descriptor}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
     {parser, [], [parse_consensus]},
     {consensus, [parallel], [cached_consensus,
                              cached_key_certificates,
                              cached_consensus_authority_signatures]},
     {server_descriptor, [], [testing_tors_test_server_descriptors,
                              cached_server_descriptors]}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    [{cached_consensus, from_file("cached-consensus", Config)},
     {cached_keycerts, from_file("cached-certs", Config)}
     | Config].

end_per_suite(_Config) ->
    ok.


%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
group(_Groupname) ->
    [].

init_per_group(_Groupname, Config) ->
    Config.

end_per_group(_Groupname, _Config) ->

    ok.


%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================
parse_consensus(Config) ->
    Consensus = ?config(cached_consensus, Config),
    {ok, _Items} = onion_document:decode(Consensus).

cached_consensus(Config) ->
    Consensus = ?config(cached_consensus, Config),
    {ok, _ValidatedItems} = onion_consensus:decode(Consensus).


cached_key_certificates(Config) ->
    KeyCertificates = ?config(cached_keycerts, Config),
    {ok, _ParsedItems} = onion_key_certificate:decode_all(KeyCertificates).


cached_consensus_authority_signatures(Config) ->
    KeyCertificates = ?config(cached_keycerts, Config),
    {ok, KeyCertsItems} = onion_key_certificate:decode_all(KeyCertificates),
    Consensus = ?config(cached_consensus, Config),
    {ok, ConsensusItems} = onion_consensus:decode(Consensus),
    ok = onion_consensus:verify_consensus(Consensus, ConsensusItems, KeyCertsItems).


cached_server_descriptors(Config) ->
    FileName = ?config(data_dir, Config) ++ "cached-descriptors",
    ok = parse_cached_server_descriptors(FileName).


testing_tors_test_server_descriptors(Config) ->
    DescriptorsData = from_file("failing_routerdescs.inc", Config),
    Descriptors = extract_tor_test_descriptors(DescriptorsData),
    ok = decode_tor_testsuite_descriptors(Descriptors).


%%%===================================================================
%% Utility Functions.
%%%===================================================================
parse_cached_server_descriptors(File) ->
    ct:timetrap({seconds, 120}),
    {ok, IoDevice} = file:open(File, [read]),
    parse_cached_server_descriptors(next_line(IoDevice), IoDevice, <<"">>).


parse_cached_server_descriptors({ok, <<"@downloaded-at 20", _/binary>>}, IoDevice, Descriptor) ->
    parse_cached_server_descriptors(next_line(IoDevice), IoDevice, Descriptor);

parse_cached_server_descriptors({ok, <<"@source \"", _/binary>>}, IoDevice, <<"">>) ->
    parse_cached_server_descriptors(next_line(IoDevice), IoDevice, <<"">>);

parse_cached_server_descriptors({ok, <<"@source \"", _/binary>>}, IoDevice, Descriptor) ->
    {ok, _ValidatedItems} = onion_server_descriptor:decode(Descriptor),
    parse_cached_server_descriptors(next_line(IoDevice), IoDevice, <<"">>);

parse_cached_server_descriptors({ok, Line}, IoDevice, Descriptor) ->
    Descriptor2 = <<Descriptor/binary, Line/binary>>,
    parse_cached_server_descriptors(next_line(IoDevice), IoDevice, Descriptor2);

parse_cached_server_descriptors(eof, _, _) ->
    ok.


next_line(IoDevice) ->
    case file:read_line(IoDevice) of
        {ok, Line} ->
            {ok, erlang:list_to_binary(Line)};

             eof ->
                eof
    end.


decode_tor_testsuite_descriptors([{'EX_RI_MINIMAL', _} = ValidDescriptor | Rest]) ->
    test_valid_tor_descriptor(ValidDescriptor),
    decode_tor_testsuite_descriptors(Rest);

decode_tor_testsuite_descriptors([{'EX_RI_MAXIMAL', _} = ValidDescriptor | Rest]) ->
    test_valid_tor_descriptor(ValidDescriptor),
    decode_tor_testsuite_descriptors(Rest);

decode_tor_testsuite_descriptors([{'EX_RI_MINIMAL_ED', _} = ValidDescriptor | Rest]) ->
    test_valid_tor_descriptor(ValidDescriptor),
    decode_tor_testsuite_descriptors(Rest);

decode_tor_testsuite_descriptors([{'EX_RI_BAD_HAS_ACCEPT6', _} | Rest]) ->
    %% This invalid descriptor is not going to be used since it actually violates the spec.
    decode_tor_testsuite_descriptors(Rest);

decode_tor_testsuite_descriptors([{Name, _} = InvalidDescriptor | Rest]) ->
    io:format("Testing tor descriptor ~p~n", [Name]),
    ?assertError(_Error, {ok, _} = onion_server_descriptor:decode(InvalidDescriptor)),
    decode_tor_testsuite_descriptors(Rest);

decode_tor_testsuite_descriptors([]) ->
    ok.


test_valid_tor_descriptor({Name, Descriptor}) ->
    io:format("Testing tor descriptor ~p~n", [Name]),
    {ok, _ValidatedItems} = onion_server_descriptor:decode(Descriptor).



extract_tor_test_descriptors(TestSuiteBin) ->
    DescriptorsRaw = binary:split(TestSuiteBin, <<";\n">>, [global, trim_all]),
    lists:map(fun extract_descriptors/1, DescriptorsRaw).

extract_descriptors(DescriptorRaw) ->
    DescriptorLines = binary:split(DescriptorRaw, <<"\n">>, [global, trim_all]),
    decode_descriptor(DescriptorLines, <<"">>, name_here).


decode_descriptor([<<"static const char ", Rest/binary>> | RestLines], ParsedPart, _) ->
    Len = size(Rest) - 4,
    <<DescriptorName:Len/binary, _/binary>> = Rest,
    Name = atom_to_list(erlang:binary_to_atom(DescriptorName, utf8)),
    decode_descriptor(RestLines, ParsedPart, Name);

decode_descriptor([<<"  \"", Rest/binary>> | RestLines], ParsedPart, Name) ->
    Len = size(Rest) - 1,
    <<DescriptorLine:Len/binary, _/binary>> = Rest,
    decode_descriptor(RestLines, <<ParsedPart/binary, DescriptorLine/binary>>, Name);

decode_descriptor([_ | RestLines], ParsedPart, Name) ->
    decode_descriptor(RestLines, ParsedPart, Name);

decode_descriptor([], ParsedPart, Name) ->
    {Name, binary:replace(ParsedPart, <<"\\n">>, <<"\n">>, [global])}.


from_file(FileName, Config) ->
    File = ?config(data_dir, Config) ++ "/" ++ FileName,
    {ok, DataBin} = file:read_file(File),
    DataBin.
