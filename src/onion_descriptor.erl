%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Directory Descriptor API.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_descriptor).

%% API.
-export([render/2]).

-type nickname() :: string().
-type address()  :: inet:ip_address().
-type or_port()  :: inet:port_number().
-type dir_port() :: inet:port_number().

-type platform() :: string().

-type contact()  :: string().

-type seconds()  :: non_neg_integer().

-type bytes_per_second() :: non_neg_integer().

-type bandwidth_average()  :: bytes_per_second().
-type bandwidth_burst()    :: bytes_per_second().
-type bandwidth_observed() :: bytes_per_second().

-type exit_policy_entry() :: {allow, string()} | {reject, string()}.
-type exit_policy()       :: [exit_policy_entry()].

-type server_descriptor() :: [server_descriptor_entry()].

-type server_descriptor_entry() :: {router, nickname(), address(), or_port(), dir_port()}
                                 | {platform, platform()}
                                 | {contact, contact()}
                                 | {published, calendar:datetime()}
                                 | {fingerprint, onion_rsa:public_key()}
                                 | {uptime, seconds()}
                                 | {bandwidth, bandwidth_average(), bandwidth_burst(), bandwidth_observed()}
                                 | {onion_key, onion_rsa:public_key()}
                                 | {signing_key, onion_rsa:public_key()}
                                 | {ntor_onion_key, onion_curve25519:public_key()}
                                 | {exit_policy, exit_policy()}.

-spec render(ServerDescriptor, SecretKey) -> {ok, binary()} | {error, Reason}
    when
        ServerDescriptor :: server_descriptor(),
        SecretKey        :: onion_rsa:secret_key(),
        Reason           :: term().
render(ServerDescriptor, SecretKey) ->
    try
        Data = lists:map(fun render_server_descriptor_entry/1, ServerDescriptor),
        Descriptor = iolist_to_binary([Data, "router-signature\n"]),
        Signature = onion_rsa:private_encrypt(crypto:hash(sha, Data), SecretKey),
        {ok, iolist_to_binary([Descriptor, onion_pem:encode(signature, Signature)])}
    catch _:_ ->
        {error, invalid_server_descriptor}
    end.

%% @private
-spec render_server_descriptor_entry(ServerDescriptorEntry) -> Result
    when
        ServerDescriptorEntry :: server_descriptor_entry(),
        Result                :: [iolist()].
render_server_descriptor_entry({router, Nickname, Address, ORPort, DirPort}) ->
    [io_lib:format("router ~s ~s ~b 0 ~b~n", [Nickname, inet:ntoa(Address), ORPort, DirPort])];

render_server_descriptor_entry({platform, Platform}) ->
    [io_lib:format("platform ~s~n", [Platform])];

render_server_descriptor_entry({contact, Contact}) ->
    [io_lib:format("contact ~s~n", [Contact])];

render_server_descriptor_entry({published, Timestamp}) ->
    [io_lib:format("published ~s~n", render_timestamp(Timestamp))];

render_server_descriptor_entry({fingerprint, PublicKey}) ->
    {ok, PublicKeyDer} = onion_rsa:der_encode(PublicKey),
    [io_lib:format("fingerprint ~s~n", [onion_binary:fingerprint(sha, PublicKeyDer)])];

render_server_descriptor_entry({uptime, Seconds}) ->
    [io_lib:format("uptime ~b~n", [Seconds])];

render_server_descriptor_entry({bandwidth, BandwidthAverage, BandwidthBurst, BandwidthObserved}) ->
    [io_lib:format("bandwidth ~b ~b ~b~n", [BandwidthAverage, BandwidthBurst, BandwidthObserved])];

render_server_descriptor_entry({onion_key, PublicKey}) ->
    {ok, PublicKeyPem} = onion_rsa:pem_encode(PublicKey),
    [io_lib:format("onion-key~n", []), PublicKeyPem];

render_server_descriptor_entry({signing_key, PublicKey}) ->
    {ok, PublicKeyPem} = onion_rsa:pem_encode(PublicKey),
    [io_lib:format("signing-key~n", []), PublicKeyPem];

render_server_descriptor_entry({ntor_onion_key, PublicKey}) ->
    [io_lib:format("ntor-onion-key ~s~n", [onion_base64:encode(PublicKey)])];

render_server_descriptor_entry({exit_policy, ExitPolicy}) ->
    [lists:map(fun render_exit_policy/1, ExitPolicy)].

%% @private
-spec render_timestamp(Timestamp) -> io_lib:chars()
    when
        Timestamp :: calendar:datetime().
render_timestamp({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b", [Year, Month, Day, Hour, Minute, Second]).

%% @private
-spec render_exit_policy(ExitPolicy) -> io_lib:chars()
    when
        ExitPolicy :: exit_policy_entry().
render_exit_policy({allow, String}) ->
    io_lib:format("accept ~s~n", String);

render_exit_policy({reject, String}) ->
    io_lib:format("reject ~s~n", [String]).
