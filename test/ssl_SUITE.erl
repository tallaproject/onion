%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Common Test suite for onion_ssl.
%%% @end
%%% -----------------------------------------------------------
-module(ssl_SUITE).

%% Test Cases.
-export([read_values/1]).

%% Common Test API
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

-include_lib("onion/include/onion_test.hrl").

all() ->
    [{group, basic_group}].

groups() ->
    [{basic_group, [], [read_values]}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(onion),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(onion),
    ok.

init_per_testcase(_Case, Config) ->
    {ok, #{ secret := SecretKey, public := PublicKey }} = onion_rsa:keypair(1024),
    {ok, SecretKeyDER} = onion_rsa:der_encode(SecretKey),
    {ok, Cert} = onion_x509:create_certificate(#{
                    public_key => PublicKey,
                    valid_from => onion_time:from_unix_epoch(0),
                    valid_to   => onion_time:from_unix_epoch(0),
                    subject    => [{name, "example.org"}],
                    issuer     => [{name, "example.org"}]
                 }),
    CertDER = onion_x509:sign(Cert, SecretKey),
    Self = self(),
    {ok, ListenSocket} = ssl:listen(30000, [{reuseaddr, true}, {key, {'RSAPrivateKey', SecretKeyDER}}, {cert, CertDER}]),
    Listener = spawn_link(fun Fun() ->
                              {ok, Socket} = ssl:transport_accept(ListenSocket),
                              ok = ssl:ssl_accept(Socket),
                              Self ! {ssl_socket, self(), Socket},
                              spawn_link(fun() ->
                                             socket_loop(Socket)
                                         end),
                              Fun()
                          end),
    [{listener, Listener} | Config].

end_per_testcase(_Case, _Config) ->

    ok.

read_values(Config) ->
    Listener = ?config(listener, Config),
    {ok, ClientSocket} = ssl:connect("127.0.0.1", 30000, [], 500),
    ServerSocket = receive
                       {ssl_socket, Listener, Sock} ->
                           Sock
                   after 500 ->
                       ct:fail(missing_server_socket)
                   end,

    MasterSecretClient = onion_ssl:master_secret(ClientSocket),
    ClientRandomClient = onion_ssl:client_random(ClientSocket),
    ServerRandomClient = onion_ssl:server_random(ClientSocket),

    MasterSecretServer = onion_ssl:master_secret(ServerSocket),
    ClientRandomServer = onion_ssl:client_random(ServerSocket),
    ServerRandomServer = onion_ssl:server_random(ServerSocket),

    ct:pal("Master Secret:~n  ~s~n  ~s", [onion_test:base64_encode(MasterSecretClient),
                                          onion_test:base64_encode(MasterSecretServer)]),
    ct:pal("Client Random:~n  ~s~n  ~s", [onion_test:base64_encode(ClientRandomClient),
                                          onion_test:base64_encode(ClientRandomServer)]),
    ct:pal("Server Random:~n  ~s~n  ~s", [onion_test:base64_encode(ServerRandomClient),
                                          onion_test:base64_encode(ServerRandomServer)]),

    MasterSecretClient = MasterSecretServer,
    ClientRandomClient = ClientRandomServer,
    ServerRandomClient = ServerRandomServer,

    ok.

socket_loop(Socket) ->
    receive
        X ->
            X
    after 500 ->
        timeout
    end,
    socket_loop(Socket).
