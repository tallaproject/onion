%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc SSL Session API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_ssl_session).

%% API.
-export([from_server_socket/2,
         from_client_socket/1,

         master_secret/1,
         client_random/1,
         server_random/1,
         certificate/1,
         cipher_suite/1,
         protocol/1
        ]).

%% Types.
-export_type([t/0]).

-type t() :: #{ atom() => binary() }.

%% @doc Create an SSL session instance from a given server socket and certificate.
-spec from_server_socket(Socket, Certificate) -> t()
    when
        Socket      :: ssl:socket(),
        Certificate :: public_key:der_encoded().
from_server_socket(Socket, Certificate) ->
    {ok, ConnectionInformation} = ssl:connection_information(Socket, [protocol, cipher_suite]),
    #{
        master_secret => onion_ssl:master_secret(Socket),
        client_random => onion_ssl:client_random(Socket),
        server_random => onion_ssl:server_random(Socket),
        certificate   => Certificate,
        cipher_suite  => proplists:get_value(cipher_suite, ConnectionInformation),
        protocol      => proplists:get_value(protocol, ConnectionInformation)
     }.

%% @doc Creatte an SSL session instance from a given client socket.
-spec from_client_socket(Socket) -> t()
    when
        Socket :: ssl:socket().
from_client_socket(Socket) ->
    {ok, Certificate} = ssl:peercert(Socket),
    {ok, ConnectionInformation} = ssl:connection_information(Socket, [protocol, cipher_suite]),
    #{
        master_secret => onion_ssl:master_secret(Socket),
        client_random => onion_ssl:client_random(Socket),
        server_random => onion_ssl:server_random(Socket),
        certificate   => Certificate,
        cipher_suite  => proplists:get_value(cipher_suite, ConnectionInformation),
        protocol      => proplists:get_value(protocol, ConnectionInformation)
     }.

%% @doc Get the master secret value.
-spec master_secret(T) -> binary()
    when
        T :: t().
master_secret(#{ master_secret := MasterSecret }) ->
    MasterSecret.

%% @doc Get the client random value.
-spec client_random(T) -> binary()
    when
        T :: t().
client_random(#{ client_random := ClientRandom }) ->
    ClientRandom.

%% @doc Get the server random value.
-spec server_random(T) -> binary()
    when
        T :: t().
server_random(#{ server_random := ServerRandom }) ->
    ServerRandom.

%% @doc Get the DER encoded certificate.
-spec certificate(T) -> public_key:der_encoded()
    when
        T :: t().
certificate(#{ certificate := Certificate }) ->
    Certificate.

%% @doc Get the list of SSL ciphers.
-spec cipher_suite(T) -> ssl:ciphers()
    when
        T :: t().
cipher_suite(#{ cipher_suite := CipherSuite }) ->
    CipherSuite.

%% @doc Get the SSL protocol.
-spec protocol(T) -> ssl:protocol()
    when
        T :: t().
protocol(#{ protocol := Protocol }) ->
    Protocol.
