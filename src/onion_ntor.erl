%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc NTor Handshake
%%%
%%% For information about the NTor handshake, see tor-spec.txt, section 5.1.4.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_ntor).

%% API.
-export([create/2,
         server_handshake/3,
         client_handshake/4
        ]).

-define(NTOR_PROTOCOL_ID, <<"ntor-curve25519-sha256-1">>).
-define(NTOR_MAC,         <<?NTOR_PROTOCOL_ID/binary, ":mac">>).
-define(NTOR_KEY,         <<?NTOR_PROTOCOL_ID/binary, ":key_extract">>).
-define(NTOR_VERIFY,      <<?NTOR_PROTOCOL_ID/binary, ":verify">>).
-define(NTOR_EXPAND,      <<?NTOR_PROTOCOL_ID/binary, ":key_expand">>).

%% @doc Create the client data to begin an NTor handshake.
%%
%% This function returns the data that a client must send to a server to
%% initialize a NTor handshake. The function also returns a generated ephemeral
%% key pair. The ephemeral key pair must be stored to be able to handle the
%% response from the server handshake.
%%
%% @end
-spec create(ServerIDKeyHash, ServerNTorPublicKey) -> {Data, EphemeralKeyPair}
    when
        ServerIDKeyHash     :: binary(),
        ServerNTorPublicKey :: onion_x25519:public_key(),
        Data                :: binary(),
        EphemeralKeyPair    :: onion_x25519:keypair().
create(ServerIDKeyHash, ServerNTorPublicKey) ->
    #{ public := EphemeralPublicKey } = EphemeralKeyPair = onion_x25519:keypair(),
    {<<ServerIDKeyHash:20/binary, ServerNTorPublicKey:32/binary, EphemeralPublicKey:32/binary>>, EphemeralKeyPair}.

%% @doc Compute shared secret and client response from initial handshake message from the client.
%%
%% This function computes the shared secret (KeySeed) and the message that must
%% be send to the client for the client to finalize the handshake.
%%
%% @end
-spec server_handshake(ServerIDKeyHash, ServerNTorOnionKeyPair, ClientEphemeralPublicKey) -> {Data, KeySeed}
    when
        ServerIDKeyHash          :: binary(),
        ServerNTorOnionKeyPair   :: onion_x25519:keypair(),
        ClientEphemeralPublicKey :: onion_x25519:public_key(),
        Data                     :: binary(),
        KeySeed                  :: binary().
server_handshake(ServerIDKeyHash, ServerNTorOnionKeyPair, ClientEphemeralPublicKey) ->
    #{ public := ServerEphemeralPublicKey,
       secret := ServerEphemeralSecretKey } = onion_x25519:keypair(),

    #{ public := ServerNTorOnionPublicKey,
       secret := ServerNTorOnionSecretKey } = ServerNTorOnionKeyPair,

    %% SecretInput = shared(y, X) | shared(b, X) | ID | B | X | Y | PROTOID
    %%     where X = ClientEphemeralPublicKey
    %%           y = ServerEphemeralSecretKey
    %%           Y = ServerEphemeralPublicKey
    %%           b = ServerNTorOnionSecretKey
    %%           B = ServerNTorOnionPublicKey
    SharedA = onion_x25519:shared_secret(ServerEphemeralSecretKey, ClientEphemeralPublicKey),
    SharedB = onion_x25519:shared_secret(ServerNTorOnionSecretKey, ClientEphemeralPublicKey),

    SecretInput = <<SharedA/binary,
                    SharedB/binary,
                    ServerIDKeyHash:20/binary,
                    ServerNTorOnionPublicKey:32/binary,
                    ClientEphemeralPublicKey:32/binary,
                    ServerEphemeralPublicKey:32/binary,
                    ?NTOR_PROTOCOL_ID/binary>>,
    KeySeed   = hmac(?NTOR_KEY, SecretInput),
    Verify    = hmac(?NTOR_VERIFY, SecretInput),
    AuthInput = <<Verify/binary,
                  ServerIDKeyHash:20/binary,
                  ServerNTorOnionPublicKey:32/binary,
                  ServerEphemeralPublicKey:32/binary,
                  ClientEphemeralPublicKey:32/binary,
                  ?NTOR_PROTOCOL_ID/binary,
                  "Server">>,
    AuthInputData = hmac(?NTOR_MAC, AuthInput),
    {<<ServerEphemeralPublicKey/binary, AuthInputData/binary>>, KeySeed}.


%% @doc Compute the shared secret.
%%
%% This function computes the shared secret (KeySeed) from the server's
%% handshake response. A client should verify that the Auth matches the
%% AuthData from the message that was send from the server to finalize the
%% handshake.
%%
%% @end
-spec client_handshake(ServerIDKeyHash, ServerNTorOnionPublicKey, ServerEphemeralPublicKey, ClientEphemeralKeyPair) -> {Auth, KeySeed}
    when
        ServerIDKeyHash :: binary(),
        ServerNTorOnionPublicKey :: onion_x25519:public_key(),
        ServerEphemeralPublicKey :: onion_x25519:public_key(),
        ClientEphemeralKeyPair   :: onion_x25519:keypair(),
        Auth                     :: binary(),
        KeySeed                  :: binary().
client_handshake(ServerIDKeyHash, ServerNTorOnionPublicKey, ServerEphemeralPublicKey, ClientEphemeralKeyPair) ->
    #{ public := ClientEphemeralPublicKey,
       secret := ClientEphemeralSecretKey } = ClientEphemeralKeyPair,

    %% ServerInput = shared(x, Y) | shared(x, B) | ID | B | X | Y | PROTOID
    %%     where x = ClientEphemeralSecretKey
    %%           X = ClientEphemeralPublicKey
    %%           Y = ServerEphemeralPublicKey
    %%           b = ServerNTorOnionSecretKey
    %%           B = ServerNTorOnionPublicKey
    SharedA = onion_x25519:shared_secret(ClientEphemeralSecretKey, ServerEphemeralPublicKey),
    SharedB = onion_x25519:shared_secret(ClientEphemeralSecretKey, ServerNTorOnionPublicKey),

    SecretInput = <<SharedA/binary,
                    SharedB/binary,
                    ServerIDKeyHash:20/binary,
                    ServerNTorOnionPublicKey:32/binary,
                    ClientEphemeralPublicKey:32/binary,
                    ServerEphemeralPublicKey:32/binary,
                    ?NTOR_PROTOCOL_ID/binary>>,
    KeySeed     = hmac(?NTOR_KEY, SecretInput),
    Verify      = hmac(?NTOR_VERIFY, SecretInput),
    AuthInput   = <<Verify/binary,
                    ServerIDKeyHash:20/binary,
                    ServerNTorOnionPublicKey:32/binary,
                    ServerEphemeralPublicKey:32/binary,
                    ClientEphemeralPublicKey:32/binary,
                    ?NTOR_PROTOCOL_ID/binary,
                    "Server">>,
    AuthInputData = hmac(?NTOR_MAC, AuthInput),
    {AuthInputData, KeySeed}.

%% @private
-spec hmac(Key, Data) -> binary()
    when
        Key  :: iodata(),
        Data :: iodata().
hmac(Key, Data) ->
    crypto:hmac(sha256, Key, Data).
