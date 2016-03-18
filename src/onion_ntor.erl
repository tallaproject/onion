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
         create/3,

         server_handshake/4,
         server_handshake/5,

         client_handshake/5
        ]).

-include("onion_test.hrl").

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
    create(ServerIDKeyHash, ServerNTorPublicKey, onion_x25519:keypair()).

%% @doc Create the client data to begin an NTor handshake.
%%
%% This function returns the data that a client must send to a server to
%% initialize a NTor handshake. The function also returns a generated ephemeral
%% key pair. The ephemeral key pair must be stored to be able to handle the
%% response from the server handshake.
%%
%% This function takes the ClientEphemeralKeyPair as an argument, which allows
%% us to test the function in a deterministic way.
%%
%% @end
-spec create(ServerIDKeyHash, ServerNTorPublicKey, ClientEphemeralKeyPair) -> {Data, ClientEphemeralKeyPair}
    when
        ServerIDKeyHash        :: binary(),
        ServerNTorPublicKey    :: onion_x25519:public_key(),
        ClientEphemeralKeyPair :: onion_x25519:keypair(),
        Data                   :: binary().
create(ServerIDKeyHash, ServerNTorPublicKey, ClientEphemeralKeyPair) ->
    #{ public := EphemeralPublicKey } = ClientEphemeralKeyPair,
    {<<ServerIDKeyHash:20/binary, ServerNTorPublicKey:32/binary, EphemeralPublicKey:32/binary>>, ClientEphemeralKeyPair}.

%% @doc Compute shared secret and client response from initial handshake message from the client.
%%
%% This function computes the shared secret and the message that must be send
%% to the client for the client to finalize the handshake.
%%
%% @end
-spec server_handshake(ServerIDKeyHash, ServerNTorOnionKeyPair, ClientEphemeralPublicKey, Length) -> {Data, Key}
    when
        ServerIDKeyHash          :: binary(),
        ServerNTorOnionKeyPair   :: onion_x25519:keypair(),
        ClientEphemeralPublicKey :: onion_x25519:public_key(),
        Length                   :: non_neg_integer(),
        Data                     :: binary(),
        Key                      :: binary().
server_handshake(ServerIDKeyHash, ServerNTorOnionKeyPair, ClientEphemeralPublicKey, Length) ->
    server_handshake(ServerIDKeyHash, ServerNTorOnionKeyPair, ClientEphemeralPublicKey, Length, onion_x25519:keypair()).

%% @doc Compute shared secret and client response from initial handshake message from the client.
%%
%% This function computes the shared secret and the message that must be send
%% to the client for the client to finalize the handshake.
%%
%% This function takes the ServerEphemeralKeyPair as an argument, which allows
%% us to test the function in a deterministic way.
%%
%% @end
-spec server_handshake(ServerIDKeyHash, ServerNTorOnionKeyPair, ClientEphemeralPublicKey, Length, ServerEphemeralKeyPair) -> {Response, SharedSecret}
    when
        ServerIDKeyHash          :: binary(),
        ServerNTorOnionKeyPair   :: onion_x25519:keypair(),
        ClientEphemeralPublicKey :: onion_x25519:public_key(),
        ServerEphemeralKeyPair   :: onion_x25519:keypair(),
        Length                   :: non_neg_integer(),
        Response                 :: binary(),
        SharedSecret             :: binary().
server_handshake(ServerIDKeyHash, ServerNTorOnionKeyPair, ClientEphemeralPublicKey, Length, ServerEphemeralKeyPair) ->
    #{ public := ServerEphemeralPublicKey, secret := ServerEphemeralSecretKey } = ServerEphemeralKeyPair,
    #{ public := ServerNTorOnionPublicKey, secret := ServerNTorOnionSecretKey } = ServerNTorOnionKeyPair,

    A = onion_x25519:shared_secret(ServerEphemeralSecretKey, ClientEphemeralPublicKey),
    B = onion_x25519:shared_secret(ServerNTorOnionSecretKey, ClientEphemeralPublicKey),

    SecretInput   = secret_input(A, B, ServerIDKeyHash, ServerNTorOnionPublicKey, ClientEphemeralPublicKey, ServerEphemeralPublicKey),
    Key           = onion_kdf:hkdf(SecretInput, ?NTOR_KEY, ?NTOR_EXPAND, Length),
    Verify        = hmac_verify(SecretInput),
    AuthInput     = auth_input(Verify, ServerIDKeyHash, ServerNTorOnionPublicKey, ClientEphemeralPublicKey, ServerEphemeralPublicKey),
    AuthInputData = hmac_mac(AuthInput),
    {<<ServerEphemeralPublicKey:32/binary, AuthInputData:32/binary>>, Key}.

%% @doc Compute the shared secret.
%%
%% This function computes the shared secret from the server's handshake
%% response. A client should verify that the Auth matches the AuthData from the
%% message that was send from the server to finalize the handshake.
%%
%% @end
-spec client_handshake(ServerIDKeyHash, ServerNTorOnionPublicKey, ServerEphemeralPublicKey, ClientEphemeralKeyPair, Length) -> {Auth, SharedSecret}
    when
        ServerIDKeyHash :: binary(),
        ServerNTorOnionPublicKey :: onion_x25519:public_key(),
        ServerEphemeralPublicKey :: onion_x25519:public_key(),
        ClientEphemeralKeyPair   :: onion_x25519:keypair(),
        Length                   :: non_neg_integer(),
        Auth                     :: binary(),
        SharedSecret             :: binary().
client_handshake(ServerIDKeyHash, ServerNTorOnionPublicKey, ServerEphemeralPublicKey, ClientEphemeralKeyPair, Length) ->
    #{ public := ClientEphemeralPublicKey, secret := ClientEphemeralSecretKey } = ClientEphemeralKeyPair,

    A = onion_x25519:shared_secret(ClientEphemeralSecretKey, ServerEphemeralPublicKey),
    B = onion_x25519:shared_secret(ClientEphemeralSecretKey, ServerNTorOnionPublicKey),

    SecretInput = secret_input(A, B, ServerIDKeyHash, ServerNTorOnionPublicKey, ClientEphemeralPublicKey, ServerEphemeralPublicKey),
    Key         = onion_kdf:hkdf(SecretInput, ?NTOR_KEY, ?NTOR_EXPAND, Length),
    Verify      = hmac_verify(SecretInput),
    AuthInput   = auth_input(Verify, ServerIDKeyHash, ServerNTorOnionPublicKey, ClientEphemeralPublicKey, ServerEphemeralPublicKey),
    {hmac_mac(AuthInput), Key}.

%% @private
-spec auth_input(Verify, ServerIDKeyHash, ServerNTorOnionPublicKey, ClientEphemeralPublicKey, ServerEphemeralPublicKey) -> binary()
    when
        Verify                   :: binary(),
        ServerIDKeyHash          :: binary(),
        ServerNTorOnionPublicKey :: onion_x25519:public_key(),
        ClientEphemeralPublicKey :: onion_x25519:public_key(),
        ServerEphemeralPublicKey :: onion_x25519:public_key().
auth_input(Verify, ServerIDKeyHash, ServerNTorOnionPublicKey, ClientEphemeralPublicKey, ServerEphemeralPublicKey) ->
    <<Verify:32/binary,
      ServerIDKeyHash:20/binary,
      ServerNTorOnionPublicKey:32/binary,
      ServerEphemeralPublicKey:32/binary,
      ClientEphemeralPublicKey:32/binary,
      ?NTOR_PROTOCOL_ID/binary,
      "Server">>.

%% @private
-spec secret_input(A, B, ServerIDKeyHash, ServerNTorOnionPublicKey, ClientEphemeralPublicKey, ServerEphemeralPublicKey) -> binary()
    when
        A                        :: binary(),
        B                        :: binary(),
        ServerIDKeyHash          :: binary(),
        ServerNTorOnionPublicKey :: onion_x25519:public_key(),
        ClientEphemeralPublicKey :: onion_x25519:public_key(),
        ServerEphemeralPublicKey :: onion_x25519:public_key().
secret_input(A, B, ServerIDKeyHash, ServerNTorOnionPublicKey, ClientEphemeralPublicKey, ServerEphemeralPublicKey) ->
    <<A:32/binary,
      B:32/binary,
      ServerIDKeyHash:20/binary,
      ServerNTorOnionPublicKey:32/binary,
      ClientEphemeralPublicKey:32/binary,
      ServerEphemeralPublicKey:32/binary,
      ?NTOR_PROTOCOL_ID/binary>>.

%% @private
-spec hmac_mac(Data) -> binary()
    when
        Data :: iodata().
hmac_mac(Data) ->
    hmac(?NTOR_MAC, Data).

%% @private
-spec hmac_verify(Data) -> binary()
    when
        Data :: iodata().
hmac_verify(Data) ->
    hmac(?NTOR_VERIFY, Data).

%% @private
-spec hmac(Key, Data) -> binary()
    when
        Key  :: iodata(),
        Data :: iodata().
hmac(Key, Data) ->
    crypto:hmac(sha256, Key, Data).

-ifdef(TEST).
create_test() ->
    ServerID               = <<"iToldYouAboutStairs.">>,
    ServerNTorPublicKey    = base16_decode("122fcc3441833e6240940c0a695dcfab70bcd4ce81f3a2d880ca66b55a7f9056"),
    ClientEphemeralKeyPair = #{ public => base16_decode("d56771c950f82086cc698e807107d81b0570dfef16b9bc1c49415f98186fd65e"),
                                secret => base16_decode("10586a2a14d8cf85a52d488e999c29bc1ab64bd4082d66a33e20601db2cdc973") },
    {Create, KeyPair}      = create(ServerID, ServerNTorPublicKey, ClientEphemeralKeyPair),
    [
        ?assertEqual(Create, base16_decode(["69546f6c64596f7541626f75745374616972732e12",
                                            "2fcc3441833e6240940c0a695dcfab70bcd4ce81f3",
                                            "a2d880ca66b55a7f9056d56771c950f82086cc698e",
                                            "807107d81b0570dfef16b9bc1c49415f98186fd65e"])),
        ?assertEqual(KeyPair, ClientEphemeralKeyPair)
    ].

server_handshake_test() ->
    ServerID                 = <<"iToldYouAboutStairs.">>,
    ServerNTorKeyPair        = #{ public => base16_decode("e0ee36663df2062500f3ba3ea93829ef1319a85c5e4a31cbb771208952bf681a"),
                                  secret => base16_decode("6010b2d3d047e7a5b31c13b5c7e9c7041431ef6732e750654750c7dd2c7fd569") },
    ClientEphemeralPublicKey = base16_decode("968cd69194860780dd05d99e992c52bb48f0ed8bd11ee4d274a735d07e7e042a"),
    ServerEphemeralKeyPair   = #{ public => base16_decode("3afbc0ae70195b88b30a77186372a48978b671bb0ed6b67de7ab33e04c5b9c02"),
                                  secret => base16_decode("f81bced970948eb5c334ae14168e987516bb23f2130c74bcc2312f609b851871") },
    {Response, Shared}       = server_handshake(ServerID, ServerNTorKeyPair, ClientEphemeralPublicKey, 72, ServerEphemeralKeyPair),
    [
        ?assertEqual(Response,
                     base16_decode(["3afbc0ae70195b88b30a77186372a48978b671bb0ed6b67de7ab33e04c5b9c02",
                                    "4ce6b76a222ae4b8ed04681287e95731d701302e8b87e3b7ef823c0d62aa0dbc"])),
        ?assertEqual(Shared,
                     base16_decode(["9b5d4c0eb97e5dd285fdfe07b4ff915a12a034d3f39b49c539901f6840772312f3e0e421",
                                    "0569be8aba28bdbbc83db9a1d48ca2d39e10a015639c0762b934b47280e7e395b1f75f6a"]))
    ].

client_handshake_test() ->
    ServerID                 = <<"iToldYouAboutStairs.">>,
    ServerNTorPublicKey      = base16_decode("e0ee36663df2062500f3ba3ea93829ef1319a85c5e4a31cbb771208952bf681a"),
    ServerEphemeralPublicKey = base16_decode("3afbc0ae70195b88b30a77186372a48978b671bb0ed6b67de7ab33e04c5b9c02"),
    ClientEphemeralKeyPair   = #{ public => base16_decode("968cd69194860780dd05d99e992c52bb48f0ed8bd11ee4d274a735d07e7e042a"),
                                  secret => base16_decode("803c44b41a780e7986e0835ad321db5c81aec58f3cb8644255d5b17318335b55") },
    ServerHandshake          = base16_decode(["3afbc0ae70195b88b30a77186372a48978b671bb0ed6b67de7ab33e04c5b9c02",
                                              "4ce6b76a222ae4b8ed04681287e95731d701302e8b87e3b7ef823c0d62aa0dbc"]),
    {Check, Shared}          = client_handshake(ServerID, ServerNTorPublicKey, ServerEphemeralPublicKey, ClientEphemeralKeyPair, 72),
    <<ServerPublicKey:32/binary,
      CheckValue:32/binary>> = ServerHandshakeCheck = base16_decode(["3afbc0ae70195b88b30a77186372a48978b671bb0ed6b67de7ab33e04c5b9c02",
                                                                     "4ce6b76a222ae4b8ed04681287e95731d701302e8b87e3b7ef823c0d62aa0dbc"]),
    [
        ?assertEqual(Check, CheckValue),
        ?assertEqual(ServerPublicKey, ServerEphemeralPublicKey),
        ?assertEqual(Shared,
                     base16_decode(["9b5d4c0eb97e5dd285fdfe07b4ff915a12a034d3f39b49c539901f6840772312f3e0e421",
                                    "0569be8aba28bdbbc83db9a1d48ca2d39e10a015639c0762b934b47280e7e395b1f75f6a"])),
        ?assertEqual(ServerHandshake, ServerHandshakeCheck)
    ].

handshake_test() ->
    ServerID                 = <<"iToldYouAboutStairs.">>,
    ServerNTorKeyPair        = #{ public => base16_decode("0e3b9a3638cbb26225986f1b47890960a5356b947c32e470f12774015bcf1114"),
                                  secret => base16_decode("b878405ccfe99f9b888be56c80121bfb5ba5bf4e765774f75dbcec901d70044a") },
    ClientEphemeralKeyPair   = #{ public => base16_decode("09fb2509c1c42bf4851fdeed00a0c243afd0740c0425c200eaf1ce3c6f27a244"),
                                  secret => base16_decode("b85baebae6149867de41c9e4fc33f7ab9abe3fa146b3dfb0408ca49942841479") },
    ServerEphemeralKeyPair   = #{ public => base16_decode("e34b5fb453038cee794ba20496e47db1b5ad4592ceac21c4530129afc7951f68"),
                                  secret => base16_decode("2830171ac0af06c98c44f6a3e05a29cc81dd67ae41ae43f11816b989055d636d") },
    {_, ClientKeyPair}       = create(ServerID, maps:get(public, ServerNTorKeyPair), ClientEphemeralKeyPair),
    {_, SharedSecretA}       = server_handshake(ServerID, ServerNTorKeyPair, maps:get(public, ClientEphemeralKeyPair), 72, ServerEphemeralKeyPair),
    {_, SharedSecretB}       = client_handshake(ServerID, maps:get(public, ServerNTorKeyPair), maps:get(public, ServerEphemeralKeyPair), ClientEphemeralKeyPair, 72),
    [
        ?assertEqual(ClientKeyPair, ClientEphemeralKeyPair),
        ?assertEqual(SharedSecretA, SharedSecretB)
    ].

hmac_verify_test() ->
    [
        ?assertEqual(hmac_verify(<<"">>),
                     base16_decode(["1e2a1675024656f174fd05d95f26aaa7f9531677e4eed4e76da02269b85a34c4"])),
        ?assertEqual(hmac_verify(<<"foobar">>),
                     base16_decode(["e00972e74219a0f97c349e73552b1734896a6f74291a00dd09ff2870410bd059"])),
        ?assertEqual(hmac_verify(<<"aaa bbb ccc">>),
                     base16_decode(["b132b5cda3f0f84ea6bad8723eade941679c53de778d2bf1f97a1b5ec0256c76"])),
        ?assertEqual(hmac_verify(<<"aaabbbccc">>),
                     base16_decode(["5cee70f6c77d10b65b0d0b1c20c7db7891786534df76c180965dc40eedeb33b9"])),
        ?assertEqual(hmac_verify(<<0, 0, 0, 0>>),
                     base16_decode(["c0abbff504a2db4b2c52a1ff1af36785d4dc9619579dc2f10141c149de906ff6"]))
    ].

hmac_mac_test() ->
    [
        ?assertEqual(hmac_mac(<<"">>),
                     base16_decode(["796ff498cb2ab62b568f4e5c6657b24711a1bc516a6639559af0c3e67ed40149"])),
        ?assertEqual(hmac_mac(<<"foobar">>),
                     base16_decode(["f54d5357308dc2ace62c226920ecab7dff8d162faf992d0497745b7da18a4d06"])),
        ?assertEqual(hmac_mac(<<"aaa bbb ccc">>),
                     base16_decode(["c7f8db82993bdb9beb1ad8ea8267a76bb10cb5ed960077de350a48538435379f"])),
        ?assertEqual(hmac_mac(<<"aaabbbccc">>),
                     base16_decode(["24389641b1edd9f569bf6d2570aeb6aabae1875c50f5c1ce66a5e21107139a31"])),
        ?assertEqual(hmac_mac(<<0, 0, 0, 0>>),
                     base16_decode(["188504215739fca18d43fd06988d37ba8df17a5d47557d5379f76ebb2fb3b9e6"]))
    ].

-endif.
