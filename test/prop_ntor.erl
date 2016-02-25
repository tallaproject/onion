%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Property Tests for onion_ntor.
%%% @end
%%% -----------------------------------------------------------
-module(prop_ntor).

%% Properties.
-export([prop_shared_secret/0]).

-include_lib("proper/include/proper.hrl").

-spec prop_shared_secret() -> term().
prop_shared_secret() ->
    ?FORALL({NTorKeyPair, IDPublicKey}, {onion_x25519:keypair(), binary()},
        begin
            %% The fingerprint of the server PublicKey.
            Fingerprint = crypto:hash(sha, IDPublicKey),

            %% NTorPublicKey and NTorSecretKey of the server.
            NTorPublicKey = maps:get(public, NTorKeyPair),

            %% The client makes the initial request:
            {<<Fingerprint:20/binary,
               NTorPublicKey:32/binary,
               ClientEPublicKey:32/binary>>,
             ClientEKeyPair} = onion_ntor:create(Fingerprint, NTorPublicKey),

            %% Assertion.
            true = ClientEPublicKey =:= maps:get(public, ClientEKeyPair),

            %% The server accepts the handshake and computes its key seed.
            {<<ServerEPublicKey:32/binary,
               ServerAuthData:32/binary>>,
             ServerKeySeed} = onion_ntor:server_handshake(Fingerprint, NTorKeyPair, ClientEPublicKey),

            %% The client receives a response and computes its key seed.
            {<<ClientAuthData:32/binary>>,
             ClientKeySeed} = onion_ntor:client_handshake(Fingerprint, NTorPublicKey, ServerEPublicKey, ClientEKeyPair),

            ServerKeySeed =:= ClientKeySeed andalso ServerAuthData =:= ClientAuthData
        end).
