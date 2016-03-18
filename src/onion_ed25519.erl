%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Ed25519 API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_ed25519).

%% API.
-export([keypair/0,
         sign/2,
         verify/3
        ]).

%% Types.
-export_type([secret_key/0,
              public_key/0,
              keypair/0
             ]).

-type secret_key() :: binary().
-type public_key() :: binary().
-type keypair()    :: #{ secret => secret_key(), public => public_key() }.

-include("onion_test.hrl").

%% @doc Creates a new Ed25519 keypair.
%%
%% Generates and returns a new Ed25519 keypair. The return value is a map to
%% avoid using the public key as the secret key and vice versa.
%%
%% @end
-spec keypair() -> KeyPair
    when
        KeyPair :: keypair().
keypair() ->
    enacl:sign_keypair().

%% @doc Sign a given Message using a given SecretKey
%%
%% Returns a detached signature of a given Message using the given SecretKey.
%%
%% @end
-spec sign(Message, SecretKey) -> Signature
    when
        Message   :: iolist(),
        SecretKey :: secret_key(),
        Signature :: binary().
sign(Message, SecretKey) ->
    enacl:sign_detached(Message, SecretKey).


%% @doc Verify a Signature of a Message using the PublicKey
%%
%% Verifies a given Signature and Message using the given PublicKey.
%%
%% @end
-spec verify(Message, Signature, PublicKey) -> boolean()
    when
        Signature :: binary(),
        Message   :: binary(),
        PublicKey :: public_key().
verify(Signature, Message, PublicKey) ->
    case enacl:sign_verify_detached(Signature, Message, PublicKey) of
        {ok, Message} ->
            true;

        {error, failed_verification} ->
            false
    end.

-ifdef(TEST).
nacl_empty_test() ->
    SecretKey = base16_decode(["9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60",
                               "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"]),
    PublicKey = base16_decode("d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"),
    Message = <<>>,
    Signature = base16_decode(["e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e06522490155",
                               "5fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"]),
    [
        ?assertEqual(Signature, sign(Message, SecretKey)),
        ?assert(verify(Signature, Message, PublicKey))
    ].

nacl_72_test() ->
    SecretKey = base16_decode(["4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
                               "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"]),
    PublicKey = base16_decode("3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"),
    Message = base16_decode("72"),
    Signature = base16_decode(["92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da",
                               "085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00"]),
    [
        ?assertEqual(Signature, sign(Message, SecretKey)),
        ?assert(verify(Signature, Message, PublicKey))
    ].

-endif.
