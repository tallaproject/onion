%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Property Tests for onion_rsa.
%%% @end
%%% -----------------------------------------------------------
-module(prop_rsa).

%% Properties.
-export([prop_bit_size/0,
         prop_der_iso/0,
         prop_pem_iso/0,
         prop_sign_verify/0,
         prop_sign_verify_failure/0
        ]).

-include_lib("public_key/include/public_key.hrl").
-include_lib("onion/include/onion_test.hrl").

-spec prop_bit_size() -> term().
prop_bit_size() ->
    ?FORALL(RSAKeySize, key_size(),
        begin
            {S, P} = test_keypair(RSAKeySize),
            #'RSAPrivateKey'{ modulus = N } = S,
            #'RSAPublicKey'{ modulus = N } = P,
            bit_size(binary:encode_unsigned(N)) =:= RSAKeySize
        end).

-spec prop_der_iso() -> term().
prop_der_iso() ->
    ?FORALL(RSAKeySize, key_size(),
        begin
            {S, P} = test_keypair(RSAKeySize),
            {ok, SecretDer} = onion_rsa:der_encode(S),
            {ok, S2} = onion_rsa:der_decode_secret_key(SecretDer),

            {ok, PublicDer} = onion_rsa:der_encode(P),
            {ok, P2} = onion_rsa:der_decode_public_key(PublicDer),

            S =:= S2 andalso P =:= P2
        end).

-spec prop_pem_iso() -> term().
prop_pem_iso() ->
    ?FORALL(RSAKeySize, key_size(),
        begin
            {S, P} = test_keypair(RSAKeySize),
            {ok, SecretPem} = onion_rsa:pem_encode(S),
            {ok, S2} = onion_rsa:pem_decode(SecretPem),

            {ok, PublicPem} = onion_rsa:pem_encode(P),
            {ok, P2} = onion_rsa:pem_decode(PublicPem),

            S =:= S2 andalso P =:= P2
        end).

-spec prop_sign_verify() -> term().
prop_sign_verify() ->
    ?FORALL({RSAKeySize, Message}, {key_size(), binary()},
        begin
            {S, P} = test_keypair(RSAKeySize),
            Signature = onion_rsa:sign(sha, Message, S),
            onion_rsa:verify(sha, Message, Signature, P)
        end).

-spec prop_sign_verify_failure() -> term().
prop_sign_verify_failure() ->
    ?FORALL({RSAKeySize, Message}, {key_size(), binary()},
        begin
            {S, P} = test_keypair(RSAKeySize),
            Signature = onion_rsa:sign(sha, Message, S),
            not onion_rsa:verify(sha, <<"foo", Message/binary>>, Signature, P)
        end).

%% @private
-spec key_size() -> term().
key_size() ->
    oneof([1024, 2048, 4096]).

%% @private
-spec test_keypair(Size :: non_neg_integer()) -> term().
test_keypair(Size) ->
    {ok, #{ secret := Secret, public := Public }} = onion_rsa:keypair(Size),
    {Secret, Public}.
