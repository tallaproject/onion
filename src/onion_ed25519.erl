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
         verify/2
        ]).

%% Types.
-type secret_key() :: binary().
-type public_key() :: binary().
-type keypair()    :: #{ secret => secret_key(), public => public_key() }.

-include("onion_test.hrl").

-spec keypair() -> KeyPair
    when
        KeyPair :: keypair().
keypair() ->
    enacl:sign_keypair().

-spec sign(Message, SecretKey) -> binary()
    when
        Message   :: iolist(),
        SecretKey :: secret_key().
sign(Message, SecretKey) ->
    enacl:sign(Message, SecretKey).

-spec verify(SignedMessage, PublicKey) -> {ok, Message} | {error, failed_verification}
    when
        SignedMessage :: binary(),
        PublicKey     :: public_key(),
        Message       :: binary().
verify(SignedMessage, PublicKey) ->
    enacl:sign_open(SignedMessage, PublicKey).

-ifdef(TEST).
test_keypair() ->
    #{ secret := SecretKey, public := PublicKey } = keypair(),
    {SecretKey, PublicKey}.

prop_sign_verify() ->
    ?FORALL({{S, P}, M}, {test_keypair(), binary()},
       begin
           SignedMessage = sign(M, S),
           {ok, M2} = verify(SignedMessage, P),
           M =:= M2
       end).
-endif.
