%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Diffie-Hellman API
%%%
%%% This module contains functions for key generation and shared secret
%%% computations for the Diffie-Hellman key exchange algorithm.
%%%
%%% Tor uses a generator (g) of 2 and the modulus (p) is a 1024-bit safe prime
%%% from RFC 2409 section 6.2. See the Tor specification for more information.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_dh).

%% API.
-export([keypair/0,
         shared_secret/2,
         params/0]).

%% Types.
-export_type([secret_key/0,
              public_key/0,
              keypair/0]).

-type secret_key() :: crypto:dh_private().
-type public_key() :: crypto_dh:public().
-type keypair()    :: #{ secret => secret_key(),
                         public => public_key() }.

-define(G, 2).

%% RFC 2409, section 6.2.
-define(P, 179769313486231590770839156793787453197860296048756011706444423684197180216158519368947833795864925541502180565485980503646440548199239100050792877003355816639229553136239076508735759914822574862575007425302077447712589550957937778424442426617334727629299387668709205606050270810842907692932019128194467627007).

-include("onion_test.hrl").

%% @doc Creates a new Diffie-Hellman keypair.
%%
%% Generates and returns a new Diffie-Hellman keypair. The return value is a
%% map to avoid using the public key as the secret key and vice versa.
%%
%% @end
-spec keypair() -> keypair().
keypair() ->
    {PublicKey, SecretKey} = crypto:generate_key(dh, params()),
    #{ secret => SecretKey, public => PublicKey }.

%% @doc Computes the shared secret between a SecretKey and PublicKey.
%%
%% This function computes the shared secret between a given SecretKey and
%% PublicKey.
%%
%% @end
-spec shared_secret(SecretKey, PublicKey) -> SharedSecret
    when
        SecretKey    :: secret_key(),
        PublicKey    :: public_key(),
        SharedSecret :: binary().
shared_secret(SecretKey, PublicKey) ->
    crypto:compute_key(dh, PublicKey, SecretKey, params()).

%% @doc Diffie-Hellman parameters used by Tor (from RFC 2409).
%%
%% This function will return a list containing two elements: P (the modulus)
%% and G (the generator). These parameters are needed for the other
%% Diffie-Hellman computations in this module and in the OTP crypto module.
%%
%% @end
-spec params() -> [non_neg_integer()].
params() ->
    [?P, ?G].
