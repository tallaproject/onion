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
         is_degenerate/1,
         params/0]).

%% Types.
-export_type([secret_key/0,
              public_key/0,
              keypair/0]).

-type secret_key() :: crypto:dh_private().
-type public_key() :: crypto:dh_public().
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
    #{ secret => crypto:bytes_to_integer(SecretKey), public => crypto:bytes_to_integer(PublicKey) }.

%% @doc Computes the shared secret between a SecretKey and PublicKey.
%%
%% This function computes the shared secret between a given SecretKey and
%% PublicKey.
%%
%% @end
-spec shared_secret(SecretKey, PublicKey) -> {ok, SharedSecret} | {error, Reason}
    when
        SecretKey    :: secret_key(),
        PublicKey    :: public_key(),
        SharedSecret :: binary(),
        Reason       :: term().
shared_secret(SecretKey, PublicKey) ->
    case is_degenerate(PublicKey) of
        false ->
            {ok, crypto:compute_key(dh, PublicKey, SecretKey, params())};

        true ->
            {error, degenerate_public_key}
    end.

%% @doc Verify if a given integer is degenerate.
%%
%% This function can be used to check that the g^x or g^x value is not
%% degenerate. That is, the value is within the range 2 and P - 2 (both
%% included).
%%
%% @end
-spec is_degenerate(Value) -> boolean()
    when
        Value :: non_neg_integer().
is_degenerate(Value) when is_binary(Value) ->
    is_degenerate(crypto:bytes_to_integer(Value));

is_degenerate(Value) when is_integer(Value), Value > 1 andalso Value < ?P - 1 ->
    false;

is_degenerate(Value) when is_integer(Value) ->
    true.

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

-ifdef(TEST).
degenerate_test() ->
    [
        ?assert(is_degenerate(-1)),
        ?assert(is_degenerate(0)),
        ?assert(is_degenerate(1)),
        ?assert(is_degenerate(?P)),
        ?assert(is_degenerate(?P - 1)),

        ?assertNot(is_degenerate(2)),
        ?assertNot(is_degenerate(3)),
        ?assertNot(is_degenerate(?P - 2))
    ].

-endif.
