%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Diffie-Hellman Utilities
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_dh).

%% API.
-export([keypair/0,
         shared_secret/2,
         params/0]).

-type keypair() :: #{ secret => crypto:dh_private(), public => crypto:dh_public() }.

-define(G, 2).

%% RFC 2409, section 6.2.
-define(P, 179769313486231590770839156793787453197860296048756011706444423684197180216158519368947833795864925541502180565485980503646440548199239100050792877003355816639229553136239076508735759914822574862575007425302077447712589550957937778424442426617334727629299387668709205606050270810842907692932019128194467627007).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-endif.

-spec keypair() -> keypair().
keypair() ->
    {PublicKey, SecretKey} = crypto:generate_key(dh, params()),
    #{ secret => SecretKey, public => PublicKey }.

-spec shared_secret(SecretKey, PublicKey) -> SharedSecret
    when
        SecretKey    :: crypto:dh_private(),
        PublicKey    :: crypto:dh_public(),
        SharedSecret :: binary().
shared_secret(SecretKey, PublicKey) ->
    crypto:compute_key(dh, PublicKey, SecretKey, params()).

-spec params() -> [non_neg_integer()].
params() ->
    [?P, ?G].

-ifdef(TEST).
test_keypair() ->
    #{ secret := S, public := P } = keypair(),
    {S, P}.

prop_shared_secret() ->
    ?FORALL({{AS, AP}, {BS, BP}}, {test_keypair(), test_keypair()},
        begin
            SharedA = shared_secret(AS, BP),
            SharedB = shared_secret(BS, AP),
            SharedA =:= SharedB
        end).
-endif.
