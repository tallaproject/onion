%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Relay Utility Functions.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_relay).

%% API.
-export([verify_digest/2]).

-include("onion_test.hrl").

-spec verify_digest(Context, Data) -> {ok, NewContext} | {error, Reason}
    when
        Context    :: term(),
        NewContext :: term(),
        Data       :: binary(),
        Reason     :: term().
verify_digest(Context, Data) ->
    case Data of
        <<Start:5/binary, _:32/integer, End/binary>> ->
            NewContext = crypto:hash_update(Context, <<Start/binary, 0:32/integer, End/binary>>),
            do_verify_digest(NewContext, Data);

        _ ->
            {error, insufficient_data}
    end.

%% @private
-spec do_verify_digest(Context, Data) -> {ok, NewContext} | {error, Reason}
    when
        Context    :: term(),
        NewContext :: term(),
        Data       :: binary(),
        Reason     :: term().
do_verify_digest(Context, Data) ->
    <<Digest:4/binary, _/binary>> = crypto:hash_final(Context),
    case Data of
        <<_:5/binary, Digest:4/binary, _/binary>> ->
            {ok, Context};

        _ ->
            {error, verification_failed}
    end.

-ifdef(TEST).
verify_digest_test() ->
    Hash = {sha, <<1,35,69,103,137,171,205,239,254,220,
                   186,152,118,84,50,16,240,225,210,195,
                   160,0,0,0,0,0,0,0,126,64,140,176,174,
                   7,61,244,55,41,61,217,240,84,150,89,
                   197,43,80,203,0,0,0,0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,0,0,0,0,20,0,0,0>>},
    Data = <<14,0,0,0,0,154,76,8,0,0,119,2,0,6,127,0,0,1,
             19,137,2,20,204,23,63,205,199,16,31,26,74,47,
             127,29,66,58,63,195,85,152,184,62,0,2,0,84,
             204,23,63,205,199,16,31,26,74,47,127,29,66,
             58,63,195,85,152,184,62,95,125,78,90,232,169,
             164,172,42,160,233,148,34,209,180,156,188,137,
             16,97,58,7,136,61,224,192,177,38,189,154,178,
             13,174,191,6,235,71,244,157,82,178,23,27,119,
             101,72,155,217,109,63,128,31,51,38,120,142,113,
             192,84,58,51,158,81,19,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
    [
        ?assertMatch({ok, _}, verify_digest(Hash, Data)),
        ?assertMatch({error, _}, verify_digest(Hash, <<1, Data/binary>>)),
        ?assertMatch({error, _}, verify_digest(Hash, <<Data/binary, 1>>))
    ].

-endif.
