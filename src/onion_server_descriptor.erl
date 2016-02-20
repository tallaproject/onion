%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Server Descriptor API.
%%%
%%% This module contains functions for working with server descriptors. For
%%% information about the server descriptor format see section 2.1.1 of Tor's
%%% dir-spec.txt
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_server_descriptor).

%% API.
-export([encode/1]).

-include("onion_test.hrl").

%% @doc Encode a given ServerDescriptor into an iolist().
%%
%% This function encodes a given ServerDescriptor into an iolist().
%%
%% @end
-spec encode(ServerDescriptor) -> iolist()
    when
        ServerDescriptor :: map().
encode(ServerDescriptor) ->
    %% Router information.
    Nickname = maps:get(nickname, ServerDescriptor),
    Address  = maps:get(address, ServerDescriptor),
    ORPort   = maps:get(or_port, ServerDescriptor),

    %% Bandwidth information.
    BWAverage  = maps:get(bandwidth_average, ServerDescriptor),
    BWBurst    = maps:get(bandwidth_burst, ServerDescriptor),
    BWObserved = maps:get(bandwidth_observed, ServerDescriptor),

    %% Platform information.
    Platform = maps:get(platform, ServerDescriptor),

    %% Contact information.
    Contact = maps:get(contact, ServerDescriptor),

    %% Family.
    Family = maps:get(family, ServerDescriptor, []),

    %% Published information.
    Published = maps:get(published, ServerDescriptor),

    %% Fingerprint information.
    Fingerprint = maps:get(fingerprint, ServerDescriptor),

    %% Uptime information.
    Uptime = maps:get(uptime, ServerDescriptor),

    %% Onion Key information.
    OnionKey = maps:get(onion_key, ServerDescriptor),

    %% NTor Onion Key information.
    NTorOnionKey = maps:get(ntor_onion_key, ServerDescriptor),

    %% Exit policy information (with a sensible default, just in case).
    ExitPolicy = maps:get(exit_policy, ServerDescriptor, [{reject, "*:*"}]),

    %% Encode the document.
    onion_document:encode([
        %% "router" nickname address ORPort SOCKSPort DirPort
        {router, [Nickname, inet:ntoa(Address), ORPort, 0, 0]},

        %% "bandwidth" bandwidth-avg bandwidth-burst bandwidth-observed
        {bandwidth, [BWAverage, BWBurst, BWObserved]},

        %% "platform" string
        {platform, [Platform]},

        %% "contact" info
        {contact, [Contact]},

        %% "family" names (ignore if empty)
        case Family of
            [] ->
                [];

            Family ->
                {family, Family}
        end,

        %% "published" YYYY-MM-DD HH:MM:SS
        {published, [onion_document:encode_datetime(Published)]},

        %% "fingerprint" fingerprint
        {fingerprint, [Fingerprint]},

        %% "uptime" number
        {uptime, [Uptime]},

        %% "onion-key" NL a public key in PEM format
        {'onion-key', [], [{"RSA PUBLIC KEY", OnionKey}]},

        %% "ntor-onion-key" base-64-encoded-key
        {'ntor-onion-key', [onion_base64:encode(NTorOnionKey)]},

        %% "signing-key" NL a public key in PEM format
        {'signing-key', [], []},

        %% "accept" exitpattern
        %% "reject" exitpattern
        lists:map(fun ({Rule, Pattern}) ->
                      {Rule, [Pattern]}
                  end, ExitPolicy),

        %% "router-signature" NL Signature (the Signature is added outside of this function).
        {'router-signature', []}
    ]).
