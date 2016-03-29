%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Onion Authenticate Cell Utilities
%%% @end
%%% -----------------------------------------------------------
-module(onion_authenticate_cell).

%% API.
-export([create/1]).

-include("onion_test.hrl").

-type config() :: #{
        %% CID: A SHA256 hash of the initiator's RSA1024 identity key [32 octets].
        client_identity_public_key => onion_rsa:public_key(),

        %% SID: A SHA256 hash of the responder's RSA1024 identity key [32 octets].
        server_identity_public_key => onion_rsa:public_key(),

        %% SLOG: A SHA256 hash of all bytes sent from the responder to the
        %% initiator as part of the negotiation up to and including the
        %% AUTH_CHALLENGE cell; that is, the VERSIONS cell, the CERTS cell, the
        %% AUTH_CHALLENGE cell, and any padding cells.  [32 octets].
        server_log => binary(),

        %% CLOG: A SHA256 hash of all bytes sent from the initiator to the
        %% responder as part of the negotiation so far; that is, the VERSIONS
        %% cell and the CERTS cell and any padding cells. [32 octets].
        client_log => binary(),

        %% SCERT: A SHA256 hash of the responder's TLS link certificate. [32 octets].
        server_certificate => public_key:der_encoded(),

        %% TLSSECRETS: A SHA256 HMAC, using the TLS master secret as the secret
        %% key, of the following:
        %%   - client_random, as sent in the TLS Client Hello.
        %%   - server_random, as sent in the TLS Server Hello.
        %%   - the NUL terminated ASCII string:
        %%     "Tor V3 handshake TLS cross-certification".
        %% [32 octets].
        ssl_session => onion_ssl_session:t(),

        %% SIG: A signature of a SHA256 hash of all the previous fields using
        %% the initiator's "Authenticate" key as presented.  (As always in Tor,
        %% we use OAEP-MGF1 padding; see tor-spec.txt section 0.3.) [variable
        %% length].
        authentication_secret_key => onion_rsa:secret_key()
    }.

%% @doc Create an authenticate cell from the given configuration.
-spec create(Config) -> onion_cell:t()
    when
        Config :: config().
create(Config) ->
    Data = iolist_to_binary([<<"AUTH0001">>,
                             public_key_hash(maps:get(client_identity_public_key, Config)),
                             public_key_hash(maps:get(server_identity_public_key, Config)),
                             maps:get(server_log, Config),
                             maps:get(client_log, Config),
                             certificate_hash(maps:get(server_certificate, Config)),
                             tls_secrets(maps:get(ssl_session, Config)),
                             random_bytes()]),

    Hash = crypto:hash(sha256, Data),
    Signature = onion_rsa:private_encrypt(Hash, maps:get(authentication_secret_key, Config), rsa_pkcs1_padding),
    onion_cell:authenticate(1, <<Data/binary, Signature/binary>>).

%% @private
-spec tls_secrets(SSLSession) -> binary()
    when
        SSLSession :: onion_ssl_session:t().
tls_secrets(SSLSession) ->
    ClientRandom = onion_ssl_session:client_random(SSLSession),
    ServerRandom = onion_ssl_session:server_random(SSLSession),
    MasterSecret = onion_ssl_session:master_secret(SSLSession),
    crypto:hmac(sha256, MasterSecret, [ClientRandom, ServerRandom, "Tor V3 handshake TLS cross-certification", <<0>>]).

%% @private
-spec public_key_hash(PublicKey) -> binary()
    when
        PublicKey :: onion_rsa:public_key().
public_key_hash(PublicKey) ->
    {ok, EncodedPublicKey} = onion_rsa:der_encode(PublicKey),
    crypto:hash(sha256, EncodedPublicKey).

%% @private
-spec certificate_hash(Certificate) -> binary()
    when
        Certificate :: public_key:der_encoded().
certificate_hash(Certificate) ->
    crypto:hash(sha256, Certificate).

%% @private
-spec random_bytes() -> binary().
random_bytes() ->
    onion_random:bytes(24).

-ifdef(TEST).
create_size_test() ->
    {ok, #{ public := ClientPublicKey }} = onion_rsa:keypair(1024),
    {ok, #{ public := ServerPublicKey }} = onion_rsa:keypair(1024),
    {ok, #{ secret := AuthSecretKey }}   = onion_rsa:keypair(1024),

    Cell = create(#{
             client_identity_public_key => ClientPublicKey,
             server_identity_public_key => ServerPublicKey,

             server_log => crypto:hash(sha256, <<>>),
             client_log => crypto:hash(sha256, <<>>),

             server_certificate => <<>>,

              ssl_session => #{
                master_secret => <<>>,
                client_random => <<>>,
                server_random => <<>>
               },

              authentication_secret_key => AuthSecretKey
            }),
    ?assertEqual(224 + 128, byte_size(maps:get(auth, maps:get(payload, Cell)))).

-endif.
