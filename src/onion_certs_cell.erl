%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Certs Cell Utilities
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_certs_cell).

%% API.
-export([validate_server/2]).

-spec validate_server(CertsCell, PublicKey) -> boolean()
    when
        CertsCell :: onion_cell:certs_cell(),
        PublicKey :: public_key:der_encoded().
validate_server(CertsCell, _PublicKey) ->
    try
        Now = onion_time:unix_epoch(),

        LinkCertificate = type_find(1, CertsCell),
        IDCertificate = type_find(2, CertsCell),

        LinkNotBefore = onion_time:to_unix_epoch(onion_x509:not_before(LinkCertificate)),
        LinkNotAfter  = onion_time:to_unix_epoch(onion_x509:not_after(LinkCertificate)),

        IDNotBefore = onion_time:to_unix_epoch(onion_x509:not_before(IDCertificate)),
        IDNotAfter  = onion_time:to_unix_epoch(onion_x509:not_after(IDCertificate)),

        {ok, _LinkPublicKey} = onion_x509:public_key(LinkCertificate),
        {ok, IDPublicKey}    = onion_x509:public_key(IDCertificate),

        %% The CERTS cell contains exactly one CertType 1 "Link" certificate.
        1 = type_count(1, CertsCell),

        %% The CERTS cell contains exactly one CertType 2 "ID" certificate.
        1 = type_count(2, CertsCell),

        %% Both certificates have validAfter and validUntil dates that are not expired.
        true = Now >= LinkNotBefore andalso Now < LinkNotAfter,
        true = Now >= IDNotBefore   andalso Now < IDNotAfter,

        %% The certified key in the Link certificate matches the link key that was used to negotiate the TLS connection.
        %% FIXME(ahf): Missing.
        %% true = onion_x509:verify(LinkCertificate, PublicKey),

        %% The certified key in the ID certificate is a 1024-bit RSA key.
        1024 = onion_rsa:key_size(IDPublicKey),

        %% The certified key in the ID certificate was used to sign both certificates.
        true = onion_x509:verify(LinkCertificate, IDPublicKey),
        true = onion_x509:verify(IDCertificate, IDPublicKey),

        %% The link certificate is correctly signed with the key in the ID certificate
        %% FIXME(ahf): Most likely wrong.
        true = onion_x509:verify(LinkCertificate, IDPublicKey),

        %% The ID certificate is correctly self-signed.
        true = onion_x509:is_self_signed(IDCertificate),

        true
    catch _:_ ->
        false
    end.

type_find(Type, [#{ certificate_type := Type, certificate := Certificate } | _]) ->
    Certificate;
type_find(Type, [_ | Rest]) ->
    type_find(Type, Rest).

%% @private
type_count(Type, CertsCell) ->
    type_count(Type, CertsCell, 0).

type_count(Type, [#{ certificate_type := Type } | Rest], Count) ->
    type_count(Type, Rest, Count + 1);

type_count(Type, [_ | Rest], Count) ->
    type_count(Type, Rest, Count);

type_count(_, [], Count) ->
    Count.


