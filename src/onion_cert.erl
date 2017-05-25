%%%
%%% Copyright (c) 2017 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Lasse Grinderslev Andersen <lasse@etableret.dk>
%%% @doc API to handle certificates as defined in cert-spec.txt
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_cert).

-export([decode/1]).


-spec decode(CertEncoded) -> {Cert, Message, Signature}
    when
        CertEncoded :: binary(),
        Cert        :: map(),
        Message     :: binary(),
        Signature   :: binary().
decode(CertEncoded) ->
    {Message, Signature} = split_cert(CertEncoded, size(CertEncoded)),
    <<Version:8/integer,
      CertType:8/integer,
      ExpirationDate:32/integer, % FIXME (lga) check that this is valid
      CertKeyType:8/integer,
      CertKey:32/binary,
      NExtensions:8/integer,
      Rest/binary>> = CertEncoded,
    Extensions = decode_extensions(NExtensions, Rest, []),
    Cert = #{ version         => Version,
              cert_type       => CertType,
              expiration_date => ExpirationDate,
              cert_key_type   => CertKeyType,
              cert_key        => CertKey,
              extensions      => Extensions },
    {Cert, Message, Signature}.


%% @private
-spec split_cert(Cert, CertLen) -> {CertData, Signature}
    when
        Cert      :: binary(),
        CertLen   :: pos_integer(),
        CertData  :: binary(),
        Signature :: binary().
split_cert(Cert, CertLen) ->
    CertMessageLen = CertLen - 64,
    <<CertData:CertMessageLen/binary, Signature/binary>> = Cert,
    {CertData, Signature}.


%% @private
-spec decode_extensions(NExtensions, EncodedExtensions, DecodedExtensions) -> DecodedExtensions
    when
        NExtensions       :: non_neg_integer(),
        EncodedExtensions :: binary(),
        DecodedExtensions :: [{integer(), integer(), binary()}].
decode_extensions(0, _Rest, DecodedExtensions) ->
    lists:reverse(DecodedExtensions);

decode_extensions(NExtensions, <<ExtLength:16/integer, ExtType:8/integer, ExtFlags:8/integer, ExtData:ExtLength/binary, Rest/binary>>, DecodedExtensions) ->
    decode_extensions(NExtensions - 1, Rest, [{ExtType, ExtFlags, ExtData} | DecodedExtensions]).
