%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc x509 API.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_x509).

%% API.
-export([create_certificate/1,

         sign/2,
         verify/2,

         is_self_signed/1,

         public_key/1,

         not_before/1,
         not_after/1,

         der_encode/1,
         der_decode/1
        ]).

%% Types.
-type subject() :: name
                 | email
                 | city
                 | state
                 | org
                 | org_unit
                 | country
                 | serial
                 | title
                 | dnQualifier.

-type options() :: #{
        %% Default: N/A.
        public_key  => onion_rsa:public_key(),

        %% Default: N/A.
        valid_from  => calendar:datetime(),
        valid_to    => calendar:datetime(),

        %% Default: [].
        subject    => [{subject(), term()}],

        %% Default: The value of 'subject'.
        issuer     => [{subject(), term()}],

        %% Default: v3.
        version    => v1 | v2 | v3,

        %% Default: 64-bit random positive integer
        %% from onion_random:bytes_unsigned/1.
        serial     => non_neg_integer(),

        %% Default: sha (SHA-1)
        digest     => sha | sha256 | sha384 | sha512 | md5
    }.

-include_lib("public_key/include/public_key.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-endif.

-spec create_certificate(Options) -> {ok, Certificate} | {error, Reason}
    when
        Options     :: options(),
        Certificate :: public_key:der_encoded(),
        Reason      :: term().
create_certificate(Options) when is_map(Options) ->
    PublicKey = maps:get(public_key, Options),
    Subject = maps:get(subject, Options, []),
    Certificate = #'OTPTBSCertificate'{
            version              = maps:get(version, Options, v3),
            serialNumber         = maps:get(serial, Options, onion_random:bytes_unsigned(8)),
            signature            = signature_algorithm(maps:get(digest, Options, sha)),
            validity             = validity(maps:get(valid_from, Options), maps:get(valid_to, Options)),
            subject              = subject(Subject),
            issuer               = subject(maps:get(issuer, Options, Subject)),
            subjectPublicKeyInfo = subject_public_key_info(PublicKey),
            extensions           = asn1_NOVALUE
        },
    {ok, Certificate}.

-spec sign(Certificate, SecretKey) -> CertificateDer
    when
        Certificate    :: #'OTPTBSCertificate'{},
        SecretKey      :: onion_rsa:secret_key(),
        CertificateDer :: public_key:der_encoded().
sign(#'OTPTBSCertificate'{} = Certificate, #'RSAPrivateKey'{} = SecretKey) ->
    public_key:pkix_sign(Certificate, SecretKey).

-spec verify(CertificateDer, Key) -> boolean()
    when
        CertificateDer :: public_key:der_encoded(),
        Key            :: onion_rsa:key().
verify(CertificateDer, #'RSAPrivateKey'{} = SecretKey) when is_binary(CertificateDer) ->
    verify(CertificateDer, onion_rsa:secret_key_to_public_key(SecretKey));

verify(CertificateDer, #'RSAPublicKey'{} = PublicKey) when is_binary(CertificateDer) ->
    public_key:pkix_verify(CertificateDer, PublicKey).

-spec is_self_signed(Certificate) -> boolean()
    when
        Certificate :: #'OTPTBSCertificate'{} | public_key:der_encoded().
is_self_signed(Certificate) ->
    public_key:pkix_is_self_signed(Certificate).

-spec public_key(Certificate) -> {ok, onion_rsa:public_key()} | {error, Reason}
    when
        Certificate :: #'OTPTBSCertificate'{} | public_key:der_encoded(),
        Reason      :: term().
public_key(CertificateDer) when is_binary(CertificateDer) ->
    {ok, Certificate} = der_decode(CertificateDer),
    public_key(Certificate);

public_key({'Certificate', #'TBSCertificate' { subjectPublicKeyInfo = #'SubjectPublicKeyInfo'{ subjectPublicKey = PublicKey }}, _, _}) ->
    onion_rsa:der_decode_public_key(PublicKey).

-spec not_before(Certificate) -> calendar:datetime()
    when
        Certificate :: #'OTPTBSCertificate'{} | public_key:der_encoded().
not_before(CertificateDer) when is_binary(CertificateDer) ->
    {ok, Certificate} = der_decode(CertificateDer),
    not_before(Certificate);

not_before({'Certificate', #'TBSCertificate' { validity = #'Validity' { notBefore = NotBefore }}, _, _}) ->
    decode_certificate_time(NotBefore).

-spec not_after(Certificate) -> calendar:datetime()
    when
        Certificate :: #'OTPTBSCertificate'{} | public_key:der_encoded().
not_after(CertificateDer) when is_binary(CertificateDer) ->
    {ok, Certificate} = der_decode(CertificateDer),
    not_after(Certificate);

not_after({'Certificate', #'TBSCertificate' { validity = #'Validity' { notAfter = NotAfter }}, _, _}) ->
    decode_certificate_time(NotAfter).

-spec der_encode(Certificate) -> {ok, CertificateDer} | {error, Reason}
    when
        Certificate    :: #'OTPTBSCertificate'{},
        CertificateDer :: public_key:der_encoded(),
        Reason         :: term().
der_encode(Certificate) ->
    try
        {ok, public_key:der_encode('Certificate', Certificate)}
    catch _:_ ->
        {error, invalid_certificate}
    end.

-spec der_decode(CertificateDer) -> {ok, Certificate} | {error, Reason}
    when
        CertificateDer :: public_key:der_encoded(),
        Certificate    :: term(),
        Reason         :: term().
der_decode(CertificateDer) when is_binary(CertificateDer) ->
    try
        {ok, public_key:der_decode('Certificate', CertificateDer)}
    catch _:_ ->
        {error, invalid_certificate}
    end.

%% @private
-spec signature_algorithm(SignatureAlgorithm) -> term()
    when
        SignatureAlgorithm :: sha | sha256 | sha512 | md5.
signature_algorithm(SignatureAlgorithm) when is_atom(SignatureAlgorithm) ->
    Algorithm = case SignatureAlgorithm of
                    sha ->
                        ?sha1WithRSAEncryption;
                    sha256 ->
                        ?sha256WithRSAEncryption;
                    sha384 ->
                        ?sha384WithRSAEncryption;
                    sha512 ->
                        ?sha512WithRSAEncryption;
                    md5 ->
                        ?md5WithRSAEncryption
                end,
    #'SignatureAlgorithm'{ algorithm  = Algorithm, parameters = 'NULL' }.

%% @private
-spec subject_public_key_info(PublicKey) -> term()
    when
        PublicKey :: onion_rsa:public_key().
subject_public_key_info(#'RSAPublicKey'{} = PublicKey) ->
    #'OTPSubjectPublicKeyInfo'{
        algorithm = #'PublicKeyAlgorithm'{
                algorithm  = ?rsaEncryption,
                parameters = 'NULL'
            },
        subjectPublicKey = PublicKey
    }.

%% @private
-spec validity(ValidFrom, ValidTo) -> term()
    when
        ValidFrom :: calendar:datetime(),
        ValidTo   :: calendar:datetime().
validity(ValidFrom, ValidTo) ->
    #'Validity'{
        notBefore = {utcTime, format_timestamp(ValidFrom)},
        notAfter  = {utcTime, format_timestamp(ValidTo)}
    }.

%% @private
-spec format_timestamp(Datetime) -> string()
    when
        Datetime :: calendar:datetime().
format_timestamp({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0wZ",
                                [Year rem 100, Month, Day, Hour, Minute, Second])).

%% @private
-spec decode_certificate_time(CertTime) -> calendar:datetime()
    when
        CertTime :: term().
decode_certificate_time({utcTime, [Y1, Y2, M1, M2, D1, D2, H1, H2, M3, M4, S1, S2, Z]}) ->
    case list_to_integer([Y1, Y2]) of
        N when N >= 50 ->
            decode_certificate_time({generalTime, [$1, $9, Y1, Y2, M1, M2, D1, D2, H1, H2, M3, M4, S1, S2, Z]});
        _ ->
            decode_certificate_time({generalTime, [$2, $0, Y1, Y2, M1, M2, D1, D2, H1, H2, M3, M4, S1, S2, Z]})
    end;

decode_certificate_time({generalTime, [Y1, Y2, Y3, Y4, M1, M2, D1, D2, H1, H2, M3, M4, S1, S2, $Z]}) ->
    Year  = list_to_integer([Y1, Y2, Y3, Y4]),
    Month = list_to_integer([M1, M2]),
    Day   = list_to_integer([D1, D2]),
    Hour  = list_to_integer([H1, H2]),
    Min   = list_to_integer([M3, M4]),
    Sec   = list_to_integer([S1, S2]),
    {{Year, Month, Day}, {Hour, Min, Sec}}.

-spec subject(Subjects) -> term()
    when
        Subjects :: [{atom(), term()}].
subject(Subjects) when is_list(Subjects) ->
    Mapper = fun (Subject) ->
                 {Type, Value} = subject_entry_encode(Subject),
                 [#'AttributeTypeAndValue'{
                         type  = Type,
                         value = Value
                     }]
             end,
    {rdnSequence, lists:map(Mapper, Subjects)}.

-spec subject_entry_encode({atom(), term()}) -> {term(), term()}.
subject_entry_encode({name, Name}) ->
    {?'id-at-commonName', {printableString, Name}};

subject_entry_encode({email, Email}) ->
    {?'id-emailAddress', Email};

subject_entry_encode({city, City}) ->
    {?'id-at-localityName', {printableString, City}};

subject_entry_encode({state, State}) ->
    {?'id-at-stateOrProvinceName', {printableString, State}};

subject_entry_encode({org, Organization}) ->
    {?'id-at-organizationName', {printableString, Organization}};

subject_entry_encode({org_unit, OrganizationUnit}) ->
    {?'id-at-organizationalUnitName', {printableString, OrganizationUnit}};

subject_entry_encode({country, Country}) ->
    {?'id-at-countryName', Country};

subject_entry_encode({serial, Serial}) ->
    {?'id-at-serialNumber', Serial};

subject_entry_encode({title, Title}) ->
    {?'id-at-title', {printableString, Title}};

subject_entry_encode({dnQualifer, DnQ}) ->
    {?'id-at-dnQualifier', DnQ}.

-ifdef(TEST).
create_verify_certificate_test() ->
    {ok, #{ public := PublicKey, secret := SecretKey }} = onion_rsa:keypair(1024),
    {ok, Certificate} = create_certificate(#{
                                public_key => PublicKey,
                                subject    => [{name, "www.example.org"}],
                                valid_from => {{2016, 1, 1}, {0, 0, 0}},
                                valid_to   => {{2017, 1, 1}, {0, 0, 0}}
                            }),
    CertificateDer = sign(Certificate, SecretKey),
    ?assert(verify(CertificateDer, PublicKey)).
-endif.
