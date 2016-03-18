%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Onion Router Cell Utilities
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_cell).

%% API.
-export([padding/0,
         create/2,
         created/1,
         relay/5,
         destroy/1,
         create_fast/1,
         created_fast/2,
         versions/0,
         netinfo/2,
         netinfo/3,
         relay_early/1,
         create2/2,
         created2/2,
         vpadding/0,
         certs/1,
         auth_challenge/2,
         authenticate/2,
         authorize/0,

         encode/2,
         decode/2
        ]).

-type circuit() :: 0 .. 4294967295.

-type command() :: padding
                 | create
                 | created
                 | relay
                 | destroy
                 | create_fast
                 | created_fast
                 | versions
                 | netinfo
                 | relay_early
                 | create2
                 | created2
                 | vpadding
                 | certs
                 | auth_challenge
                 | authenticate
                 | authorize.

-type cell() :: #{ circuit => circuit(),
                   command => command(),
                   payload => term() }.

-type relay_command() :: relay_begin
                       | relay_data
                       | relay_end
                       | relay_connected
                       | relay_sendme
                       | relay_extend
                       | relay_extended
                       | relay_truncate
                       | relay_truncated
                       | relay_drop
                       | relay_resolve
                       | relay_resolved
                       | relay_begin_dir
                       | relay_extend2
                       | relay_extended2.

-type error_code() :: none
                    | protocol
                    | internal
                    | requested
                    | hibernating
                    | resource_limit
                    | connect_failed
                    | or_identity
                    | or_connection_closed
                    | finished
                    | timeout
                    | destroyed
                    | no_such_service.

-include("onion_test.hrl").

%% All values are in bytes.
-define(KEY_LEN, 16).
-define(DH_LEN, 128).
-define(DH_SEC_LEN, 40).
-define(PK_ENC_LEN, 128).
-define(PK_PAD_LEN, 42).
-define(HASH_LEN, 20).

-define(TAP_C_HANDSHAKE_LEN, (?DH_LEN + ?KEY_LEN + ?PK_PAD_LEN)).
-define(TAP_S_HANDSHAKE_LEN, (?DH_LEN + ?HASH_LEN)).

-define(PAYLOAD_LEN, 509).

-spec padding() -> cell().
padding() ->
    cell(0, padding, []).

-spec create(Type, Data) -> cell()
    when
        Type :: ntor | tap,
        Data :: binary().
create(Type, Data) ->
    cell(0, create, #{ type => Type, data => Data }).

-spec created(Data) -> cell()
    when
        Data :: binary().
created(Data) ->
    cell(0, created, #{ data => Data }).

-spec relay(Command, Recognized, StreamID, Digest, Data) -> cell()
    when
        Command    :: relay_command(),
        Recognized :: non_neg_integer(),
        StreamID   :: non_neg_integer(),
        Digest     :: non_neg_integer(),
        Data       :: binary().
relay(Command, Recognized, StreamID, Digest, Data) ->
    cell(0, relay, #{ command    => Command,
                      recognized => Recognized,
                      stream     => StreamID,
                      digest     => Digest,
                      data       => Data }).

-spec destroy(Reason) -> cell()
    when
        Reason :: error_code().
destroy(Reason) ->
    cell(0, destroy, #{ reason => Reason }).

-spec create_fast(KeyMaterial) -> cell()
    when
        KeyMaterial :: binary().
create_fast(KeyMaterial) ->
    cell(0, create_fast, #{ key_material => KeyMaterial }).

-spec created_fast(KeyMaterial, DerivativeKeyData) -> cell()
    when
        KeyMaterial       :: binary(),
        DerivativeKeyData :: binary().
created_fast(KeyMaterial, DerivativeKeyData) ->
    cell(0, created_fast, #{ key_material      => KeyMaterial,
                             DerivativeKeyData => DerivativeKeyData }).

-spec versions() -> cell().
versions() ->
    cell(0, versions, onion_protocol:supported()).

-spec netinfo(TargetAddress, SourceAddresses) -> cell()
    when
        TargetAddress   :: inet:ip_address(),
        SourceAddresses :: [inet:ip_address()].
netinfo(TargetAddress, SourceAddresses) ->
    netinfo(onion_time:epoch(), TargetAddress, SourceAddresses).

-spec netinfo(Timestamp, TargetAddress, SourceAddresses) -> cell()
    when
        Timestamp       :: non_neg_integer(),
        TargetAddress   :: inet:ip_address(),
        SourceAddresses :: [inet:ip_address()].
netinfo(Timestamp, TargetAddress, SourceAddresses) ->
    cell(0, netinfo, #{ timestamp        => Timestamp,
                        target_address   => TargetAddress,
                        source_addresses => SourceAddresses }).

-spec relay_early(Data) -> cell()
    when
        Data :: binary().
relay_early(Data) ->
    cell(0, relay_early, #{ data => Data }).


-spec create2(Type, Data) -> cell()
    when
        Type :: ntor | tap,
        Data :: binary().
create2(Type, Data) ->
    cell(0, create2, #{ type => Type, data => Data }).

-spec created2(Circuit, Data) -> cell()
    when
        Circuit :: non_neg_integer(),
        Data    :: binary().
created2(Circuit, Data) ->
    cell(Circuit, created2, #{ data => Data }).

-spec vpadding() -> cell().
vpadding() ->
    cell(0, vpadding, []).

-spec certs(Certificates) -> cell()
    when
        Certificates :: [term()].
certs(Certificates) ->
    cell(0, certs, Certificates).

-spec auth_challenge(Challenge, Methods) -> cell()
    when
        Challenge :: binary(),
        Methods   :: [term()].
auth_challenge(Challenge, Methods) ->
    cell(0, auth_challenge, #{ challenge => Challenge, methods => Methods }).

-spec authenticate(AuthType, Auth) -> cell()
    when
        AuthType :: non_neg_integer(),
        Auth     :: binary().
authenticate(AuthType, Auth) ->
    cell(0, authenticate, #{ auth_type => AuthType, auth => Auth }).

-spec authorize() -> cell().
authorize() ->
    cell(0, authorize, []).

-spec encode(Version, Cell) -> {ok, Data} | {error, Reason}
    when
        Version         :: onion_protocol:version(),
        Cell            :: cell(),
        Data            :: iolist(),
        Reason          :: term().
encode(Version, Cell) ->
    try
        Circuit = encode_circuit(Version, maps:get(circuit, Cell, 0)),
        CommandAtom = maps:get(command, Cell),
        Command = command_to_integer(CommandAtom),
        Payload = encode_payload(CommandAtom, maps:get(payload, Cell)),
        PayloadSize = iolist_size(Payload),
        case Command of
            N when N =:= 7 orelse N >= 128 ->
                {ok, [Circuit, <<Command:8/integer, PayloadSize:16/integer>>, Payload]};

            _ ->
                ZeroPaddingSize = (509 - PayloadSize) * 8,
                {ok, [Circuit, <<Command:8/integer>>, Payload, <<0:ZeroPaddingSize>>]}
        end
    catch X:Y ->
        io:format("~p~p~n", [X, Y]),
        {error, invalid_cell}
    end.

-spec decode(Version, Data) -> {ok, Cell, Continuation} | {error, Reason}
    when
        Version      :: onion_protocol:version(),
        Data         :: binary(),
        Cell         :: cell(),
        Continuation :: binary(),
        Reason       :: term().
decode(Version, Data) ->
    try
        case {Version, Data} of
            {4, <<Circuit:32/integer, Command:8/integer, Size:16/integer, Payload:Size/binary, Rest/binary>>} when Command =:= 7 orelse Command >= 128 ->
                {ok, decode_cell(Circuit, Command, Payload), Rest};

            {4, <<Circuit:32/integer, Command:8/integer, Payload:509/binary, Rest/binary>>} ->
                {ok, decode_cell(Circuit, Command, Payload), Rest};

            {3, <<Circuit:16/integer, Command:8/integer, Size:16/integer, Payload:Size/binary, Rest/binary>>} when Command =:= 7 orelse Command >= 128 ->
                {ok, decode_cell(Circuit, Command, Payload), Rest};

            {3, <<Circuit:16/integer, Command:8/integer, Payload:509/binary, Rest/binary>>} ->
                {ok, decode_cell(Circuit, Command, Payload), Rest};

            {_, _} ->
                {error, insufficient_data}
        end
    catch _:_ ->
        {error, invalid_cell}
    end.

%% @private
-spec cell(Circuit, Command, Payload) -> cell()
    when
        Circuit :: circuit(),
        Command :: command(),
        Payload :: term().
cell(Circuit, Command, Payload) ->
    #{ circuit => Circuit,
       command => Command,
       payload => Payload }.

%% @private
-spec decode_cell(Circuit, Command, Payload) -> Cell
    when
        Circuit :: circuit(),
        Command :: 0 .. 255,
        Payload :: binary(),
        Cell    :: cell().
decode_cell(Circuit, Command, Payload) ->
    CommandAtom = integer_to_command(Command),
    #{ circuit => Circuit,
       command => CommandAtom,
       payload => decode_payload(CommandAtom, Payload) }.

%% @private
-spec decode_payload(Command, Payload) -> term()
    when
        Command :: command(),
        Payload :: binary().
decode_payload(padding, Payload) ->
    #{ size => byte_size(Payload) };

decode_payload(create, <<"ntorNTORntorNTOR", Data:(?TAP_C_HANDSHAKE_LEN - 16)/binary, _/binary>>) ->
    #{ type => ntor, data => Data };

decode_payload(create, <<Data:(?TAP_C_HANDSHAKE_LEN)/binary, _/binary>>) ->
    #{ type => tap, data => Data };

decode_payload(created, <<Data:(?TAP_S_HANDSHAKE_LEN)/binary, _/binary>>) ->
    #{ data => Data };

decode_payload(relay, <<Command:8/integer, Recognized:16/integer, StreamID:16/integer, Digest:32/integer, Size:16/integer, Data:(?PAYLOAD_LEN - 11)/binary, _/binary>>) ->
    #{ command    => integer_to_relay_command(Command),
       recognized => Recognized,
       stream     => StreamID,
       digest     => Digest,
       size       => Size,
       data       => Data };

decode_payload(destroy, <<Reason:8/integer, _/binary>>) ->
    #{ reason => integer_to_error_code(Reason) };

decode_payload(create_fast, <<KeyMaterial:(?HASH_LEN)/binary, _>>) ->
    #{ key_material => KeyMaterial };

decode_payload(created_fast, <<KeyMaterial:(?HASH_LEN)/binary, DerivativeKeyData:(?HASH_LEN)/binary, _/binary>>) ->
    #{ key_material        => KeyMaterial,
       derivative_key_data => DerivativeKeyData };

decode_payload(versions, Payload) ->
    [ Version || <<Version:16/integer>> <= Payload ];

decode_payload(netinfo, <<Timestamp:32/integer, Payload/binary>>) ->
    try
        {ok, TargetAddress, <<SourceAddressCount:8/integer, Rest/binary>>} = decode_address(Payload),
        {ok, SourceAddresses, _} = decode_address_list(SourceAddressCount, Rest),
        #{ timestamp        => Timestamp,
           target_address   => TargetAddress,
           source_addresses => SourceAddresses }
    catch _:_ ->
        throw({error, invalid_netinfo_payload})
    end;

decode_payload(relay_early, Data) ->
    #{ data => Data };

decode_payload(create2, <<Type:16/integer, Size:16/integer, Data:Size/binary, _/binary>>) ->
    case Type of
        0 ->
            #{ type => tap, data => Data };
        2 ->
            #{ type => ntor, data => Data };
        _ ->
            #{ type => {unknown, Type}, data => Data }
    end;

decode_payload(created2, <<Size:16/integer, Data:Size/binary, _/binary>>) ->
    #{ data => Data };

decode_payload(vpadding, Payload) ->
    #{ size => byte_size(Payload) };

decode_payload(certs, <<CertCount:8/integer, Rest/binary>>) ->
    Certs = [ #{ type => Type, cert => Cert } || <<Type:8/integer, CertSize:16/integer, Cert:CertSize/binary>> <= Rest ],
    case length(Certs) of
        CertCount ->
            Certs;

        _ ->
            throw(invalid_certs_size)
    end;

decode_payload(auth_challenge, <<Challenge:32/binary, MethodsCount:16/integer, Rest/binary>>) ->
    MethodsByteCount = MethodsCount * 2,
    <<Methods:MethodsByteCount/binary, _/binary>> = Rest,
    #{ challenge => Challenge,
       methods   => [begin
                         case Method of
                             1 -> {rsa, sha256, tls_secret};
                             _ -> {unknown_method, Method}
                         end
                     end || <<Method:16/integer>> <= Methods ] };

decode_payload(authenticate, <<AuthType:16/integer, AuthSize:16/integer, Auth:AuthSize/binary>>) ->
    #{ auth_type => AuthType,
       auth      => Auth };

decode_payload(authorize, _) ->
    #{};

decode_payload(_, Payload) ->
    Payload.

%% @private
-spec integer_to_command(Integer) -> Command | {unknown_cell_command, Integer}
    when
        Integer :: non_neg_integer(),
        Command :: command().
integer_to_command(Integer) when is_integer(Integer) ->
    case Integer of
        0   -> padding;
        1   -> create;
        2   -> created;
        3   -> relay;
        4   -> destroy;
        5   -> create_fast;
        6   -> created_fast;
        7   -> versions;
        8   -> netinfo;
        9   -> relay_early;
        10  -> create2;
        11  -> created2;
        128 -> vpadding;
        129 -> certs;
        130 -> auth_challenge;
        131 -> authenticate;
        132 -> authorize;
        _   -> {unknown_cell_command, Integer}
    end.

%% @private
-spec decode_address(Payload) -> {ok, term(), Rest}
    when
        Payload :: binary(),
        Rest    :: binary().
decode_address(<<4, 4, A:8/integer, B:8/integer, C:8/integer, D:8/integer, Rest/binary>>) ->
    {ok, {A, B, C, D}, Rest};

decode_address(<<6, 16, A:16/integer, B:16/integer, C:16/integer, D:16/integer, E:16/integer, F:16/integer, G:16/integer, H:16/integer, Rest/binary>>) ->
    {ok, {A, B, C, D, E, F, G, H}, Rest};

decode_address(<<Type:8/integer, Size:8/integer, Data:Size/binary, Rest/binary>>) ->
    {ok, {unknown_address, Type, Data}, Rest}.

%% @private
-spec decode_address_list(N, Data) -> {ok, [term()], Rest}
    when
        N    :: non_neg_integer(),
        Data :: binary(),
        Rest :: binary().
decode_address_list(N, Data) ->
    decode_address_list(N, Data, []).

decode_address_list(0, Data, List) ->
    {ok, lists:reverse(List), Data};

decode_address_list(N, Data, List) ->
    {ok, Address, Rest} = decode_address(Data),
    decode_address_list(N - 1, Rest, [Address | List]).

-spec encode_payload(Command, Payload) -> iolist()
    when
        Command :: command(),
        Payload :: term().
encode_payload(padding, _Payload) ->
    [];

encode_payload(create, #{ type := Type, data := Data}) ->
    case Type of
        ntor when byte_size(Data) =:= (?TAP_C_HANDSHAKE_LEN - 16) ->
            [<<"ntorNTORntorNTOR">>, Data];

        tap when byte_size(Data) =:= ?TAP_C_HANDSHAKE_LEN ->
            [Data]
    end;

encode_payload(created, #{ data := Data }) when byte_size(Data) =:= ?TAP_S_HANDSHAKE_LEN ->
    [Data];

encode_payload(relay, #{ command := CommandAtom, recognized := Recognized, stream := StreamID, digest := Digest, data := Data }) ->
    Command = relay_command_to_integer(CommandAtom),
    Size = byte_size(Data),
    [<<Command:8/integer, Recognized:16/integer, StreamID:16/integer, Digest:32/integer, Size:16/integer, Data/binary>>];

encode_payload(destroy, #{ reason := ReasonAtom }) ->
    Reason = error_code_to_integer(ReasonAtom),
    [<<Reason:8/integer>>];

encode_payload(create_fast, #{ key_material := KeyMaterial }) when byte_size(KeyMaterial) =:= ?HASH_LEN ->
    [KeyMaterial];

encode_payload(created_fast, #{ key_material := KeyMaterial, derivative_key_data := DerivativeKeyData }) ->
    [KeyMaterial, DerivativeKeyData];

encode_payload(versions, Versions) ->
    [<<Version:16/integer>> || Version <- Versions];

encode_payload(netinfo, #{ timestamp := Timestamp, target_address := TargetAddress, source_addresses := SourceAddresses }) ->
    Length = length(SourceAddresses),
    [<<Timestamp:32/integer>>, encode_address(TargetAddress), <<Length:8/integer>>, lists:map(fun encode_address/1, SourceAddresses)];

encode_payload(relay_early, #{ data := Data }) ->
    [Data];

encode_payload(create2, #{ type := Type, data := Data }) ->
    Size = byte_size(Data),
    case Type of
        tap ->
            [<<0:16/integer, Size:16/integer, Data/binary>>];
        ntor ->
            [<<1:16/integer, Size:16/integer, Data/binary>>]
    end;

encode_payload(created2, #{ data := Data }) ->
    Size = byte_size(Data),
    [<<Size:16/integer, Data/binary>>];

encode_payload(vpadding, _Payload) ->
    [];

encode_payload(certs, Certificates) ->
    Count = length(Certificates),
    [<<Count:8/integer>>, lists:map(fun (#{ type := Type, cert := Cert }) ->
                                        CertSize = byte_size(Cert),
                                        <<Type:8/integer, CertSize:16/integer, Cert/binary>>
                                    end, Certificates)];

encode_payload(auth_challenge, #{ challenge := Challenge, methods := Methods }) ->
    EncodedMethods = lists:map(fun (Method) ->
                                   case Method of
                                       {rsa, sha256, tls_secret} ->
                                           <<1:16/integer>>
                                   end
                               end, Methods),
    MethodsCount = length(EncodedMethods),
    [Challenge, <<MethodsCount:16/integer>>, EncodedMethods];

encode_payload(authenticate, #{ auth_type := AuthType, auth := Auth }) ->
    AuthSize = byte_size(Auth),
    [<<AuthType:16/integer, AuthSize:16/integer, Auth/binary>>];

encode_payload(authorize, _Payload) ->
    [].

%% @private
-spec encode_circuit(Version, Circuit) -> binary()
    when
        Version :: onion_protocol:version(),
        Circuit :: circuit().
encode_circuit(3, Circuit) ->
    <<Circuit:16/integer>>;

encode_circuit(4, Circuit) ->
    <<Circuit:32/integer>>.

%% @private
-spec encode_address(Address) -> iolist()
    when
        Address :: inet:ip_address().
encode_address({A, B, C, D}) ->
    [<<4, 4, A:8/integer, B:8/integer, C:8/integer, D:8/integer>>];

encode_address({A, B, C, D, E, F, G, H}) ->
    [<<6, 16, A:16/integer, B:16/integer, C:16/integer, D:16/integer, E:16/integer, F:16/integer, G:16/integer, H:16/integer>>].

%% @private
-spec command_to_integer(Command) -> Integer | {unknown_cell_command, Command}
    when
        Command :: command(),
        Integer :: non_neg_integer().
command_to_integer(Command) ->
    case Command of
        padding        -> 0;
        create         -> 1;
        created        -> 2;
        relay          -> 3;
        destroy        -> 4;
        create_fast    -> 5;
        created_fast   -> 6;
        versions       -> 7;
        netinfo        -> 8;
        relay_early    -> 9;
        create2        -> 10;
        created2       -> 11;
        vpadding       -> 128;
        certs          -> 129;
        auth_challenge -> 130;
        authenticate   -> 131;
        authorize      -> 132;
        _              -> {unknown_cell_command, Command}
    end.

%% @private
-spec integer_to_relay_command(Integer) -> RelayCommand | {unknown_relay_command, Integer}
    when
        RelayCommand :: relay_command(),
        Integer      :: non_neg_integer().
integer_to_relay_command(Integer) ->
    case Integer of
        1  -> relay_begin;
        2  -> relay_data;
        3  -> relay_end;
        4  -> relay_connected;
        5  -> relay_sendme;
        6  -> relay_extend;
        7  -> relay_extended;
        8  -> relay_truncate;
        9  -> relay_truncated;
        10 -> relay_drop;
        11 -> relay_resolve;
        12 -> relay_resolved;
        13 -> relay_begin_dir;
        14 -> relay_extend2;
        15 -> relay_extended2;
        _  -> {unknown_relay_command, Integer}
    end.

-spec relay_command_to_integer(RelayCommand) -> Integer | {unknown_relay_command, RelayCommand}
    when
        Integer      :: non_neg_integer(),
        RelayCommand :: relay_command().
relay_command_to_integer(Command) ->
    case Command of
        relay_begin     -> 1;
        relay_data      -> 2;
        relay_end       -> 3;
        relay_connected -> 4;
        relay_sendme    -> 5;
        relay_extend    -> 6;
        relay_extended  -> 7;
        relay_truncate  -> 8;
        relay_truncated -> 9;
        relay_drop      -> 10;
        relay_resolve   -> 11;
        relay_resolved  -> 12;
        relay_begin_dir -> 13;
        relay_extend2   -> 14;
        relay_extended2 -> 15;
        _  -> {unknown_relay_command, Command}
    end.

%% @private
-spec integer_to_error_code(Integer) -> ErrorCode | {unknown_error_code, Integer}
    when
        ErrorCode :: error_code(),
        Integer   :: non_neg_integer().
integer_to_error_code(Integer) ->
    case Integer of
        0  -> none;
        1  -> protocol;
        2  -> internal;
        3  -> requested;
        4  -> hibernating;
        5  -> resource_limit;
        6  -> connect_failed;
        7  -> or_identity;
        8  -> or_connection_closed;
        9  -> finished;
        10 -> timeout;
        11 -> destroyed;
        12 -> no_such_service;
        _  -> {unknown_error_code, Integer}
    end.

%% @private
-spec error_code_to_integer(ErrorCode) -> Integer | {unknown_error_code, ErrorCode}
    when
        Integer   :: non_neg_integer(),
        ErrorCode :: error_code().
error_code_to_integer(ErrorCode) ->
    case ErrorCode of
        none                 -> 0;
        protocol             -> 1;
        internal             -> 2;
        requested            -> 3;
        hibernating          -> 4;
        resource_limit       -> 5;
        connect_failed       -> 6;
        or_identity          -> 7;
        or_connection_closed -> 8;
        finished             -> 9;
        timeout              -> 10;
        destroyed            -> 11;
        no_such_service      -> 12;
        _                    -> {unknown_error_code, ErrorCode}
    end.

-ifdef(TEST).
decode_v3_cell_test() ->
    [
        ?assertEqual(decode(3, <<0:16, 127:8, 0:4072>>),           {ok, #{ circuit => 0, command => {unknown_cell_command, 127}, payload => <<0:4072>> }, <<>>}),
        ?assertEqual(decode(3, <<0:16, 127:8, 0:4072, "foobar">>), {ok, #{ circuit => 0, command => {unknown_cell_command, 127}, payload => <<0:4072>> }, <<"foobar">>})
    ].

decode_v3_cell_insufficient_data_test() ->
    [
        ?assertEqual(decode(3, <<>>),         {error, insufficient_data}),
        ?assertEqual(decode(3, <<0:4095>>),   {error, insufficient_data}),
        ?assertEqual(decode(3, <<"foobar">>), {error, insufficient_data})
    ].

decode_v3_cell_version_test() ->
    [
        ?assertEqual(decode(3, <<0:16, 7:8, 8:16, 1:16, 2:16, 3:16, 4:16, "foobar">>), {ok, #{ circuit => 0, command => versions, payload => [1, 2, 3, 4] }, <<"foobar">>}),
        ?assertEqual(decode(3, <<0:16, 7:8, 0:16, "foobar">>), {ok, #{ circuit => 0, command => versions, payload => [] }, <<"foobar">>})
    ].

decode_v4_cell_test() ->
    [
        ?assertEqual(decode(4, <<0:32, 127:8, 0:4072>>),           {ok, #{ circuit => 0, command => {unknown_cell_command, 127}, payload => <<0:4072>> }, <<>>}),
        ?assertEqual(decode(4, <<0:32, 127:8, 0:4072, "foobar">>), {ok, #{ circuit => 0, command => {unknown_cell_command, 127}, payload => <<0:4072>> }, <<"foobar">>})
    ].

decode_v4_cell_insufficient_data_test() ->
    [
        ?assertEqual(decode(4, <<>>),         {error, insufficient_data}),
        ?assertEqual(decode(4, <<0:4095>>),   {error, insufficient_data}),
        ?assertEqual(decode(4, <<"foobar">>), {error, insufficient_data})
    ].
-endif.
