%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Ed25519 API
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_ed25519).

%% API.
-export([keypair/0,
         sign/2,
         verify/3,
         public_key_from_x25519_public_key/2
        ]).

%% Test API.
-export([point_on_curve/1,
         point_encode/1,
         point_decode/1
        ]).

%% Types.
-export_type([secret_key/0,
              public_key/0,
              keypair/0
             ]).

-type secret_key() :: binary().
-type public_key() :: binary().
-type keypair()    :: #{ secret => secret_key(), public => public_key() }.

-type point()      :: {number(), number()}.

-include("onion_test.hrl").

%% 2^255 - 19.
-define(P, 57896044618658097711785492504343953926634992332820282019728792003956564819949).

%% -121665 * (121666^{P - 2} mod P).
-define(D, -4513249062541557337682894930092624173785641285191125241628941591882900924598840740).

%% 2^{(P - 1) / 4} mod P
-define(I, 19681161376707505956807079304988542015446066515923890162744021073123829784752).

%% Number of bits.
-define(B, 256).

%% @doc Creates a new Ed25519 keypair.
%%
%% Generates and returns a new Ed25519 keypair. The return value is a map to
%% avoid using the public key as the secret key and vice versa.
%%
%% @end
-spec keypair() -> KeyPair
    when
        KeyPair :: keypair().
keypair() ->
    enacl:sign_keypair().

%% @doc Sign a given Message using a given SecretKey
%%
%% Returns a detached signature of a given Message using the given SecretKey.
%%
%% @end
-spec sign(Message, SecretKey) -> Signature
    when
        Message   :: iolist(),
        SecretKey :: secret_key(),
        Signature :: binary().
sign(Message, SecretKey) ->
    enacl:sign_detached(Message, SecretKey).


%% @doc Verify a Signature of a Message using the PublicKey
%%
%% Verifies a given Signature and Message using the given PublicKey.
%%
%% @end
-spec verify(Message, Signature, PublicKey) -> boolean()
    when
        Signature :: binary(),
        Message   :: binary(),
        PublicKey :: public_key().
verify(Signature, Message, PublicKey) ->
    case enacl:sign_verify_detached(Signature, Message, PublicKey) of
        {ok, Message} ->
            true;

        {error, failed_verification} ->
            false
    end.

%% @doc Return the matching Ed25519 public key from an x25519 public key.
%% @end
-spec public_key_from_x25519_public_key(X25519PublicKey, SignBit) -> Ed25519PublicKey
    when
        X25519PublicKey  :: onion_x25519:public_key(),
        SignBit          :: 0 | 1,
        Ed25519PublicKey :: public_key().
public_key_from_x25519_public_key(X25519PublicKey, SignBit) ->
    {X, U} = point_decode(X25519PublicKey),

    UMinusOne = onion_math:mod(U - 1, ?P),
    UPlusOne  = onion_math:mod(U + 1, ?P),
    UInverse  = onion_math:mod_pow(UPlusOne, ?P - 2, ?P),

    Y = onion_math:mod(UMinusOne * UInverse, ?P),

    <<Encoded:31/binary, Byte:8/integer>> = point_encode({X, Y}),
    <<Encoded/binary, (Byte bor (SignBit bsl 7)):8/integer>>.

%% @private
-spec point_on_curve(Point) -> boolean()
    when
        Point :: point().
point_on_curve({X, Y}) ->
    V = -X*X + Y*Y - 1 - ?D*X*X*Y*Y,
    onion_math:mod(V, ?P) =:= 0.

%% @private
-spec point_encode(Point) -> binary()
    when
        Point :: point().
point_encode({X, Y}) ->
    Bits = [(Y bsr I) band 1 || I <- lists:seq(0, 254)] ++ [X band 1],
    list_to_binary([lists:sum([lists:nth(I * 8 + J + 1, Bits) bsl J || J <- lists:seq(0, 7)]) || I <- lists:seq(0, ?B div 8 - 1)]).

%% @private
-spec point_decode(Data) -> point()
    when
        Data :: binary().
point_decode(P) ->
    Y = lists:sum([onion_math:pow(2, I) * onion_binary:bit(P, I) || I <- lists:seq(0, ?B - 2)]),
    X = recover_x(Y),
    case (X band 0) =/= onion_binary:bit(P, 255) of
        true ->
            {?P - X, Y};

        false ->
            {X, Y}
    end.

%% @private
-spec recover_x(Y) -> X
    when
        Y :: number(),
        X :: number().
recover_x(Y) ->
    XX = (Y*Y-1) * inv(?D*Y*Y+1),
    X  = onion_math:mod_pow(XX, (?P + 3) div 8, ?P),

    X2 = case onion_math:mod(X*X - XX, ?P) =/= 0 of
             true ->
                onion_math:mod(X*?I, ?P);
             false ->
                 X
         end,
    case onion_math:mod(X2, 2) =/= 0 of
        true ->
            ?P - X2;
        false ->
            X2
    end.

%% @private
inv(V) ->
    onion_math:mod_pow(V, ?P - 2, ?P).

-ifdef(TEST).
nacl_empty_test() ->
    SecretKey = base16_decode(["9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60",
                               "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"]),
    PublicKey = base16_decode("d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"),
    Message = <<>>,
    Signature = base16_decode(["e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e06522490155",
                               "5fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"]),
    [
        ?assertEqual(Signature, sign(Message, SecretKey)),
        ?assert(verify(Signature, Message, PublicKey))
    ].

nacl_72_test() ->
    SecretKey = base16_decode(["4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
                               "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"]),
    PublicKey = base16_decode("3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"),
    Message = base16_decode("72"),
    Signature = base16_decode(["92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da",
                               "085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00"]),
    [
        ?assertEqual(Signature, sign(Message, SecretKey)),
        ?assert(verify(Signature, Message, PublicKey))
    ].

point_decode_test() ->
    P = base16_decode("9E2224497084D8A5C052BD37F7DFAD36AC3D944E59EE726E535592B5674C96F3"),
    X = 56869039927154433150303327682297341023549405502822941135494550611620162867615,
    Y = 52281531975510977933525998162807435782741842954841616417264840253193893192350,
    [
        ?assertEqual({X, Y}, point_decode(P))
    ].

point_encode_test() ->
    R = base16_decode("9E2224497084D8A5C052BD37F7DFAD36AC3D944E59EE726E535592B5674C96F3"),
    P = point_decode(R),
    E = point_encode(P),
    [
        ?assertEqual(R, E)
    ].

point_on_curve_test() ->
    R = base16_decode("9E2224497084D8A5C052BD37F7DFAD36AC3D944E59EE726E535592B5674C96F3"),
    P = point_decode(R),
    ?assert(point_on_curve(P)).

x25519_public_key_conversion_test() ->
    [
        ?assertEqual(base16_decode("6FC3E6E155EB221A3AB21AD60A04242B0805F73B5923C8C308FA6A502277DACA"),
                     public_key_from_x25519_public_key(base16_decode("F0A8D229CF1861B19A9532244E2C65042AA36D5F121BC2449F51D1AAD30B3328"), 1)),
        ?assertEqual(base16_decode("FE948C40083AC46897E8C2BFB1FCE1A9461A420AA3F55A964C9C2F67774017B6"),
                     public_key_from_x25519_public_key(base16_decode("0A1F5E2C0D2A4A7FDD94DA0882FDA5A8CFB850FC987436B093A420368441AC15"), 1)),
        ?assertEqual(base16_decode("7135EE6C966ECDD771094ABDD0D4A71C2884513922DD4E8A3E4125CE10F6B3F4"),
                     public_key_from_x25519_public_key(base16_decode("524962C4A3E20558307D93175CD634B05F34F50291F168894B9EEC270B556D6F"), 1))
    ].

-endif. %% TEST.
