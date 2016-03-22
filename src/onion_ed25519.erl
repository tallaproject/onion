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
         public_key_from_secret_key/1,
         public_key_from_x25519_public_key/2,
         keypair_from_x25519_keypair/1
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

%% 2^{(P - 1) / 4} mod P.
-define(I, 19681161376707505956807079304988542015446066515923890162744021073123829784752).

%% recover_x(BASE_Y).
-define(BASE_X, 15112221349535400772501151409588531511454012693041857206046113283949847762202).

%% 4 * inv(8).
-define(BASE_Y, 46316835694926478169428394003475163141307993866256225615783033603165251855960).

%% {?BASE_X mod P, ?BASE_Y mod P}.
-define(BASE, {?BASE_X, ?BASE_Y}).

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
    enacl:crypto_sign_ed25519_keypair().

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

%% @doc Compute the public key from a given secret key.
%%
%% Computes an Ed 25519 public key from a given secret key.
%%
%% @end
-spec public_key_from_secret_key(SecretKey) -> PublicKey
    when
        SecretKey :: secret_key(),
        PublicKey :: public_key().
public_key_from_secret_key(SecretKey) ->
    H = SecretKey,
    Value = onion_math:pow(2, ?B - 2) + lists:sum([onion_math:pow(2, I) * onion_binary:bit(H, I) || I <- lists:seq(3, ?B - 3)]),
    A = scalarmult(?BASE, Value),
    point_encode(A).

%% @doc Return the matching Ed25519 public key from an x25519 public key.
%% @end
-spec public_key_from_x25519_public_key(X25519PublicKey, SignBit) -> PublicKey
    when
        X25519PublicKey :: onion_x25519:public_key(),
        SignBit         :: 0 | 1,
        PublicKey       :: public_key().
public_key_from_x25519_public_key(X25519PublicKey, SignBit) ->
    {X, U} = point_decode(X25519PublicKey),

    UMinusOne = onion_math:mod(U - 1, ?P),
    UPlusOne  = onion_math:mod(U + 1, ?P),
    UInverse  = inv(UPlusOne),

    Y = onion_math:mod(UMinusOne * UInverse, ?P),

    <<Encoded:31/binary, Byte:8/integer>> = point_encode({X, Y}),
    <<Encoded/binary, (Byte bor (SignBit bsl 7)):8/integer>>.

%% @doc Return the matching Ed25519 keypair from an x25519 keypair.
%% @end
-spec keypair_from_x25519_keypair(X25519KeyPair) -> KeyPair
    when
        X25519KeyPair  :: onion_x25519:keypair(),
        KeyPair        :: keypair().
keypair_from_x25519_keypair(#{ secret := X25519SecretKey, public := X25519PublicKey }) ->
    <<Hash:32/binary, _:32/binary>> = crypto:hash(sha512, [X25519SecretKey,
                                                           <<"Derive high part of ed25519 key from curve25519 key", 0>>]),
    SecretKey = <<X25519SecretKey/binary, Hash/binary>>,
    PublicKey = public_key_from_secret_key(SecretKey),

    PublicKeyCheck = public_key_from_x25519_public_key(X25519PublicKey, binary:at(PublicKey, 31) bsr 7),
    PublicKey = PublicKeyCheck,

    #{ public => PublicKey, secret => SecretKey }.

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
    B = onion_binary:bit(P, 255),
    case X band 0 of
        B ->
            {X, Y};

        _ ->
            {?P - X, Y}
    end.

%% @private
-spec recover_x(Y) -> X
    when
        Y :: number(),
        X :: number().
recover_x(Y) ->
    XX = (Y * Y - 1) * inv(?D * Y * Y + 1),
    X  = onion_math:mod_pow(XX, (?P + 3) div 8, ?P),
    X2 = case onion_math:mod(X * X - XX, ?P) of
             0 ->
                 X;

             _ ->
                 onion_math:mod(X * ?I, ?P)
         end,
    case onion_math:mod(X2, 2) of
        0 ->
            X2;

        _ ->
            ?P - X2
    end.

%% @private
inv(V) ->
    onion_math:mod_pow(V, ?P - 2, ?P).

%% @private
-spec edwards(PointA, PointB) -> PointC
    when
        PointA :: point(),
        PointB :: point(),
        PointC :: point().
edwards({X1, Y1}, {X2, Y2}) ->
    X3 = (X1 * Y2 + X2 * Y1) * inv(1 + ?D * X1 * X2 * Y1 * Y2),
    Y3 = (Y1 * Y2 + X1 * X2) * inv(1 - ?D * X1 * X2 * Y1 * Y2),
    {onion_math:mod(X3, ?P), onion_math:mod(Y3, ?P)}.

%% @private
-spec scalarmult(Point, Scalar) -> NewPoint
    when
        Point    :: point(),
        Scalar   :: non_neg_integer(),
        NewPoint :: point().
scalarmult(_Point, 0) ->
    {0, 1};

scalarmult(Point, B) ->
    Q1 = scalarmult(Point, B div 2),
    Q2 = edwards(Q1, Q1),
    case B band 1 of
        0 ->
            Q2;

        1 ->
            edwards(Q2, Point)
    end.

-ifdef(TEST).
constants_test() ->
    [
        ?assertEqual(?P, onion_math:pow(2, 255) - 19),
        ?assertEqual(?D, -121665 * inv(121666)),
        ?assertEqual(?I, onion_math:mod_pow(2, (?P - 1) div 4, ?P)),
        ?assertEqual(?B, bit_size(<<0:256>>)),
        ?assertEqual(?BASE_X, recover_x(?BASE_Y)),
        ?assertEqual(?BASE_Y, 4 * inv(5)),
        ?assertEqual(?BASE, {onion_math:mod(?BASE_X, ?P), onion_math:mod(?BASE_Y, ?P)})
    ].

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

x25519_keypair_conversion_1_test() ->
    X25519KeyPair = #{ secret => base16_decode("70CFF2118989237E0D923D2CD404F3E51F785E0687FC63A3A74B21985666E77E"),
                       public => base16_decode("36ECF106998BF6EF5DAC2A630DD2A8FE4F90625090E5D32D2EE5E65375347C25") },
    Ed25519KeyPair = #{ secret => base16_decode("70CFF2118989237E0D923D2CD404F3E51F785E0687FC63A3A74B21985666E77E68CC3C08312ADD636FF497B329BF5E37D7419A4FED50A0C9C79CF73357845F58"),
                        public => base16_decode("851B859F1EC9E2C87F36F943EFBC48BF7BBEEABA6ED3C2510C9AA95530F0678F") },
    SignBit = 1,
    [
        ?assertEqual(Ed25519KeyPair, keypair_from_x25519_keypair(X25519KeyPair)),
        ?assertEqual(maps:get(public, Ed25519KeyPair), public_key_from_x25519_public_key(maps:get(public, X25519KeyPair), SignBit))
    ].

x25519_keypair_conversion_2_test() ->
    X25519KeyPair = #{ secret => base16_decode("3840718865390DA1E93AF12E3293D57BBCC1E4F529C30B9B62F0E1ABEBD02157"),
                       public => base16_decode("453A6975E5E2E18FB248C3A5AC8163B41131D346BB95C12B313F3CBFD063E858") },
    Ed25519KeyPair = #{ secret => base16_decode("3840718865390DA1E93AF12E3293D57BBCC1E4F529C30B9B62F0E1ABEBD021572B1CDC671ECB8D3BDABB1109DF9E8F3221EE5882DE6848836BCA67A7C94E9B2D"),
                        public => base16_decode("6BEF0C559B20AAD38F59B7111F46C0E17A12686D36F9F6153D5F32441CD33112") },
    SignBit = 0,
    [
        ?assertEqual(Ed25519KeyPair, keypair_from_x25519_keypair(X25519KeyPair)),
        ?assertEqual(maps:get(public, Ed25519KeyPair), public_key_from_x25519_public_key(maps:get(public, X25519KeyPair), SignBit))
    ].

-endif. %% TEST.
