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
         secret_key_expand/1,
         sign/2,
         open/3,
         public_key_from_secret_key/1,
         public_key_from_x25519_public_key/2,
         keypair_from_x25519_keypair/1
        ]).

%% Types.
-export_type([secret_key/0,
              public_key/0,
              keypair/0
             ]).

-type secret_key() :: ed25519_ref10:secret_key().
-type public_key() :: ed25519_ref10:public_key().
-type keypair()    :: ed25519_ref10:keypair().

-include("onion_test.hrl").

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
    ed25519_ref10:keypair().

%% @doc Generate a new Ed25519 from a given seed.
%%
%% Generates and returns a new Ed25519 secret key.
%%
%% @end
-spec secret_key_expand(Seed) -> secret_key()
    when
        Seed :: binary().
secret_key_expand(Seed) ->
    ed25519_ref10:secret_key_expand(Seed).

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
    ed25519_ref10:sign(Message, SecretKey).


%% @doc Verify a Signature of a Message using the PublicKey
%%
%% Verifies a given Signature and Message using the given PublicKey.
%%
%% @end
-spec open(Message, Signature, PublicKey) -> boolean()
    when
        Signature :: binary(),
        Message   :: binary(),
        PublicKey :: public_key().
open(Signature, Message, PublicKey) ->
    ed25519_ref10:open(Signature, Message, PublicKey).

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
    ed25519_ref10:public_key(SecretKey).

%% @doc Return the matching Ed25519 public key from an x25519 public key.
%% @end
-spec public_key_from_x25519_public_key(X25519PublicKey, SignBit) -> PublicKey
    when
        X25519PublicKey :: onion_x25519:public_key(),
        SignBit         :: 0 | 1,
        PublicKey       :: public_key().
public_key_from_x25519_public_key(X25519PublicKey, SignBit) ->
    ed25519_ref10:public_key_from_x25519_public_key(X25519PublicKey, SignBit).

%% @doc Return the matching Ed25519 keypair from an x25519 keypair.
%% @end
-spec keypair_from_x25519_keypair(X25519KeyPair) -> {KeyPair, SignBit}
    when
        X25519KeyPair  :: onion_x25519:keypair(),
        KeyPair        :: keypair(),
        SignBit        :: 0 | 1.
keypair_from_x25519_keypair(X25519KeyPair) ->
    ed25519_ref10:keypair_from_x25519_keypair(X25519KeyPair).

-ifdef(TEST).

%% From: 7.1. Test Vectors for Ed25519 (draft-irtf-cfrg-eddsa)
ed25519_sign_draft_irtf_cfrg_eddsa_1_test() ->
    SecretKey = secret_key_expand(base16_decode(["9d61b19deffd5a60ba844af492ec2cc4",
                                                 "4449c5697b326919703bac031cae7f60"])),
    PublicKey = base16_decode(["d75a980182b10ab7d54bfed3c964073a",
                               "0ee172f3daa62325af021a68f707511a"]),
    Message   = <<>>,
    Signature = base16_decode(["e5564300c360ac729086e2cc806e828a",
                               "84877f1eb8e5d974d873e06522490155",
                               "5fb8821590a33bacc61e39701cf9b46b",
                               "d25bf5f0595bbe24655141438e7a100b"]),
    [
        ?assertEqual(0, byte_size(Message)),
        ?assertEqual(PublicKey, public_key_from_secret_key(SecretKey)),
        ?assertEqual(Signature, sign(Message, SecretKey)),
        ?assert(open(Signature, Message, PublicKey))
    ].

ed25519_sign_draft_irtf_cfrg_eddsa_2_test() ->
    SecretKey = secret_key_expand(base16_decode(["4ccd089b28ff96da9db6c346ec114e0f",
                                                 "5b8a319f35aba624da8cf6ed4fb8a6fb"])),
    PublicKey = base16_decode(["3d4017c3e843895a92b70aa74d1b7ebc",
                               "9c982ccf2ec4968cc0cd55f12af4660c"]),
    Message   = base16_decode(["72"]),
    Signature = base16_decode(["92a009a9f0d4cab8720e820b5f642540",
                               "a2b27b5416503f8fb3762223ebdb69da",
                               "085ac1e43e15996e458f3613d0f11d8c",
                               "387b2eaeb4302aeeb00d291612bb0c00"]),
    [
        ?assertEqual(1, byte_size(Message)),
        ?assertEqual(PublicKey, public_key_from_secret_key(SecretKey)),
        ?assertEqual(Signature, sign(Message, SecretKey)),
        ?assert(open(Signature, Message, PublicKey))
    ].

ed25519_sign_draft_irtf_cfrg_eddsa_3_test() ->
    SecretKey = secret_key_expand(base16_decode(["c5aa8df43f9f837bedb7442f31dcb7b1",
                                                 "66d38535076f094b85ce3a2e0b4458f7"])),
    PublicKey = base16_decode(["fc51cd8e6218a1a38da47ed00230f058",
                               "0816ed13ba3303ac5deb911548908025"]),
    Message   = base16_decode(["af82"]),
    Signature = base16_decode(["6291d657deec24024827e69c3abe01a3",
                               "0ce548a284743a445e3680d7db5ac3ac",
                               "18ff9b538d16f290ae67f760984dc659",
                               "4a7c15e9716ed28dc027beceea1ec40a"]),
    [
        ?assertEqual(2, byte_size(Message)),
        ?assertEqual(PublicKey, public_key_from_secret_key(SecretKey)),
        ?assertEqual(Signature, sign(Message, SecretKey)),
        ?assert(open(Signature, Message, PublicKey))
    ].

ed25519_sign_draft_irtf_cfrg_eddsa_4_test() ->
    SecretKey = secret_key_expand(base16_decode(["f5e5767cf153319517630f226876b86c",
                                                 "8160cc583bc013744c6bf255f5cc0ee5"])),
    PublicKey = base16_decode(["278117fc144c72340f67d0f2316e8386",
                               "ceffbf2b2428c9c51fef7c597f1d426e"]),
    Message   = base16_decode(["08b8b2b733424243760fe426a4b54908",
                               "632110a66c2f6591eabd3345e3e4eb98",
                               "fa6e264bf09efe12ee50f8f54e9f77b1",
                               "e355f6c50544e23fb1433ddf73be84d8",
                               "79de7c0046dc4996d9e773f4bc9efe57",
                               "38829adb26c81b37c93a1b270b20329d",
                               "658675fc6ea534e0810a4432826bf58c",
                               "941efb65d57a338bbd2e26640f89ffbc",
                               "1a858efcb8550ee3a5e1998bd177e93a",
                               "7363c344fe6b199ee5d02e82d522c4fe",
                               "ba15452f80288a821a579116ec6dad2b",
                               "3b310da903401aa62100ab5d1a36553e",
                               "06203b33890cc9b832f79ef80560ccb9",
                               "a39ce767967ed628c6ad573cb116dbef",
                               "efd75499da96bd68a8a97b928a8bbc10",
                               "3b6621fcde2beca1231d206be6cd9ec7",
                               "aff6f6c94fcd7204ed3455c68c83f4a4",
                               "1da4af2b74ef5c53f1d8ac70bdcb7ed1",
                               "85ce81bd84359d44254d95629e9855a9",
                               "4a7c1958d1f8ada5d0532ed8a5aa3fb2",
                               "d17ba70eb6248e594e1a2297acbbb39d",
                               "502f1a8c6eb6f1ce22b3de1a1f40cc24",
                               "554119a831a9aad6079cad88425de6bd",
                               "e1a9187ebb6092cf67bf2b13fd65f270",
                               "88d78b7e883c8759d2c4f5c65adb7553",
                               "878ad575f9fad878e80a0c9ba63bcbcc",
                               "2732e69485bbc9c90bfbd62481d9089b",
                               "eccf80cfe2df16a2cf65bd92dd597b07",
                               "07e0917af48bbb75fed413d238f5555a",
                               "7a569d80c3414a8d0859dc65a46128ba",
                               "b27af87a71314f318c782b23ebfe808b",
                               "82b0ce26401d2e22f04d83d1255dc51a",
                               "ddd3b75a2b1ae0784504df543af8969b",
                               "e3ea7082ff7fc9888c144da2af58429e",
                               "c96031dbcad3dad9af0dcbaaaf268cb8",
                               "fcffead94f3c7ca495e056a9b47acdb7",
                               "51fb73e666c6c655ade8297297d07ad1",
                               "ba5e43f1bca32301651339e22904cc8c",
                               "42f58c30c04aafdb038dda0847dd988d",
                               "cda6f3bfd15c4b4c4525004aa06eeff8",
                               "ca61783aacec57fb3d1f92b0fe2fd1a8",
                               "5f6724517b65e614ad6808d6f6ee34df",
                               "f7310fdc82aebfd904b01e1dc54b2927",
                               "094b2db68d6f903b68401adebf5a7e08",
                               "d78ff4ef5d63653a65040cf9bfd4aca7",
                               "984a74d37145986780fc0b16ac451649",
                               "de6188a7dbdf191f64b5fc5e2ab47b57",
                               "f7f7276cd419c17a3ca8e1b939ae49e4",
                               "88acba6b965610b5480109c8b17b80e1",
                               "b7b750dfc7598d5d5011fd2dcc5600a3",
                               "2ef5b52a1ecc820e308aa342721aac09",
                               "43bf6686b64b2579376504ccc493d97e",
                               "6aed3fb0f9cd71a43dd497f01f17c0e2",
                               "cb3797aa2a2f256656168e6c496afc5f",
                               "b93246f6b1116398a346f1a641f3b041",
                               "e989f7914f90cc2c7fff357876e506b5",
                               "0d334ba77c225bc307ba537152f3f161",
                               "0e4eafe595f6d9d90d11faa933a15ef1",
                               "369546868a7f3a45a96768d40fd9d034",
                               "12c091c6315cf4fde7cb68606937380d",
                               "b2eaaa707b4c4185c32eddcdd306705e",
                               "4dc1ffc872eeee475a64dfac86aba41c",
                               "0618983f8741c5ef68d3a101e8a3b8ca",
                               "c60c905c15fc910840b94c00a0b9d0"]),
    Signature = base16_decode(["0aab4c900501b3e24d7cdf4663326a3a",
                               "87df5e4843b2cbdb67cbf6e460fec350",
                               "aa5371b1508f9f4528ecea23c436d94b",
                               "5e8fcd4f681e30a6ac00a9704a188a03"]),
    [
        ?assertEqual(1023, byte_size(Message)),
        ?assertEqual(PublicKey, public_key_from_secret_key(SecretKey)),
        ?assertEqual(Signature, sign(Message, SecretKey)),
        ?assert(open(Signature, Message, PublicKey))
    ].

ed25519_sign_draft_irtf_cfrg_eddsa_5_test() ->
    SecretKey = secret_key_expand(base16_decode(["833fe62409237b9d62ec77587520911e",
                                                 "9a759cec1d19755b7da901b96dca3d42"])),
    PublicKey = base16_decode(["ec172b93ad5e563bf4932c70e1245034",
                               "c35467ef2efd4d64ebf819683467e2bf"]),
    Message   = base16_decode(["ddaf35a193617abacc417349ae204131",
                               "12e6fa4e89a97ea20a9eeee64b55d39a",
                               "2192992a274fc1a836ba3c23a3feebbd",
                               "454d4423643ce80e2a9ac94fa54ca49f"]),
    Signature = base16_decode(["dc2a4459e7369633a52b1bf277839a00",
                               "201009a3efbf3ecb69bea2186c26b589",
                               "09351fc9ac90b3ecfdfbc7c66431e030",
                               "3dca179c138ac17ad9bef1177331a704"]),
    [
        ?assertEqual(64, byte_size(Message)),
        ?assertEqual(PublicKey, public_key_from_secret_key(SecretKey)),
        ?assertEqual(Signature, sign(Message, SecretKey)),
        ?assert(open(Signature, Message, PublicKey))
    ].

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
        ?assertMatch({Ed25519KeyPair, SignBit}, keypair_from_x25519_keypair(X25519KeyPair)),
        ?assertEqual(maps:get(public, Ed25519KeyPair), public_key_from_x25519_public_key(maps:get(public, X25519KeyPair), SignBit))
    ].

x25519_keypair_conversion_2_test() ->
    X25519KeyPair = #{ secret => base16_decode("3840718865390DA1E93AF12E3293D57BBCC1E4F529C30B9B62F0E1ABEBD02157"),
                       public => base16_decode("453A6975E5E2E18FB248C3A5AC8163B41131D346BB95C12B313F3CBFD063E858") },
    Ed25519KeyPair = #{ secret => base16_decode("3840718865390DA1E93AF12E3293D57BBCC1E4F529C30B9B62F0E1ABEBD021572B1CDC671ECB8D3BDABB1109DF9E8F3221EE5882DE6848836BCA67A7C94E9B2D"),
                        public => base16_decode("6BEF0C559B20AAD38F59B7111F46C0E17A12686D36F9F6153D5F32441CD33112") },
    SignBit = 0,
    [
        ?assertMatch({Ed25519KeyPair, SignBit}, keypair_from_x25519_keypair(X25519KeyPair)),
        ?assertEqual(maps:get(public, Ed25519KeyPair), public_key_from_x25519_public_key(maps:get(public, X25519KeyPair), SignBit))
    ].

-endif. %% TEST.
