%%%
%%% Copyright (c) 2016 The Talla Authors. All rights reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% -----------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Key Derivation Functions used in Tor.
%%%
%%% For information about the HKDF function, see RFC 5869.
%%%
%%% @end
%%% -----------------------------------------------------------
-module(onion_kdf).

%% API.
-export([hkdf/4,
         kdf_tor/2
        ]).

-include("onion_test.hrl").

-define(HKDF_HASH_LENGTH, (256 / 8)).

%% @doc HMAC-based Extract-and-Expand Key Derivation Function.
%%
%% For more information, see RFC 5869.
%%
%% @end
-spec hkdf(Key, Salt, Info, Length) -> binary()
    when
        Key    :: iodata(),
        Salt   :: iodata(),
        Info   :: iodata(),
        Length :: pos_integer().
hkdf(Key, Salt, Info, Length) ->
    PRK = hkdf_hmac(Salt, Key),
    hkdf_expand(PRK, Info, Length).

%% @doc KDF-Tor
%%
%% For more information, see tor-spec.txt section 5.2.1.
%%
%% @end
-spec kdf_tor(Key, Length) -> binary()
    when
        Key    :: binary(),
        Length :: pos_integer().
kdf_tor(Key, Length) ->
    L = (Length + 20 - onion_math:mod(Length, 20)) div 20,
    R = iolist_to_binary([crypto:hash(sha, [Key, I]) || I <- lists:seq(0, L - 1)]),
    <<Result:Length/binary, _/binary>> = R,
    Result.

%% @private
hkdf_expand(PRK, Info, Length) ->
    Dict = lists:foldl(fun (N, Dict) ->
                           orddict:append(N, hkdf_t(PRK, N, Info, Dict), Dict)
                       end, orddict:new(), lists:seq(0, onion_math:ceil(Length / ?HKDF_HASH_LENGTH))),
    <<OKM:Length/binary, _/binary>> = iolist_to_binary(lists:map(fun ({_, Value}) ->
                                                                     Value
                                                                 end, orddict:to_list(Dict))),
    OKM.

%% @private
-spec hkdf_t(PRK, N, Info, Dict) -> binary()
    when
        PRK  :: binary(),
        N    :: non_neg_integer(),
        Info :: iodata(),
        Dict :: orddict:orddict(non_neg_integer(), binary()).
hkdf_t(PRK, N, Info, Dict) ->
    case N of
        0 ->
            <<>>;
        N ->
            {ok, Value} = orddict:find(N - 1, Dict),
            hkdf_hmac(PRK, [Value, Info, <<N:8/integer>>])
    end.

%% @private
-spec hkdf_hmac(Key, Data) -> binary()
    when
        Key  :: iodata(),
        Data :: iodata().
hkdf_hmac(Key, Data) ->
    crypto:hmac(sha256, Key, Data).

-ifdef(TEST).
hkdf_tor_test() ->
    %% Taken from test_crypto_hkdf_sha256() in tor/src/test/test_crypto.c.
    Salt   = <<"ntor-curve25519-sha256-1:key_extract">>,
    Expand = <<"ntor-curve25519-sha256-1:key_expand">>,
    [
        ?assertEqual(hkdf(<<"">>, Salt, Expand, 100),
                     base16_decode(["d3490ed48b12a48f9547861583573fe3f19aafe3f81dc7fc75",
                                    "eeed96d741b3290f941576c1f9f0b2d463d1ec7ab2c6bf71cd",
                                    "d7f826c6298c00dbfe6711635d7005f0269493edf6046cc7e7",
                                    "dcf6abe0d20c77cf363e8ffe358927817a3d3e73712cee28d8"])),
        ?assertEqual(hkdf(<<"Tor">>, Salt, Expand, 100),
                     base16_decode(["5521492a85139a8d9107a2d5c0d9c91610d0f95989975ebee6",
                                    "c02a4f8d622a6cfdf9b7c7edd3832e2760ded1eac309b76f8d",
                                    "66c4a3c4d6225429b3a016e3c3d45911152fc87bc2de9630c3",
                                    "961be9fdb9f93197ea8e5977180801926d3321fa21513e59ac"])),
        ?assertEqual(hkdf(<<"AN ALARMING ITEM TO FIND ON YOUR CREDIT-RATING STATEMENT">>, Salt, Expand, 100),
                     base16_decode(["a2aa9b50da7e481d30463adb8f233ff06e9571a0ca6ab6df0f",
                                    "b206fa34e5bc78d063fc291501beec53b36e5a0e434561200c",
                                    "5f8bd13e0f88b3459600b4dc21d69363e2895321c06184879d",
                                    "94b18f078411be70b767c7fc40679a9440a0c95ea83a23efbf"]))
    ].

hkdf_rfc5869_1_test() ->
    IKM  = base16_decode("0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"),
    Salt = base16_decode("000102030405060708090a0b0c"),
    Info = base16_decode("f0f1f2f3f4f5f6f7f8f9"),
    L = 42,
    [
        ?assertEqual(hkdf(IKM, Salt, Info, L), base16_decode(["3cb25f25faacd57a90434f64d0362f2a",
                                                              "2d2d0a90cf1a5a4c5db02d56ecc4c5bf",
                                                              "34007208d5b887185865"]))
    ].

hkdf_rfc5869_2_test() ->
    IKM  = base16_decode(["000102030405060708090a0b0c0d0e0f",
                          "101112131415161718191a1b1c1d1e1f",
                          "202122232425262728292a2b2c2d2e2f",
                          "303132333435363738393a3b3c3d3e3f",
                          "404142434445464748494a4b4c4d4e4f"]),
    Salt = base16_decode(["606162636465666768696a6b6c6d6e6f",
                          "707172737475767778797a7b7c7d7e7f",
                          "808182838485868788898a8b8c8d8e8f",
                          "909192939495969798999a9b9c9d9e9f",
                          "a0a1a2a3a4a5a6a7a8a9aaabacadaeaf"]),
    Info = base16_decode(["b0b1b2b3b4b5b6b7b8b9babbbcbdbebf",
                          "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf",
                          "d0d1d2d3d4d5d6d7d8d9dadbdcdddedf",
                          "e0e1e2e3e4e5e6e7e8e9eaebecedeeef",
                          "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"]),
    L = 82,
    [
        ?assertEqual(hkdf(IKM, Salt, Info, L), base16_decode(["b11e398dc80327a1c8e7f78c596a4934",
                                                              "4f012eda2d4efad8a050cc4c19afa97c",
                                                              "59045a99cac7827271cb41c65e590e09",
                                                              "da3275600c2f09b8367793a9aca3db71",
                                                              "cc30c58179ec3e87c14c01d5c1f3434f",
                                                              "1d87"]))
    ].

hkdf_rfc5869_3_test() ->
    IKM  = base16_decode("0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"),
    Salt = <<>>,
    Info = <<>>,
    L = 42,
    [
        ?assertEqual(hkdf(IKM, Salt, Info, L), base16_decode(["8da4e775a563c18f715f802a063c5a31",
                                                              "b8a11f5c5ee1879ec3454e5f3c738d2d",
                                                              "9d201395faa4b61a96c8"]))
    ].

kdf_tor_test() ->
    [
        ?assertEqual(kdf_tor(<<"">>, 100),
                     base16_decode(["5ba93c9db0cff93f52b521d7420e43f6eda2784fbf8b4530d8",
                                    "d246dd74ac53a13471bba17941dff7c4ea21bb365bbeeaf5f2",
                                    "c654883e56d11e43c44e9842926af7ca0a8cca12604f945414",
                                    "f07b01e13da42c6cf1de3abfdea9b95f34687cbbe92b9a7383"])),

        ?assertEqual(kdf_tor(<<"Tor">>, 100),
                     base16_decode(["776c6214fc647aaa5f683c737ee66ec44f03d0372e1cce6922",
                                    "7950f236ddf1e329a7ce7c227903303f525a8c6662426e8034",
                                    "870642a6dabbd41b5d97ec9bf2312ea729992f48f8ea2d0ba8",
                                    "3f45dfda1a80bdc8b80de01b23e3e0ffae099b3e4ccf28dc28"])),

        ?assertEqual(kdf_tor(<<"AN ALARMING ITEM TO FIND ON A MONTHLY AUTO-DEBIT NOTICE">>, 100),
                     base16_decode(["a340b5d126086c3ab29c2af4179196dbf95e1c72431419d331",
                                    "4844bf8f6afb6098db952b95581fb6c33625709d6f4400b8e7",
                                    "ace18a70579fad83c0982ef73f89395bcc39493ad53a685854",
                                    "daf2ba9b78733b805d9a6824c907ee1dba5ac27a1e466d4d10"]))
    ].

-endif.
