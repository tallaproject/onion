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
-export([hkdf/4]).

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
base16_decode(Data) ->
    {ok, DecodedData} = onion_base16:decode(iolist_to_binary(Data)),
    DecodedData.

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

-endif.
