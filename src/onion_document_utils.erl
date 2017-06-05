-module(onion_document_utils).

-export([parse_portlist/1,
         decode_items/2,
         decode_bool/1,
         decode_address/1,
         verify_existence_properties/2,
         verify_order/2,
         parse_datetime/1,
         sp_split/1,
         binaries2integers/1,
         split_items/2
        ]).


decode_items(Items, Parser) ->
    decode_items(Items, [], 1, #{}, Parser).

%% @private
-spec decode_items(ItemsIn, ItemsOut, Position, ItemOrdering, Parser) -> [ParsedItems]
    when
      Parser       :: function(),
      ItemsIn      :: [term()],
      ItemsOut     :: [term()],
      Position     :: non_neg_integer(),
      ItemOrdering :: #{},
      ParsedItems  :: term().
decode_items([{Keyword, Arguments, Object} | Rest], ItemsOut, Position, ItemOrdering, Parser) ->
    ParsedItemData = Parser(Keyword, Arguments, Object),
    decode_items(Rest,
                     [{Keyword, ParsedItemData} | ItemsOut],
                     Position + 1,
                     update_stats(Keyword, Position, ItemOrdering),
                     Parser);

decode_items([], ItemsOut, Position, ItemOrdering, _Parser) ->
    DocumentLength = Position - 1,
    {lists:reverse(ItemsOut), DocumentLength, ItemOrdering}.


%% @private
-spec update_stats(Keyword, Position, DocumentStats) -> #{}
    when
      Keyword       :: atom() | binary(),
      Position      :: non_neg_integer(),
      DocumentStats :: #{}.
update_stats(Keyword, Position, DocumentStats) ->
    Occurences =  maps:get(Keyword, DocumentStats, []),
    DocumentStats#{ Keyword => [Position | Occurences] }.

verify_existence_properties(ItemSpecs, ItemOrdering) ->
    true = lists:all(
                fun ({Keyword, at_most_once}) ->
                        at_most_once(Keyword, ItemOrdering);

                    ({Keyword, exactly_once}) ->
                        exactly_once(Keyword, ItemOrdering);

                    ({Keyword, at_least_once}) ->
                        at_least_once(Keyword, ItemOrdering);

                    %% "Any Number" is only included for completeness
                    ({_Keyword, any_number}) ->
                        true

                end, ItemSpecs),
    ok.

%% @private
-spec at_most_once(Keyword, DocumentStats) -> true | false
    when
        Keyword       :: atom(),
        DocumentStats :: #{}.
at_most_once(Keyword, DocumentStats) ->
    occurences_in_document(Keyword, DocumentStats) =< 1.

%% @private
-spec exactly_once(Keyword, DocumentStats) -> true | false
    when
      Keyword       :: atom(),
      DocumentStats :: #{}.
exactly_once(Keyword, DocumentStats) ->
    occurences_in_document(Keyword, DocumentStats) =:= 1.

%% @private
-spec at_least_once(Keyword, DocumentStats) -> true | false
    when
      Keyword       :: atom(),
      DocumentStats :: #{}.
at_least_once(Keyword, DocumentStats) ->
    occurences_in_document(Keyword, DocumentStats) >= 1.

%% @private
-spec occurences_in_document(Keyword, DocumentStats) -> non_neg_integer()
    when
      Keyword       :: binary() | atom(),
      DocumentStats :: #{}.
occurences_in_document(Keyword, DocumentStats) ->
    length(maps:get(Keyword, DocumentStats, [])).


%% FIXME this needs some unit-testing!
verify_order(OrderSpec, ItemOrdering) ->
    order_verifier(OrderSpec, ItemOrdering, 0, 0).

order_verifier([{SpecKeyword, _} | RestSpec], ItemOrdering, PreviousMinPosition, PreviousMaxPosition) ->
    %% We insert default values that will always produce {true, true} in case the SpecKeyword is not found
    %% Checks for mandatory fields etc. is done elsewhere.
    MinPosition = min_list(maps:get(SpecKeyword, ItemOrdering, [PreviousMinPosition])),
    MaxPosition = max_list(maps:get(SpecKeyword, ItemOrdering, [PreviousMaxPosition])),
    case {PreviousMinPosition =< MinPosition, PreviousMaxPosition =< MaxPosition} of
        {true, true} ->
            %% The next item in the spec is found and it has the correct order.
            %% Proceed to the next item and look for the next item in spec.
            order_verifier(RestSpec, ItemOrdering, MinPosition, MaxPosition);
        _ ->
            order_violated
    end;

order_verifier([], _ItemOrdering, _PreviousMinPosition, _PreviousMaxPosition) ->
    %% We have parsed all items without any violation
    ok.

max_list([A, B | Rest]) ->
    max_list([max(A, B) | Rest]);

max_list([A]) ->
    A.

min_list([A, B | Rest]) ->
    min_list([min(A, B) | Rest]);

min_list([A]) ->
    A.


-spec split_items(Items, Keyword) -> {ok, Part, Rest} | keyword_not_found
    when
        Keyword :: atom(),
        Items   :: [term()],
        Part    :: Items,
        Rest    :: Items.
split_items(Items, Keyword) ->
    item_splitter(Items, Keyword, []).

%% @private
item_splitter([{Keyword, _, _} | _] = Items, Keyword, ProcessedItems) ->
    {ok, lists:reverse(ProcessedItems), Items};

item_splitter([Item | Rest], Keyword, ProcessedItems) ->
    item_splitter(Rest, Keyword, [Item | ProcessedItems]);

item_splitter([], _Keyword, _ProcessedItems) ->
    keyword_not_found.


-spec parse_datetime(DateTimeRaw) -> DateTime
    when
        DateTimeRaw :: binary(),
        DateTime    :: calendar:datetime().
parse_datetime(<<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary, " ", Hour:2/binary, ":", Minutes:2/binary, ":", Seconds:2/binary>>) ->
    Date = erlang:list_to_tuple(binaries2integers([Year, Month, Day])),
    Time = erlang:list_to_tuple(binaries2integers([Hour, Minutes, Seconds])),
    {Date, Time}.


-spec parse_portlist(PortListEncoded) -> [PortOrRange]
    when
      PortListEncoded :: binary(),
      PortOrRange     :: inet:port_number() | {inet:port_number(), inet:port_number()}.
parse_portlist(PortListEncoded) ->
    lists:map(fun decode_port_or_range/1, binary:split(PortListEncoded, <<",">>, [global])).


%% @private
-spec decode_port_or_range(PortOrRangeEncoded) -> [PortOrRange]
    when
      PortOrRangeEncoded :: binary(),
      PortOrRange        :: inet:port_number() | {inet:port_number(), inet:port_number()}.
decode_port_or_range(PortOrRange) ->
    case binary:split(PortOrRange, <<"-">>) of
        [PortStart, PortEnd] ->
            binaries2integers([PortStart, PortEnd]);
        [Port] ->
            erlang:binary_to_integer(Port)
    end.


-spec decode_bool(binary()) -> boolean().
decode_bool(<<"1">>) ->
    true;

decode_bool(<<"0">>) ->
    false.


-spec decode_address(AddressRaw) -> {inet:ip_address(), inet:port()}
    when
        AddressRaw :: binary().
decode_address(<<"[", IPv6Raw/binary>>) ->
    [IPv6_, Port] = binary:split(IPv6Raw, <<"]:">>),
    %% This would also pass on an IPv4 address
    {ok, IPv6} = inet:decode_address(erlang:binary_to_list(IPv6_)),
    {IPv6, erlang:binary_to_integer(Port)};

decode_address(IPv4Raw) ->
    [IPv4_, Port] = binary:split(IPv4Raw, <<":">>),
    {ok, IPv4} = inet:decode_address(erlang:binary_to_list(IPv4_)),
    {IPv4, erlang:binary_to_integer(Port)}.


-spec binaries2integers([binary()]) -> [integer()].
binaries2integers(Binaries) ->
    lists:map(fun erlang:binary_to_integer/1, Binaries).


-spec  sp_split(Arguments) -> [Argument]
    when
      Arguments :: binary() | no_arguments,
      Argument  :: binary().
sp_split(no_arguments) ->
    [];

sp_split(Binary) ->
    binary:split(Binary, <<" ">>, [global]).
