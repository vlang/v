%% vbeam_array - Array operations for V language BEAM backend
%% V arrays are represented as Erlang lists
%% Note: V arrays are 0-indexed, Erlang lists are 1-indexed

-module(vbeam_array).
-export([len/1, first/1, last/1,
         push/2, pop/1, insert/3, delete/2,
         index/2, contains/2,
         slice/3, reverse/1,
         filter/2, map/2, reduce/3,
         sort/1, sort/2,
         join/2]).

%% Get array length
-spec len([term()]) -> non_neg_integer().
len(List) when is_list(List) ->
    length(List).

%% Get first element (or none if empty)
-spec first([term()]) -> term().
first([H|_]) -> H;
first([]) -> error(empty_array).

%% Get last element (or none if empty)
-spec last([term()]) -> term().
last([H]) -> H;
last([_|T]) -> last(T);
last([]) -> error(empty_array).

%% Append element to end (returns new list)
-spec push([term()], term()) -> [term()].
push(List, Elem) when is_list(List) ->
    List ++ [Elem].

%% Remove and return last element (returns {Element, NewList})
-spec pop([term()]) -> {term(), [term()]}.
pop([]) ->
    error(empty_array);
pop(List) when is_list(List) ->
    {lists:last(List), lists:droplast(List)}.

%% Insert element at index (0-indexed)
-spec insert([term()], non_neg_integer(), term()) -> [term()].
insert(List, Index, Elem) when is_list(List), is_integer(Index), Index >= 0 ->
    {Before, After} = lists:split(Index, List),
    Before ++ [Elem] ++ After.

%% Delete element at index (0-indexed)
-spec delete([term()], non_neg_integer()) -> [term()].
delete(List, Index) when is_list(List), is_integer(Index), Index >= 0 ->
    {Before, [_|After]} = lists:split(Index, List),
    Before ++ After.

%% Get element at index (0-indexed, returns none if out of bounds)
-spec index([term()], non_neg_integer()) -> term().
index(List, Index) when is_list(List), is_integer(Index), Index >= 0 ->
    case Index < length(List) of
        true -> lists:nth(Index + 1, List);  %% Convert to 1-indexed
        false -> error({index_out_of_bounds, Index})
    end.

%% Check if array contains element
-spec contains([term()], term()) -> boolean().
contains(List, Elem) when is_list(List) ->
    lists:member(Elem, List).

%% Get slice from Start to End (exclusive), 0-indexed
-spec slice([term()], integer(), integer()) -> [term()].
slice(List, Start, End) when is_list(List), is_integer(Start), is_integer(End) ->
    Len = length(List),
    ActualStart = max(0, Start),
    ActualEnd = min(End, Len),
    case ActualStart < ActualEnd of
        true ->
            lists:sublist(List, ActualStart + 1, ActualEnd - ActualStart);
        false ->
            []
    end.

%% Reverse array
-spec reverse([term()]) -> [term()].
reverse(List) when is_list(List) ->
    lists:reverse(List).

%% Filter array with predicate function
-spec filter([term()], fun((term()) -> boolean())) -> [term()].
filter(List, Fun) when is_list(List), is_function(Fun, 1) ->
    lists:filter(Fun, List).

%% Map function over array
-spec map([term()], fun((term()) -> term())) -> [term()].
map(List, Fun) when is_list(List), is_function(Fun, 1) ->
    lists:map(Fun, List).

%% Reduce array with accumulator
-spec reduce([term()], term(), fun((term(), term()) -> term())) -> term().
reduce(List, Init, Fun) when is_list(List), is_function(Fun, 2) ->
    lists:foldl(Fun, Init, List).

%% Sort array (ascending)
-spec sort([term()]) -> [term()].
sort(List) when is_list(List) ->
    lists:sort(List).

%% Sort array with comparison function
-spec sort([term()], fun((term(), term()) -> boolean())) -> [term()].
sort(List, Fun) when is_list(List), is_function(Fun, 2) ->
    lists:sort(Fun, List).

%% Join array elements with separator
-spec join([term()], binary()) -> binary().
join(List, Sep) when is_list(List), is_binary(Sep) ->
    case List of
        [] -> <<>>;
        [H|T] ->
            lists:foldl(fun(Elem, Acc) ->
                ElemBin = vbeam_conv:to_binary(Elem),
                <<Acc/binary, Sep/binary, ElemBin/binary>>
            end, vbeam_conv:to_binary(H), T)
    end.
