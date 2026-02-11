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
len(List) when is_list(List) ->
    length(List).

%% Get first element (or none if empty)
first([H|_]) -> H;
first([]) -> error(empty_array).

%% Get last element (or none if empty)
last([H]) -> H;
last([_|T]) -> last(T);
last([]) -> error(empty_array).

%% Append element to end (returns new list)
push(List, Elem) when is_list(List) ->
    List ++ [Elem].

%% Remove and return last element (returns {Element, NewList})
pop([]) ->
    error(empty_array);
pop(List) when is_list(List) ->
    {lists:last(List), lists:droplast(List)}.

%% Insert element at index (0-indexed)
insert(List, Index, Elem) when is_list(List), is_integer(Index), Index >= 0 ->
    {Before, After} = lists:split(Index, List),
    Before ++ [Elem] ++ After.

%% Delete element at index (0-indexed)
delete(List, Index) when is_list(List), is_integer(Index), Index >= 0 ->
    {Before, [_|After]} = lists:split(Index, List),
    Before ++ After.

%% Get element at index (0-indexed, returns none if out of bounds)
index(List, Index) when is_list(List), is_integer(Index), Index >= 0 ->
    case Index < length(List) of
        true -> lists:nth(Index + 1, List);  %% Convert to 1-indexed
        false -> error({index_out_of_bounds, Index})
    end.

%% Check if array contains element
contains(List, Elem) when is_list(List) ->
    lists:member(Elem, List).

%% Get slice from Start to End (exclusive), 0-indexed
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
reverse(List) when is_list(List) ->
    lists:reverse(List).

%% Filter array with predicate function
filter(List, Fun) when is_list(List), is_function(Fun, 1) ->
    lists:filter(Fun, List).

%% Map function over array
map(List, Fun) when is_list(List), is_function(Fun, 1) ->
    lists:map(Fun, List).

%% Reduce array with accumulator
reduce(List, Init, Fun) when is_list(List), is_function(Fun, 2) ->
    lists:foldl(Fun, Init, List).

%% Sort array (ascending)
sort(List) when is_list(List) ->
    lists:sort(List).

%% Sort array with comparison function
sort(List, Fun) when is_list(List), is_function(Fun, 2) ->
    lists:sort(Fun, List).

%% Join array elements with separator
join(List, Sep) when is_list(List), is_binary(Sep) ->
    case List of
        [] -> <<>>;
        [H|T] ->
            lists:foldl(fun(Elem, Acc) ->
                ElemBin = vbeam_conv:to_binary(Elem),
                <<Acc/binary, Sep/binary, ElemBin/binary>>
            end, vbeam_conv:to_binary(H), T)
    end.
