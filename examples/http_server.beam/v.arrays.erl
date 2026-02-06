-module('v.arrays').
-export([min/1, max/1, idx_min/1, idx_max/1, merge/2, append/2, group/1, chunk/2, chunk_while/2, window/2, sum/1, reduce/2, reduce_indexed/2, filter_indexed/2, fold/3, fold_indexed/3, flatten/1, flat_map/2, flat_map_indexed/2, map_indexed/2, group_by/2, concat/2, lower_bound/2, upper_bound/2, binary_search/2, rotate_left/2, rotate_right/2, ptr_rotate/3, raw_array_cap/0, raw_array_malloc_size/0, memswap/3, swap_nonoverlapping/3, copy/2, can_copy_bits/0, carray_to_varray/2, find_first/2, find_last/2, join_to_string/3, partition/2, each/2, each_indexed/2, index_of_first/2, index_of_last/2, map_of_indexes/1, map_of_counts/1, reverse_iterator/1, 'ReverseIterator.next'/1, 'ReverseIterator.free'/1, uniq/1, uniq_only/1, uniq_only_repeated/1, uniq_all_repeated/1, distinct/1]).

min(Array) ->
    case length(Array) == 0 of
        true -> error(<<".min called on an empty array">>);
        false -> begin
            Val = lists:nth(1, Array),
            lists:foreach(fun(E) ->
                case E < Val of
                    true -> ok;
                    false -> ok
                end,
                ok
            end, Array),
            Val
        end
        end.

max(Array) ->
    case length(Array) == 0 of
        true -> error(<<".max called on an empty array">>);
        false -> begin
            Val = lists:nth(1, Array),
            lists:foreach(fun(E) ->
                case E > Val of
                    true -> ok;
                    false -> ok
                end,
                ok
            end, Array),
            Val
        end
        end.

idx_min(Array) ->
    case length(Array) == 0 of
        true -> error(<<".idx_min called on an empty array">>);
        false -> begin
            Idx = 0,
            Val = lists:nth(1, Array),
            lists:foreach(fun(E) ->
                case E < Val of
                    true -> begin
                        Val1 = E,
                        Idx1 = I,
                    end;
                    false -> ok
                end,
                ok
            end, Array),
            Idx1
        end
        end.

idx_max(Array) ->
    case length(Array) == 0 of
        true -> error(<<".idx_max called on an empty array">>);
        false -> begin
            Idx = 0,
            Val = lists:nth(1, Array),
            lists:foreach(fun(E) ->
                case E > Val of
                    true -> begin
                        Val1 = E,
                        Idx1 = I,
                    end;
                    false -> ok
                end,
                ok
            end, Array),
            Idx1
        end
        end.

merge(A, B) ->
    M = [],
    Ia = 0,
    Ib = 0,
    J = 0,
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    M.

append(A, B) ->
    M = [],
    M bsl A,
    M bsl B,
    M.

group(Arrs) ->
    Length = case length(Arrs) > 0 of
        true -> length(lists:nth(1, Arrs));
        false -> 0
    end,
    lists:foreach(fun(Ndx) ->
        case length(lists:nth(Ndx + 1, Arrs)) < Length of
            true -> ok;
            false -> ok
        end,
        ok
    end, lists:seq(1, length(Arrs) - 1)),
    case Length > 0 of
        true -> Arr;
        false -> []
        end.

chunk(Array, Size) ->
    Chunks = [],
    % TODO: unhandled stmt type
    Chunks.

chunk_while(A, Predicate) ->
    case length(A) == 0 of
        true -> [];
        false -> begin
            Chunks = [],
            Chunk = [lists:nth(1, A)],
            I = 0,
            % TODO: unhandled stmt type
            case length(Chunk) > 0 of
                true -> Chunks bsl Chunk;
                false -> ok
            end,
            Chunks
        end
        end.

window(Array, Attr) ->
    case length(Array) == 0 of
        true -> [];
        false -> begin
            Windows = [],
            % TODO: unhandled stmt type
            Windows
        end
        end.

sum(Array) ->
    case length(Array) == 0 of
        true -> error(<<"Cannot sum up array of nothing.">>);
        false -> begin
            Head = lists:nth(1, Array),
            lists:foreach(fun(E) ->
                Head1 = E,
                ok
            end, lists:nth(todo + 1, Array)),
            Head1
        end
    end.

reduce(Array, Reduce_op) ->
    case length(Array) == 0 of
        true -> error(<<"Cannot reduce array of nothing.">>);
        false -> begin
            Value = lists:nth(1, Array),
            lists:foreach(fun(E) ->
                Value1 = reduce_op(Value, E),
                ok
            end, lists:nth(todo + 1, Array)),
            Value1
        end
    end.

reduce_indexed(Array, Reduce_op) ->
    case length(Array) == 0 of
        true -> error(<<"Cannot reduce array of nothing.">>);
        false -> begin
            Value = lists:nth(1, Array),
            lists:foreach(fun(E) ->
                case I == 0 of
                    true -> ok;
                    false -> ok
                end.
                ok
            end, Array),
            Value
        end
    end.

filter_indexed(Array, Predicate) ->
    Result = [],
    lists:foreach(fun(E) ->
        case predicate(I, E) of
            true -> Result bsl E;
            false -> ok
        end,
        ok
    end, Array),
    Result.

fold(Array, Init, Fold_op) ->
    Value = #{{vbeam, type} => 'R'},
    lists:foreach(fun(E) ->
        Value1 = fold_op(Value, E),
        ok
    end, Array),
    Value1.

fold_indexed(Array, Init, Fold_op) ->
    Value = Init,
    lists:foreach(fun(E) ->
        Value1 = fold_op(I, Value, E),
        ok
    end, Array),
    Value1.

flatten(Array) ->
    Required_size = 0,
    lists:foreach(fun(E1) ->
        lists:foreach(fun(_) ->
            Required_size1 = 1,
            ok
        end, E1),
        ok
    end, Array),
    Result = [],
    lists:foreach(fun(E1) ->
        lists:foreach(fun(E2) ->
            Result bsl E2,
            ok
        end, E1),
        ok
    end, Array),
    Result.

flat_map(Array, Transform) ->
    Result = [],
    lists:foreach(fun(V) ->
        Result bsl transform(V),
        ok
    end, Array),
    flatten(Result).

flat_map_indexed(Array, Transform) ->
    Result = [],
    lists:foreach(fun(V) ->
        Result bsl transform(I, V),
        ok
    end, Array),
    flatten(Result).

map_indexed(Array, Transform) ->
    Result = [],
    lists:foreach(fun(V) ->
        Result bsl transform(I, V),
        ok
    end, Array),
    Result.

group_by(Array, Grouping_op) ->
    Result = #{},
    lists:foreach(fun(V) ->
        Key = grouping_op(V),
        case lists:member(Key, Result) of
            true -> lists:nth(Key + 1, Result) bsl V;
            false -> ok
        end,
        ok
    end, Array),
    Result.

concat(A, B) ->
    M = [],
    M bsl A,
    M bsl B,
    M.

lower_bound(Array, Val) ->
    case length(Array) == 0 of
        true -> error(<<".lower_bound called on an empty array">>);
        false -> begin
            Left = 0,
            Right = length(Array) - 1,
            % TODO: unhandled stmt type
            case Left >= length(Array) of
                true -> error(<<"">>);
                false -> lists:nth(Left + 1, Array)
            end
        end
        end.

upper_bound(Array, Val) ->
    case length(Array) == 0 of
        true -> error(<<".upper_bound called on an empty array">>);
        false -> begin
            Left = 0,
            Right = length(Array) - 1,
            % TODO: unhandled stmt type
            case Right < 0 of
                true -> error(<<"">>);
                false -> lists:nth(Right + 1, Array)
            end
        end
        end.

binary_search(Array, Target) ->
    Left = 0,
    Right = length(Array) - 1,
    % TODO: unhandled stmt type
    error(<<"">>).

rotate_left(Array, Mid) ->
    % TODO: unhandled stmt type
    K = length(Array) - Mid,
    P = todo,
    todo,
    ok.

rotate_right(Array, K) ->
    % TODO: unhandled stmt type
    Mid = length(Array) - K,
    P = todo,
    todo,
    ok.

ptr_rotate(Left_, Mid, Right_) ->
    Sz = todo,
    Left = todo,
    Right = todo,
    Limit = raw_array_cap(),
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
        ok.

raw_array_cap() ->
    Size = todo,
    case Size > Extra_size of
        true -> 1;
        false -> Extra_size / Size
    end.

raw_array_malloc_size() ->
    Size = todo,
    case Size > Extra_size of
        true -> Size * 2;
        false -> Extra_size
    end.

memswap(X, Y, Len) ->
    Block_size = todo,
    I = todo,
    % TODO: unhandled stmt type
    case I < Len of
        true -> begin
            T_ = #{{vbeam, type} => 'UnalignedBlock'},
            T = todo,
            Rem = todo,
            Xi = todo + I,
            Yi = todo + I,
            % TODO: unhandled stmt type
        end;
        false -> ok
    end.

swap_nonoverlapping(X_, Y_, Count) ->
    X = todo,
    Y = todo,
    Len = todo * todo,
    todo,
    ok.

copy(Dst, Src) ->
    Min = case length(Dst) < length(Src) of
        true -> length(Dst);
        false -> length(Src)
    end,
    case Min =< 0 of
        true -> 0;
        false -> begin
            case can_copy_bits() of
                true -> begin
                    Blen = Min * todo,
                    todo
                end;
                false -> ok
            end,
            Min
        end
        end.

can_copy_bits() ->
    case lists:member(lists:nth(1, maps:get(name, T)), [todo, todo, todo, todo, todo, todo, todo, todo]) of
        true -> true;
        false -> false
        end.

carray_to_varray(C_array_data, Items) ->
    V_array = [],
    Total_size = Items * todo,
    todo,
    V_array.

find_first(Array, Predicate) ->
    case length(Array) == 0 of
        true -> todo;
        false -> begin
            lists:foreach(fun(Item) ->
                case predicate(Item) of
                    true -> Item;
                    false -> ok
                end,
                ok
            end, Array),
            todo
        end
        end.

find_last(Array, Predicate) ->
    case length(Array) == 0 of
        true -> todo;
        false -> begin
            % TODO: unhandled stmt type
            todo
        end
        end.

join_to_string(Array, Separator, Transform) ->
    Sb = new_builder(length(Array) * 2),
    % TODO: unhandled stmt type
    lists:foreach(fun(Item) ->
        X = transform(Item),
        'unknown.write_string'(Sb, X),
        todo,
        case I < length(Array) - 1 of
            true -> 'unknown.write_string'(Sb, Separator);
            false -> ok
        end,
        ok
    end, Array),
    'unknown.str'(Sb).

partition(Array, Predicate) ->
    Matching = [],
    Non_matching = [],
    lists:foreach(fun(Item) ->
        case predicate(Item) of
            true -> Matching bsl Item;
            false -> Non_matching bsl Item
        end,
        ok
    end, Array),
    Matching.

each(A, Cb) ->
    lists:foreach(fun(Item) ->
        cb(Item),
        ok.
        ok
    end, A),
        ok.

each_indexed(A, Cb) ->
    lists:foreach(fun(Item) ->
        cb(Idx, Item),
        ok.
        ok
    end, A),
        ok.

index_of_first(Array, Predicate) ->
    lists:foreach(fun(E) ->
        case predicate(I, E) of
            true -> I;
            false -> ok
        end,
        ok
    end, Array),
    -1.

index_of_last(Array, Predicate) ->
    % TODO: unhandled stmt type
    -1.

map_of_indexes(Array) ->
    Result = #{},
    lists:foreach(fun(E) ->
        case todo of
            true -> lists:nth(E + 1, Result) bsl I;
            false -> ok
        end,
        ok
    end, Array),
    Result.

map_of_counts(Array) ->
    Result = #{},
    lists:foreach(fun(E) ->
        todo,
        ok
    end, Array),
    Result.

reverse_iterator(A) ->
    #{a => A, i => length(A), {vbeam, type} => 'ReverseIterator'}.

'ReverseIterator.next'(Iter) ->
    todo,
    case maps:get(i, Iter) < 0 of
        true -> todo;
        false -> todo
        end.

'ReverseIterator.free'(Iter) ->
    ok.

uniq(A) ->
    Res = [],
    J = -1,
    case length(A) > 0 of
        true -> begin
            J1 = 0,
            Res bsl lists:nth(1, A)
        end;
        false -> ok
    end,
    lists:foreach(fun(E) ->
        case lists:nth(J1 + 1, A) == E of
            true -> ok;
            false -> ok
        end,
        J2 = Idx,
        Res bsl E,
        ok
    end, A),
    Res.

uniq_only(A) ->
    case length(A) == 0 of
        true -> [];
        false -> 
            case length(A) == 1 of
                true -> 'unknown.clone'(A);
                false -> 
                    case length(A) == 2 of
                        true -> [];
                        false -> begin
                            Res = [],
                            case lists:nth(1, A) /= lists:nth(2, A) of
                                true -> Res bsl lists:nth(1, A);
                                false -> ok
                            end,
                            % TODO: unhandled stmt type
                            case lists:nth(length(A) - 2 + 1, A) /= lists:nth(length(A) - 1 + 1, A) of
                                true -> Res bsl lists:nth(length(A) - 1 + 1, A);
                                false -> ok
                            end,
                            Res
                        end
                                        end
                                        end
                end.

uniq_only_repeated(A) ->
    case length(A) == 0 orelse length(A) == 1 of
        true -> [];
        false -> begin
            Res = [],
            % TODO: unhandled stmt type
            case lists:nth(length(A) - 2 + 1, A) == lists:nth(length(A) - 1 + 1, A) of
                true -> Res bsl lists:nth(length(A) - 1 + 1, A);
                false -> ok
            end,
            Res
        end
        end.

uniq_all_repeated(A) ->
    case length(A) == 0 orelse length(A) == 1 of
        true -> [];
        false -> begin
            case length(A) == 2 of
                true -> case lists:nth(1, A) == lists:nth(2, A) of
                    true -> 'unknown.clone'(A);
                    false -> ok
                end;
                false -> ok
            end,
            Res = [],
            % TODO: unhandled stmt type
            Res
        end
        end.

distinct(A) ->
    uniq('unknown.sorted'(A, A < B)).
