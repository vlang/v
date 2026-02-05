-module('v.main').
-export([main/0, quick_sort/3, is_sorted/1]).
% TODO: const gen_len = 1000;
% TODO: const gen_max = 10000;

main() ->
    Arr = [],
    lists:foreach(fun(_) ->
        Arr << intn(10000),
        ok
    end, lists:seq(0, 1000 - 1)),
    vbeam_io:println(<<"length of random array is ", (integer_to_binary(length(Arr)))/binary>>),
    vbeam_io:println(<<"before quick sort whether array is sorted: ", (atom_to_binary(is_sorted(Arr)))/binary>>),
    quick_sort(Arr, 0, length(Arr) - 1),
    vbeam_io:println(<<"after quick sort whether array is sorted: ", (atom_to_binary(is_sorted(Arr)))/binary>>),
    ok.

quick_sort(Arr, L, R) ->
    case L >= R of
        true -> ok;
        false -> ok
    end,
    Sep = L,
    lists:foreach(fun(I) ->
        case lists:nth(I + 1, Arr) < lists:nth(L + 1, Arr) of
            true -> begin
                todo,
            end;
            false -> ok
        end,
        ok
    end, lists:seq(L + 1, R + 1 - 1)),
    quick_sort(Arr, L, Sep - 1),
    quick_sort(Arr, Sep + 1, R),
    ok.

is_sorted(Arr) ->
    lists:foreach(fun(I) ->
        case lists:nth(I + 1, Arr) > lists:nth(I + 1 + 1, Arr) of
            true -> false;
            false -> ok
        end,
        ok
    end, lists:seq(0, length(Arr) - 1 - 1)),
    true.
