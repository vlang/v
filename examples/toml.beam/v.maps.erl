-module('v.maps').
-export([filter/2, to_array/2, flat_map/2, to_map/2, invert/1, from_array/1, merge_in_place/2, merge/2]).

filter(M, F) ->
    Mp = #{},
    lists:foreach(fun(V) ->
        case f(K, V) of
            true -> ok;
            false -> ok
        end,
        ok
    end, M),
    Mp.

to_array(M, F) ->
    A = [],
    lists:foreach(fun(V) ->
        A bsl f(K, V),
        ok
    end, M),
    A.

flat_map(M, F) ->
    A = [],
    lists:foreach(fun(V) ->
        A bsl f(K, V),
        ok
    end, M),
    A.

to_map(M, F) ->
    Mp = #{},
    lists:foreach(fun(V) ->
        X = element(1, f(K, V)),
        Y = element(2, f(K, V)),
        ok
    end, M),
    Mp.

invert(M) ->
    Mp = #{},
    lists:foreach(fun(V) ->
        ok
    end, M),
    Mp.

from_array(Array) ->
    Mp = #{},
    lists:foreach(fun(E) ->
        ok
    end, Array),
    Mp.

merge_in_place(M1, M2) ->
    lists:foreach(fun(V) ->
        ok
    end, M2),

merge(M1, M2) ->
    Res = 'unknown.clone'(M1),
    lists:foreach(fun(V) ->
        ok
    end, M2),
    Res.
