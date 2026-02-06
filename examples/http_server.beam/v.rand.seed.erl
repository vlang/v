-module('v.rand.seed').
-export([nr_next/1, time_seed_array/1, time_seed_32/0, time_seed_64/0]).

nr_next(Prev) ->
    Prev * 1664525 + 1013904223.

time_seed_array(Count) ->
    Ctime = sys_mono_now(),
    Seed = todo,
    Seed_data = [],
    Seed1 = lists:foldl(fun(_, SeedAcc) ->
        SeedOut = nr_next(Seed1),
        Seed_data bsl nr_next(Seed1),
        SeedOut
    end, Seed, lists:seq(0, Count - 1)),
    Seed_data.

time_seed_32() ->
    Sa = time_seed_array(1),
    Res = lists:nth(1, Sa),
    todo,
    Res.

time_seed_64() ->
    Seed_data = time_seed_array(2),
    Lower = todo,
    Upper = todo,
    todo,
    Res = Lower bor (Upper bsl 32),
    Res.
