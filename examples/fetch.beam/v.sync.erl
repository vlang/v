-module('v.sync').
-export([new_many_times/1, 'ManyTimes.do'/2, 'ManyTimes.do_slow'/2]).

new_many_times(Times) ->
    Many_times = &#{times => Times, {vbeam, type} => 'ManyTimes'},
    'RwMutex.init'(maps:get(m, Many_times)),
    Many_times.

'ManyTimes.do'(M, F) ->
    case load_u64(&maps:get(count, M)) < maps:get(times, M) of
        true -> 'ManyTimes.do_slow'(M, F);
        false -> ok
    end.

'ManyTimes.do_slow'(M, F) ->
    'RwMutex.lock'(maps:get(m, M)),
    case maps:get(count, M) < maps:get(times, M) of
        true -> begin
            store_u64(&maps:get(count, M), maps:get(count, M) + 1),
            f()
        end;
        false -> ok
    end,
    'RwMutex.unlock'(maps:get(m, M)),
    ok.
