-module('v.main').
-export([vlang_time/1, remote_ip/1, main/0]).

vlang_time(Wg) ->
    Start = ticks(),
    Data = get(<<"https://vlang.io/utc_now">>),
    Finish = ticks(),
    vbeam_io:println(<<"Finish getting time ", (integer_to_binary(Finish - Start))/binary, " ms">>),
    vbeam_io:println(maps:get(body, Data)),
    'WaitGroup.done'(Wg),
    maps:get(body, Data).

remote_ip(Wg) ->
    Start = ticks(),
    Data = get(<<"https://api.ipify.org">>),
    Finish = ticks(),
    vbeam_io:println(<<"Finish getting ip ", (integer_to_binary(Finish - Start))/binary, " ms">>),
    vbeam_io:println(maps:get(body, Data)),
    'WaitGroup.done'(Wg),
    maps:get(body, Data).

main() ->
    Wg = new_waitgroup(),
    'WaitGroup.add'(Wg, 2),
    todo,
    todo,
    'WaitGroup.wait'(Wg),
    ok.
