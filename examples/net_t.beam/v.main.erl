-module('v.main').
-export([send_request/1, main/0]).

send_request(Wg) ->
    Start = ticks(),
    Data = get(<<"https://google.com">>),
    Finish = ticks(),
    vbeam_io:println(<<"Finish getting time ", (integer_to_binary(Finish - Start))/binary, " ms">>),
    'WaitGroup.done'(Wg),
    maps:get(body, Data).

main() ->
    Wg = new_waitgroup(),
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
    'WaitGroup.wait'(Wg),
    ok.
