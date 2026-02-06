-module('v.main').
-export([main/0]).

main() ->
    Resp = get(<<"https://vlang.io/utc_now">>),
    T = unix(binary_to_integer(maps:get(body, Resp))),
    vbeam_io:println('Time.format'(T)),
    ok.
