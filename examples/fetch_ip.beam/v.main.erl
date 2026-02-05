-module('v.main').
-export([main/0]).

main() ->
    Url = <<"http://ifconfig.co/json">>,
    Resp = fetch(#{url => Url, user_agent => <<"Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/117.0">>, {vbeam, type} => 'FetchConfig'}),
    Ip_info = decode(todo, maps:get(body, Resp)),
    vbeam_io:println(Ip_info),
    ok.
