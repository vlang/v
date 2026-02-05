-module('v.main').
-export([main/0]).

main() ->
    Addr = <<"127.0.0.1:42228">>,
    Stream = dial_tcp(Addr),
    Log_inter = #{{vbeam, type} => 'LoggingInterceptor'},
    Inters = #{event => [maps:get(on_event, Log_inter)], encoded_request => [maps:get(on_encoded_request, Log_inter)], request => [maps:get(on_request, Log_inter)], response => [maps:get(on_response, Log_inter)], encoded_response => [maps:get(on_encoded_response, Log_inter)], {vbeam, type} => 'Interceptors'},
    C = new_client(#{stream => Stream, interceptors => Inters, {vbeam, type} => 'ClientConfig'}),
    vbeam_io:println(<<"TCP JSON-RPC client on ", (Addr)/binary>>),
    D1 = 'Client.request'(C, <<"kv.delete">>, #{<<"key">> => <<"foo">>}, <<"kv.delete">>),
    vbeam_io:println(<<"RESULT: ", (D1)/binary>>),
    Res = 'Client.batch'(C, [new_request(<<"kv.create">>, #{<<"key">> => <<"foo">>, <<"value">> => <<"bar">>}, <<"kv.create">>), new_request(<<"kv.create">>, #{<<"key">> => <<"bar">>, <<"value">> => <<"foo">>}, <<"kv.create">>)]),
    vbeam_io:println(<<"RESULT: ", (Res)/binary>>),
    'Client.notify'(C, <<"kv.create">>, #{<<"key">> => <<"bazz">>, <<"value">> => <<"barr">>}),
    ok.
