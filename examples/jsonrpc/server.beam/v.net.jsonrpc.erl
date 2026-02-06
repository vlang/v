-module('v.net.jsonrpc').
-export([new_client/1, 'Client.notify'/3, 'Client.request'/4, 'Client.batch'/2, dispatch_event/3, intercept_encoded_request/2, intercept_request/2, intercept_response/2, intercept_encoded_response/2, 'Server.is_interceptor_enabled'/1, 'Interceptors.get_interceptor'/1, 'ResponseError.code'/1, 'ResponseError.msg'/1, 'ResponseError.err'/1, response_error/1, error_with_code/2, 'Null.str'/1, 'Empty.str'/1, new_request/3, 'Request.encode'/1, 'Request.encode_batch'/1, 'Request.decode_params'/1, decode_request/1, decode_batch_request/1, new_response/3, 'Response.encode'/1, 'Response.encode_batch'/1, 'Response.decode_result'/1, decode_response/1, decode_batch_response/1, try_encode/1, try_decode/1, 'LoggingInterceptor.on_event'/3, 'LoggingInterceptor.on_encoded_request'/2, 'LoggingInterceptor.on_request'/2, 'LoggingInterceptor.on_response'/2, 'LoggingInterceptor.on_encoded_response'/2, new_server/1, 'Server.respond'/1, 'Server.writer'/1, 'Server.start'/1, 'Router.handle_jsonrpc'/3, 'Router.register'/3, 'ResponseWriter.start_batch'/1, 'ResponseWriter.close_batch'/1, 'ResponseWriter.close'/1, 'ResponseWriter.write'/2, 'ResponseWriter.write_empty'/1, 'ResponseWriter.write_error'/2]).

new_client(Cfg) ->
    #{stream => maps:get(stream, Cfg), interceptors => maps:get(interceptors, Cfg), {vbeam, type} => 'Client'}.

'Client.notify'(C, Method, Params) ->
    Req = new_request(Method, Params, <<"">>),
    intercept_request(maps:get(request, maps:get(interceptors, C)), Req),
    Enc_req = 'unknown.bytes'('unknown.encode'(Req)),
    intercept_encoded_request(maps:get(encoded_request, maps:get(interceptors, C)), Enc_req),
    'unknown.write'(maps:get(stream, C), Enc_req),
    ok.

'Client.request'(C, Method, Params, Id) ->
    Req = new_request(Method, Params, Id),
    intercept_request(maps:get(request, maps:get(interceptors, C)), Req),
    Enc_req = 'unknown.bytes'('unknown.encode'(Req)),
    intercept_encoded_request(maps:get(encoded_request, maps:get(interceptors, C)), Enc_req),
    'unknown.write'(maps:get(stream, C), Enc_req),
    Enc_resp = [],
    'unknown.read'(maps:get(stream, C), Enc_resp),
    intercept_encoded_response(maps:get(encoded_response, maps:get(interceptors, C)), Enc_resp),
    Resp = decode_response('unknown.bytestr'(Enc_resp)),
    intercept_response(maps:get(response, maps:get(interceptors, C)), Resp),
    Resp.

'Client.batch'(C, Reqs) ->
    Reqs_str = <<"[">>,
    lists:foreach(fun(Req) ->
        intercept_request(maps:get(request, maps:get(interceptors, C)), Req),
        Reqs_str1 = <<(<<(Reqs_str)/binary, ('Request.encode'(Req))/binary>>)/binary, (<<", ">>)/binary>>,
        ok
    end, Reqs),
    Reqs_str2 = <<('string.all_before_last'(Reqs_str1, <<", ">>))/binary, (<<"]">>)/binary>>,
    Enc_reqs = 'string.bytes'(Reqs_str2),
    intercept_encoded_request(maps:get(encoded_request, maps:get(interceptors, C)), Enc_reqs),
    'ReaderWriter.write'(maps:get(stream, C), Enc_reqs),
    Enc_resp = [],
    'ReaderWriter.read'(maps:get(stream, C), Enc_resp),
    intercept_encoded_response(maps:get(encoded_response, maps:get(interceptors, C)), Enc_resp),
    Resps = decode_batch_response('[]u8.bytestr'(Enc_resp)),
    lists:foreach(fun(Resp) ->
        intercept_response(maps:get(response, maps:get(interceptors, C)), Resp),
        ok
    end, Resps),
    Resps.

dispatch_event(Ints, Event_name, Data) ->
    lists:foreach(fun(I) ->
        i(Event_name, Data),
        ok.
        ok
    end, Ints),

intercept_encoded_request(Ints, Req) ->
    lists:foreach(fun(I) ->
        i(Req),
        ok
    end, Ints),
    ok.

intercept_request(Ints, Req) ->
    lists:foreach(fun(I) ->
        i(Req),
        ok
    end, Ints),
    ok.

intercept_response(Ints, Resp) ->
    lists:foreach(fun(I) ->
        i(Resp),
        ok.
        ok
    end, Ints),

intercept_encoded_response(Ints, Resp) ->
    lists:foreach(fun(I) ->
        i(Resp),
        ok.
        ok
    end, Ints),

'Server.is_interceptor_enabled'(S) ->
    'unknown.get_interceptor'(S),
    true.

'Interceptors.get_interceptor'(I) ->
    lists:foreach(fun(Inter) ->
        case Inter is todo of
            true -> Inter;
            false -> ok
        end,
        ok
    end, maps:get(event, I)),
    lists:foreach(fun(Inter) ->
        case Inter is todo of
            true -> Inter;
            false -> ok
        end,
        ok
    end, maps:get(encoded_request, I)),
    lists:foreach(fun(Inter) ->
        case Inter is todo of
            true -> Inter;
            false -> ok
        end,
        ok
    end, maps:get(request, I)),
    lists:foreach(fun(Inter) ->
        case Inter is todo of
            true -> Inter;
            false -> ok
        end,
        ok
    end, maps:get(response, I)),
    lists:foreach(fun(Inter) ->
        case Inter is todo of
            true -> Inter;
            false -> ok
        end,
        ok
    end, maps:get(encoded_response, I)),
    todo.

'ResponseError.code'(Err) ->
    maps:get(code, Err).

'ResponseError.msg'(Err) ->
    maps:get(message, Err).

'ResponseError.err'(E) ->
    todo.

response_error(Params) ->
    #{code => 'IError.code'(maps:get(error, Params)), message => 'IError.msg'(maps:get(error, Params)), data => maps:get(data, Params), {vbeam, type} => 'ResponseError'}.

error_with_code(Message, Code) ->
    #{code => Code, message => Message, data => <<"">>, {vbeam, type} => 'ResponseError'}.

'Null.str'(N) ->
    <<"null">>.

'Empty.str'(E) ->
    <<"">>.

new_request(Method, Params, Id) ->
    #{method => Method, params => try_encode(Params), id => Id, {vbeam, type} => 'Request'}.

'Request.encode'(Req) ->
    Params_payload = case length(maps:get(params, Req)) == 0 of
        true -> <<"">>;
        false -> <<",\"params\":", (maps:get(params, Req))/binary>>
    end,
    Id_payload = case length(maps:get(id, Req)) /= 0 of
        true -> <<",\"id\":\"", (maps:get(id, Req))/binary, "\"">>;
        false -> <<"">>
    end,
    <<"{\"jsonrpc\":\"", (<<"2.0">>)/binary, "\",\"method\":\"", (maps:get(method, Req))/binary, "\"", (Params_payload)/binary, (Id_payload)/binary, "}">>.

'Request.encode_batch'(Reqs) ->
    case length(Reqs) == 0 of
        true -> <<"[]">>;
        false -> begin
            S = <<(<<"[">>)/binary, ('Request.encode'(lists:nth(1, Reqs)))/binary>>,
            lists:foreach(fun(Req) ->
                S1 = <<(<<(S)/binary, (<<",">>)/binary>>)/binary, ('Request.encode'(Req))/binary>>,
                ok
            end, lists:nth(todo + 1, Reqs)),
            <<(S1)/binary, (<<"]">>)/binary>>
        end
        end.

'Request.decode_params'(Req) ->
    try_decode(maps:get(params, Req)).

decode_request(Raw) ->
    Json_payload = 'string.all_after'(Raw, <<"\\r\\n\\r\\n">>),
    decode(todo, Json_payload).

decode_batch_request(Raw) ->
    Json_payload = 'string.all_after'(Raw, <<"\\r\\n\\r\\n">>),
    decode(todo, Json_payload).

new_response(Result, Error, Id) ->
    #{result => case maps:get(code, Error) /= 0 of
        true -> <<"">>;
        false -> try_encode(Result)
    end, error => Error, id => Id, {vbeam, type} => 'Response'}.

'Response.encode'(Resp) ->
    S = <<"{\"jsonrpc\":\"", (<<"2.0">>)/binary, "\"">>,
    case maps:get(code, maps:get(error, Resp)) /= 0 of
        true -> ok;
        false -> ok
    end,
    S1 = <<(S)/binary, (<<",\"id\":">>)/binary>>,
    case length(maps:get(id, Resp)) == 0 of
        true -> ok;
        false -> ok
    end,
    <<(S1)/binary, (<<"}">>)/binary>>.

'Response.encode_batch'(Resps) ->
    case length(Resps) == 0 of
        true -> <<"[]">>;
        false -> begin
            S = <<(<<"[">>)/binary, ('Response.encode'(lists:nth(1, Resps)))/binary>>,
            lists:foreach(fun(Resp) ->
                S1 = <<(<<(S)/binary, (<<",">>)/binary>>)/binary, ('Response.encode'(Resp))/binary>>,
                ok
            end, lists:nth(todo + 1, Resps)),
            <<(S1)/binary, (<<"]">>)/binary>>
        end
        end.

'Response.decode_result'(Resp) ->
    try_decode(maps:get(result, Resp)).

decode_response(Raw) ->
    Json_payload = 'string.all_after'(Raw, <<"\\r\\n\\r\\n">>),
    decode(todo, Json_payload).

decode_batch_response(Raw) ->
    Json_payload = 'string.all_after'(Raw, <<"\\r\\n\\r\\n">>),
    decode(todo, Json_payload).

try_encode(Data) ->
    .

try_decode(S) ->

'LoggingInterceptor.on_event'(L, Name, Data) ->
    Msg = <<"[EVENT] name=", (Name)/binary, " data=", (Data)/binary>>,
    'Log.send_output'(maps:get(log, L), Msg, 'Log.get_level'(maps:get(log, L))),
    ok.

'LoggingInterceptor.on_encoded_request'(L, Req) ->
    Msg = <<"[RAW REQ] ", ('[]u8.bytestr'(Req))/binary>>,
    'Log.send_output'(maps:get(log, L), Msg, 'Log.get_level'(maps:get(log, L))),
    ok.

'LoggingInterceptor.on_request'(L, Req) ->
    Msg = <<"[REQ] method=", (maps:get(method, Req))/binary, " params=", (maps:get(params, Req))/binary, " id=", (maps:get(id, Req))/binary>>,
    'Log.send_output'(maps:get(log, L), Msg, 'Log.get_level'(maps:get(log, L))),
    ok.

'LoggingInterceptor.on_response'(L, Resp) ->
    Msg = <<"[RESP] result=", (maps:get(result, Resp))/binary, " ">>,
    case maps:get(code, maps:get(error, Resp)) /= 0 of
        true -> ok;
        false -> ok
    end,
    Msg1 = <<(Msg)/binary, (<<" id=", (maps:get(id, Resp))/binary>>)/binary>>,
    'Log.send_output'(maps:get(log, L), Msg1, 'Log.get_level'(maps:get(log, L))),
    ok.

'LoggingInterceptor.on_encoded_response'(L, Resp) ->
    Msg = <<"[RAW RESP] ", ('[]u8.bytestr'(Resp))/binary>>,
    'Log.send_output'(maps:get(log, L), Msg, 'Log.get_level'(maps:get(log, L))),
    ok.

new_server(Cfg) ->
    #{stream => maps:get(stream, Cfg), handler => maps:get(handler, Cfg), interceptors => maps:get(interceptors, Cfg), {vbeam, type} => 'Server'}.

'Server.respond'(S) ->
    Rw = 'Server.writer'(S),
    Rx = [],
    Bytes_read = 'ReaderWriter.read'(maps:get(stream, S), Rx),
    case Bytes_read == 0 of
        true -> ok;
        false -> begin
            intercept_encoded_request(maps:get(encoded_request, maps:get(interceptors, S)), Rx),
            Req_str = '[]u8.bytestr'(Rx),
            Req_batch = [],
            case 'u8.ascii_str'(lists:nth(1, Req_str)) of
                <<"[">> -> begin
                    Req_batch1 = decode_batch_request(Req_str),
                    'ResponseWriter.start_batch'(Rw)
                end;
                <<"{">> -> begin
                    Req = decode_request(Req_str),
                    'Request.prepend'(Req_batch1, Req)
                end;
                _ -> begin
                    'ResponseWriter.write_error'(Rw, response_error(#{error => error_with_code(<<"Invalid JSON.">>, -32700), {vbeam, type} => 'ResponseErrorGeneratorParams'})),
                    todo
                end
            end,
            lists:foreach(fun(Rq) ->
                intercept_request(maps:get(request, maps:get(interceptors, S)), Rq),
                'Server.handler'(S, Rq, Rw),
                ok
            end, Req_batch1),
            case length(Req_batch1) > 1 of
                true -> 'ResponseWriter.close_batch'(Rw);
                false -> ok
            end,
            ok
        end
        end.

'Server.writer'(S) ->
    #{writer => maps:get(stream, S), sb => new_builder(4096), server => S, {vbeam, type} => 'ResponseWriter'}.

'Server.start'(S) ->
    % TODO: unhandled stmt type
    ok
'Router.handle_jsonrpc'(R, Req, Wr) ->
    case todo of
        true -> ok;
        false -> 'ResponseWriter.write_error'(Wr, error_with_code(<<"Method not found.">>, -32601))
        end.

'Router.register'(R, Method, Handler) ->
    case lists:member(Method, maps:get(methods, R)) of
        true -> false;
        false -> begin
            true
        end
        end.

'ResponseWriter.start_batch'(Rw) ->
    'Builder.write_string'(maps:get(sb, Rw), <<"[">>),
    ok.

'ResponseWriter.close_batch'(Rw) ->
    'Builder.go_back'(maps:get(sb, Rw), 2),
    'Builder.write_string'(maps:get(sb, Rw), <<"]">>),
    'ResponseWriter.close'(Rw),
    ok.

'ResponseWriter.close'(Rw) ->
    intercept_encoded_response(maps:get(encoded_response, maps:get(interceptors, maps:get(server, Rw))), maps:get(sb, Rw)),
    'ReaderWriter.write'(maps:get(writer, Rw), maps:get(sb, Rw)),
    'Builder.go_back_to'(maps:get(sb, Rw), 0),
    ok.

'ResponseWriter.write'(Rw, Payload) ->
    Final_resp = #{id => maps:get(req_id, Rw), result => encode(Payload), {vbeam, type} => 'Response'},
    intercept_response(maps:get(response, maps:get(interceptors, maps:get(server, Rw))), Final_resp),
    case length(maps:get(req_id, Rw)) == 0 of
        true -> ok;
        false -> begin
            'Builder.write_string'(maps:get(sb, Rw), 'Response.encode'(Final_resp)),
            case maps:get(is_batch, Rw) == true of
                true -> ok;
                false -> 'ResponseWriter.close'(Rw)
                        end
        end
        end.

'ResponseWriter.write_empty'(Rw) ->
    'ResponseWriter.write'(Rw, #{{vbeam, type} => 'Null'}),
    ok.

'ResponseWriter.write_error'(Rw, Err) ->
    Res_err = Err,
    case Err !is todo of
        true -> case (not lists:member('IError.code'(Err), ['ResponseError.code'(error_with_code(<<"Invalid JSON.">>, -32700)), 'ResponseError.code'(error_with_code(<<"Invalid request.">>, -32600)), 'ResponseError.code'(error_with_code(<<"Method not found.">>, -32601)), 'ResponseError.code'(error_with_code(<<"Invalid params">>, -32602)), 'ResponseError.code'(error_with_code(<<"Internal error.">>, -32693)), 'ResponseError.code'(error_with_code(<<"Error occurred when starting server.">>, -32099)), 'ResponseError.code'(error_with_code(<<"Server not initialized.">>, -32002)), 'ResponseError.code'(error_with_code(<<"Error occurred when stopping the server.">>, -32000)), 'ResponseError.code'(error_with_code(<<"Unknown error.">>, -32001))])) of
            true -> ok;
            false -> ok
        end;
        false -> ok
    end,
    Final_resp = #{id => maps:get(req_id, Rw), error => todo, {vbeam, type} => 'Response'},
    intercept_response(maps:get(response, maps:get(interceptors, maps:get(server, Rw))), Final_resp),
    'Builder.write_string'(maps:get(sb, Rw), 'Response.encode'(Final_resp)),
    case maps:get(is_batch, Rw) of
        true -> ok;
        false -> 'ResponseWriter.close'(Rw)
        end.
