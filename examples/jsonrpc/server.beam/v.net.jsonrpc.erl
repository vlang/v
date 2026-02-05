-module('v.net.jsonrpc').
-export([new_server/1, 'Server.respond'/1, 'Server.writer'/1, 'Server.start'/1, 'Router.handle_jsonrpc'/3, 'Router.register'/3, 'ResponseWriter.start_batch'/1, 'ResponseWriter.close_batch'/1, 'ResponseWriter.close'/1, 'ResponseWriter.write'/2, 'ResponseWriter.write_empty'/1, 'ResponseWriter.write_error'/2]).
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]

new_server(Cfg) ->
    #{stream => maps:get(stream, Cfg), handler => maps:get(handler, Cfg), interceptors => maps:get(interceptors, Cfg), {vbeam, type} => 'Server'}.

'Server.respond'(S) ->
    Rw = 'Server.writer'(S),
    Rx = [],
    Bytes_read = 'ReaderWriter.read'(maps:get(stream, S), Rx),
    case Bytes_read == 0 of
        true -> ok;
        false -> ok
    end,
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
        intercept_request(maps:get(request, maps:get(interceptors, S)), &Rq),
        'Server.handler'(S, &Rq, Rw),
        ok
    end, Req_batch1),
    case length(Req_batch1) > 1 of
        true -> 'ResponseWriter.close_batch'(Rw);
        false -> ok
    end,
    ok.

'Server.writer'(S) ->
    &#{writer => maps:get(stream, S), sb => new_builder(4096), server => S, {vbeam, type} => 'ResponseWriter'}.

'Server.start'(S) ->
    % TODO: for {

'Router.handle_jsonrpc'(R, Req, Wr) ->
    case todo of
        true -> begin
            h(Req, Wr),
        end;
        false -> ok
    end,
    'ResponseWriter.write_error'(Wr, error_with_code(<<"Method not found.">>, -32601)),
    ok.

'Router.register'(R, Method, Handler) ->
    case Method in maps:get(methods, R) of
        true -> false;
        false -> ok
    end,
    true.

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
        false -> ok
    end,
    'Builder.write_string'(maps:get(sb, Rw), 'Response.encode'(Final_resp)),
    case maps:get(is_batch, Rw) == true of
        true -> begin
            'Builder.write_string'(maps:get(sb, Rw), <<", ">>),
        end;
        false -> ok
    end,
    'ResponseWriter.close'(Rw),
    ok.

'ResponseWriter.write_empty'(Rw) ->
    'ResponseWriter.write'(Rw, #{{vbeam, type} => 'Null'}),
    ok.

'ResponseWriter.write_error'(Rw, Err) ->
    Res_err = Err,
    case Err !is todo of
        true -> case 'IError.code'(Err) !in ['ResponseError.code'(error_with_code(<<"Invalid JSON.">>, -32700)), 'ResponseError.code'(error_with_code(<<"Invalid request.">>, -32600)), 'ResponseError.code'(error_with_code(<<"Method not found.">>, -32601)), 'ResponseError.code'(error_with_code(<<"Invalid params">>, -32602)), 'ResponseError.code'(error_with_code(<<"Internal error.">>, -32693)), 'ResponseError.code'(error_with_code(<<"Error occurred when starting server.">>, -32099)), 'ResponseError.code'(error_with_code(<<"Server not initialized.">>, -32002)), 'ResponseError.code'(error_with_code(<<"Error occurred when stopping the server.">>, -32000)), 'ResponseError.code'(error_with_code(<<"Unknown error.">>, -32001))] of
            true -> ok;
            false -> ok
        end;
        false -> ok
    end,
    Final_resp = #{id => maps:get(req_id, Rw), error => todo, {vbeam, type} => 'Response'},
    intercept_response(maps:get(response, maps:get(interceptors, maps:get(server, Rw))), Final_resp),
    'Builder.write_string'(maps:get(sb, Rw), 'Response.encode'(Final_resp)),
    case maps:get(is_batch, Rw) of
        true -> begin
            'Builder.write_string'(maps:get(sb, Rw), <<", ">>),
        end;
        false -> ok
    end,
    'ResponseWriter.close'(Rw),
    ok.
