-module('v.main').
-export(['KvStore.create'/3, 'KvStore.get'/2, 'KvStore.update'/3, 'KvStore.delete'/2, 'KvStore.dump'/1, 'KvHandler.handle_create'/3, 'KvHandler.handle_get'/3, 'KvHandler.handle_update'/3, 'KvHandler.handle_delete'/3, 'KvHandler.handle_list'/3, handle_conn/2, main/0]).

'KvStore.create'(S, Key, Value) ->
    'RwMutex.lock'(maps:get(mu, S)),
    % TODO: defer {s.mu.unlock();}
    case Key in maps:get(store, S) of
        true -> false;
        false -> ok
    end,
    true.

'KvStore.get'(S, Key) ->
    'RwMutex.rlock'(maps:get(mu, S)),
    % TODO: defer {s.mu.runlock();}
    case todo of
        true -> Value;
        false -> ok
    end,
    todo.

'KvStore.update'(S, Key, Value) ->
    'RwMutex.lock'(maps:get(mu, S)),
    % TODO: defer {s.mu.unlock();}
    case Key in maps:get(store, S) of
        true -> begin
            true
        end;
        false -> ok
    end,
    false.

'KvStore.delete'(S, Key) ->
    'RwMutex.lock'(maps:get(mu, S)),
    % TODO: defer {s.mu.unlock();}
    case Key in maps:get(store, S) of
        true -> begin
            'map[string]string.delete'(maps:get(store, S), Key),
            true
        end;
        false -> ok
    end,
    false.

'KvStore.dump'(S) ->
    maps:get(store, S).

'KvHandler.handle_create'(H, Req, Wr) ->
    P = 'Request.decode_params'(Req),
    case length(maps:get(key, P)) == 0 of
        true -> begin
            'ResponseWriter.write_error'(Wr, 'net.jsonrpc.error_with_code'(<<"Invalid params">>, -32602)),
        end;
        false -> ok
    end,
    warn(<<"params=", (P)/binary>>),
    case !'KvStore.create'(maps:get(store, H), maps:get(key, P), maps:get(value, P)) of
        true -> begin
            'ResponseWriter.write_error'(Wr, #{code => -32010, message => <<"Key already exists">>, data => maps:get(key, P), {vbeam, type} => 'ResponseError'}),
        end;
        false -> ok
    end,
    'ResponseWriter.write'(Wr, #{<<"ok">> => true}),
    ok.

'KvHandler.handle_get'(H, Req, Wr) ->
    P = 'Request.decode_params'(Req),
    Value = 'KvStore.get'(maps:get(store, H), maps:get(key, P)),
    'ResponseWriter.write'(Wr, #{key => maps:get(key, P), value => Value, {vbeam, type} => 'KvItem'}),
    ok.

'KvHandler.handle_update'(H, Req, Wr) ->
    P = 'Request.decode_params'(Req),
    case !'KvStore.update'(maps:get(store, H), maps:get(key, P), maps:get(value, P)) of
        true -> begin
            'ResponseWriter.write_error'(Wr, #{code => -32004, message => <<"Not found">>, data => maps:get(key, P), {vbeam, type} => 'ResponseError'}),
        end;
        false -> ok
    end,
    'ResponseWriter.write'(Wr, #{<<"ok">> => true}),
    ok.

'KvHandler.handle_delete'(H, Req, Wr) ->
    P = 'Request.decode_params'(Req),
    case !'KvStore.delete'(maps:get(store, H), maps:get(key, P)) of
        true -> begin
            'ResponseWriter.write_error'(Wr, #{code => -32004, message => <<"Not found">>, data => maps:get(key, P), {vbeam, type} => 'ResponseError'}),
        end;
        false -> ok
    end,
    'ResponseWriter.write'(Wr, #{<<"ok">> => true}),
    ok.

'KvHandler.handle_list'(H, _req, Wr) ->
    Items = [],
    lists:foreach(fun(V) ->
        Items << #{key => K, value => V, {vbeam, type} => 'KvItem'},
        ok
    end, 'KvStore.dump'(maps:get(store, H))),
    'KvItem.sort'(Items, maps:get(key, A) < maps:get(key, B)),
    'ResponseWriter.write'(Wr, Items),
    ok.

handle_conn(Conn, H) ->
    % TODO: defer {conn.close();}
    Log_inter = #{{vbeam, type} => 'LoggingInterceptor'},
    Inters = #{event => [maps:get(on_event, Log_inter)], encoded_request => [maps:get(on_encoded_request, Log_inter)], request => [maps:get(on_request, Log_inter)], response => [maps:get(on_response, Log_inter)], encoded_response => [maps:get(on_encoded_response, Log_inter)], {vbeam, type} => 'Interceptors'},
    Srv = new_server(#{stream => Conn, handler => H, interceptors => Inters, {vbeam, type} => 'ServerConfig'}),
    dispatch_event(maps:get(event, Inters), <<"start">>, <<"server started">>),
    'Server.start'(Srv),
    ok.

main() ->
    S = #{{vbeam, type} => 'KvStore'},
    H = #{store => S, {vbeam, type} => 'KvHandler'},
    R = #{{vbeam, type} => 'Router'},
    'Router.register'(R, <<"kv.create">>, maps:get(handle_create, H)),
    'Router.register'(R, <<"kv.get">>, maps:get(handle_get, H)),
    'Router.register'(R, <<"kv.update">>, maps:get(handle_update, H)),
    'Router.register'(R, <<"kv.delete">>, maps:get(handle_delete, H)),
    'Router.register'(R, <<"kv.list">>, maps:get(handle_list, H)),
    Addr = <<"127.0.0.1:42228">>,
    L = listen_tcp(ip, Addr),
    vbeam_io:println(<<"TCP JSON-RPC server on ", (Addr)/binary, " (Content-Length framing)">>),
    % TODO: for {
