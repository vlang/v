-module('v.net.http').
-export(['Request.ssl_do'/5, read_from_ssl_connection_cb/3, 'Request.do_request'/3, read_cookies/2, 'Cookie.str'/1, sanitize/2, sanitize_cookie_name/1, sanitize_cookie_value/1, sanitize_cookie_path/1, valid_cookie_value_byte/1, valid_cookie_path_byte/1, valid_cookie_domain/1, is_cookie_domain_name/1, parse_cookie_value/2, is_cookie_name_valid/1, parse_cookie/1, download_file/2, download_file_with_cookies/3, download_file_with_progress/3, download_progres_cb/5, 'SilentStreamingDownloader.on_start'/3, 'SilentStreamingDownloader.on_chunk'/5, 'SilentStreamingDownloader.on_finish'/3, 'TerminalStreamingDownloader.on_start'/3, 'TerminalStreamingDownloader.on_chunk'/5, 'TerminalStreamingDownloader.on_finish'/3, 'CommonHeader.str'/1, 'Header.free'/1, new_header/1, new_header_from_map/1, new_custom_header_from_map/1, 'Header.add'/3, 'Header.add_custom'/3, 'Header.add_map'/2, 'Header.add_custom_map'/2, 'Header.set'/3, 'Header.set_custom'/3, 'Header.delete'/2, 'Header.delete_custom'/2, 'Header.contains'/2, 'Header.contains_custom'/3, 'Header.get'/2, 'Header.get_custom'/3, 'Header.starting_with'/2, 'Header.values'/2, 'Header.custom_values'/3, 'Header.keys'/1, 'Header.render'/2, 'Header.render_into_sb'/3, 'Header.join'/2, canonicalize/1, 'HeaderKeyError.msg'/1, 'HeaderKeyError.code'/1, is_valid/1, is_token/1, 'Header.str'/1, parse_headers/1, parse_header/1, parse_header_fast/1, new_request/3, get/1, post/2, post_json/2, post_form/2, post_form_with_cookies/3, post_multipart_form/2, put/2, patch/2, head/1, delete/1, prepare/1, fetch/1, get_text/1, url_encode_form_data/1, build_url_from_fetch/1, new_http_proxy/1, 'HttpProxy.build_proxy_headers'/2, 'HttpProxy.http_do'/5, 'HttpProxy.dial'/2, 'HttpProxy.ssl_dial'/2, 'Method.str'/1, method_from_str/1, 'Request.free'/1, 'Request.add_header'/3, 'Request.add_custom_header'/3, 'Request.add_cookie'/2, 'Request.cookie'/2, 'Request.do'/1, 'Request.method_and_url_to_response'/3, 'Request.build_request_headers'/5, 'Request.build_request_cookies_header'/1, 'Request.http_do'/4, 'Request.receive_all_data_from_cb_in_builder'/4, read_from_tcp_connection_cb/3, 'Request.read_all_from_client_connection'/2, 'Request.referer'/1, parse_request/1, parse_request_head/1, parse_request_head_str/1, parse_request_str/1, parse_request_line/1, parse_form/1, 'UnexpectedExtraAttributeError.msg'/1, 'MultiplePathAttributesError.msg'/1, multipart_form_body/2, parse_multipart_form/2, parse_disposition/1, is_no_need_retry_error/1, 'Response.free'/1, 'Response.bytes'/1, 'Response.bytestr'/1, parse_response/1, parse_status_line/1, 'Response.cookies'/1, 'Response.status'/1, 'Response.set_status'/2, 'Response.version'/1, 'Response.set_version'/2, new_response/1, find_headers_range/1, 'Server.listen_and_serve'/1, 'Server.stop'/1, 'Server.close'/1, 'Server.status'/1, 'Server.wait_till_running'/2, new_handler_worker/4, 'HandlerWorker.process_requests'/1, 'HandlerWorker.handle_conn'/2, 'DebugHandler.handle'/2, status_from_int/1, 'Status.str'/1, 'Status.int'/1, 'Status.is_valid'/1, 'Status.is_error'/1, 'Status.is_success'/1, fast_request_words/1, 'Version.str'/1, version_from_str/1, 'Version.protos'/1, 'SameSite__static__from'/1, 'CommonHeader__static__from'/1, 'Method__static__from'/1, 'ServerStatus__static__from'/1, 'Status__static__from'/1, 'Version__static__from'/1]).

'Request.ssl_do'(Req, Port, Method, Host_name, Path) ->
    Ssl_conn = new_ssl_conn(#{verify => maps:get(verify, Req), cert => maps:get(cert, Req), cert_key => maps:get(cert_key, Req), validate => maps:get(validate, Req), in_memory_verification => maps:get(in_memory_verification, Req), {vbeam, type} => 'SSLConnectConfig'}),
    Retries = 0,
    % TODO: unhandled stmt type
    Req_headers = 'Request.build_request_headers'(Req, Method, Host_name, Port, Path),
    % TODO: unhandled stmt type
    'Request.do_request'(Req, Req_headers, Ssl_conn).

read_from_ssl_connection_cb(Con, Buf, Bufsize) ->
    Ssl_conn = todo,
    'SSLConn.socket_read_into_ptr'(Ssl_conn, Buf, Bufsize).

'Request.do_request'(Req, Req_headers, Ssl_conn) ->
    'SSLConn.write_string'(Ssl_conn, Req_headers),
    Content = new_builder(4096),
    'Request.receive_all_data_from_cb_in_builder'(Req, Content, todo, Net.http.read_from_ssl_connection_cb),
    'SSLConn.shutdown'(Ssl_conn),
    Response_text = 'Builder.str'(Content),
    % TODO: unhandled stmt type
    case maps:get(on_finish, Req) /= todo of
        true -> 'Request.on_finish'(Req, Req, todo);
        false -> ok
    end,
    parse_response(Response_text).

read_cookies(H, Filter) ->
    Lines = 'Header.values'(H, cookie),
    case length(Lines) == 0 of
        true -> [];
        false -> begin
            Cookies = [],
            lists:foreach(fun(Line_) ->
                Line = string:trim(Line_),
                Part = <<"">>,
                % TODO: unhandled stmt type
                ok
            end, Lines),
            Cookies
        end
        end.

'Cookie.str'(C) ->
    case not is_cookie_name_valid(maps:get(name, C)) of
        true -> <<"">>;
        false -> begin
            Extra_cookie_length = 110,
            B = new_builder(length(maps:get(name, C)) + length(maps:get(value, C)) + length(maps:get(domain, C)) + length(maps:get(path, C)) + Extra_cookie_length),
            'Builder.write_string'(B, maps:get(name, C)),
            'Builder.write_string'(B, <<"=">>),
            'Builder.write_string'(B, sanitize_cookie_value(maps:get(value, C))),
            case length(maps:get(path, C)) > 0 of
                true -> begin
                    'Builder.write_string'(B, <<"; path=">>),
                    'Builder.write_string'(B, sanitize_cookie_path(maps:get(path, C)))
                end;
                false -> ok
            end,
            case length(maps:get(domain, C)) > 0 of
                true -> case valid_cookie_domain(maps:get(domain, C)) of
                    true -> begin
                        D = maps:get(domain, C),
                        case lists:nth(1, D) == todo of
                            true -> ok;
                            false -> ok
                        end,
                        'Builder.write_string'(B, <<"; domain=">>),
                        'Builder.write_string'(B, D)
                    end;
                    false -> ok
                end;
                false -> ok
            end,
            case maps:get(year, maps:get(expires, C)) > 1600 of
                true -> begin
                    Time_str = 'Time.http_header_string'(maps:get(expires, C)),
                    'Builder.write_string'(B, <<"; expires=">>),
                    'Builder.write_string'(B, Time_str)
                end;
                false -> ok
            end,
            case maps:get(max_age, C) > 0 of
                true -> begin
                    'Builder.write_string'(B, <<"; Max-Age=">>),
                    'Builder.write_string'(B, integer_to_binary(maps:get(max_age, C)))
                end;
                false -> case maps:get(max_age, C) < 0 of
                    true -> 'Builder.write_string'(B, <<"; Max-Age=0">>);
                    false -> ok
                end
            end,
            case maps:get(http_only, C) of
                true -> 'Builder.write_string'(B, <<"; HttpOnly">>);
                false -> ok
            end,
            case maps:get(secure, C) of
                true -> 'Builder.write_string'(B, <<"; Secure">>);
                false -> ok
            end,
            case maps:get(same_site, C) of
                same_site_not_set -> ok;
                same_site_default_mode -> 'Builder.write_string'(B, <<"; SameSite">>);
                same_site_none_mode -> 'Builder.write_string'(B, <<"; SameSite=None">>);
                same_site_lax_mode -> 'Builder.write_string'(B, <<"; SameSite=Lax">>);
                same_site_strict_mode -> 'Builder.write_string'(B, <<"; SameSite=Strict">>)
            end,
            'Builder.str'(B)
        end
        end.

sanitize(Valid, V) ->
    Ok = true,
    Ok1 = lists:foldl(fun(I, OkAcc) ->
        case valid(lists:nth(I + 1, V)) of
            true -> ok;
            false -> ok
        end,
        OkOut = false,
        % TODO: unhandled stmt type
        OkOut
    end, Ok, lists:seq(0, length(V) - 1)),
    case Ok1 of
        true -> 'string.clone'(V);
        false -> '[]u8.bytestr'(lists:filter(valid(It), binary_to_list(V)))
        end.

sanitize_cookie_name(Name) ->
    'string.replace_each'(Name, [<<"\\n">>, <<"-">>, <<"\\r">>, <<"-">>]).

sanitize_cookie_value(V) ->
    Val = sanitize(Net.http.valid_cookie_value_byte, V),
    case length(V) == 0 of
        true -> V;
        false -> 
            case case string:prefix(Val, <<" ">>) of nomatch -> false; _ -> true end orelse case binary:match(V, <<";">>) of nomatch -> false; _ -> true end orelse case binary:longest_common_suffix([Val, <<" ">>]) of 0 -> false; _ -> true end orelse case string:prefix(Val, <<",">>) of nomatch -> false; _ -> true end orelse case binary:longest_common_suffix([Val, <<",">>]) of 0 -> false; _ -> true end of
                true -> <<"\"", (V)/binary, "\"">>;
                false -> V
                        end
                end.

sanitize_cookie_path(V) ->
    sanitize(Net.http.valid_cookie_path_byte, V).

valid_cookie_value_byte(B) ->
    16#20 =< B andalso B < 16#7f andalso B /= todo andalso B /= todo andalso B /= todo.

valid_cookie_path_byte(B) ->
    16#20 =< B andalso B < 16#7f andalso B /= todo.

valid_cookie_domain(V) ->
    case is_cookie_domain_name(V) of
        true -> true;
        false -> false
        end.

is_cookie_domain_name(_s) ->
    S = _s,
    case length(S) == 0 of
        true -> false;
        false -> 
            case length(S) > 255 of
                true -> false;
                false -> begin
                    case lists:nth(1, S) == todo of
                        true -> ok;
                        false -> ok
                    end,
                    Last = todo,
                    Ok = false,
                    Part_len = 0,
                    lists:foreach(fun(_) ->
                        C = lists:nth(I + 1, S),
                        case 'u8.is_letter'(C) of
                            true -> begin
                                Ok1 = true,
                                todo
                            end;
                            false -> case todo =< C andalso C =< todo of
                                true -> todo;
                                false -> case C == todo of
                                    true -> begin
                                        case Last == todo of
                                            true -> false;
                                            false -> ok
                                        end,
                                        todo
                                    end;
                                    false -> case C == todo of
                                        true -> begin
                                            case Last == todo orelse Last == todo of
                                                true -> false;
                                                false -> ok
                                            end,
                                            case Part_len > 63 orelse Part_len == 0 of
                                                true -> false;
                                                false -> ok
                                            end,
                                            Part_len1 = 0,
                                        end;
                                        false -> false
                                    end
                                end
                            end
                        end,
                        Last1 = C,
                        ok
                    end, S),
                    case Last1 == todo orelse Part_len1 > 63 of
                        true -> false;
                        false -> Ok1
                                        end
                end
                        end
                end.

parse_cookie_value(_raw, Allow_double_quote) ->
    Raw = _raw,
    case Allow_double_quote andalso length(Raw) > 1 andalso lists:nth(1, Raw) == todo andalso lists:nth(length(Raw) - 1 + 1, Raw) == todo of
        true -> ok;
        false -> ok
    end,
    lists:foreach(fun(I) ->
        case not valid_cookie_value_byte(lists:nth(I + 1, Raw)) of
            true -> error(<<"http.cookie: invalid cookie value">>);
            false -> ok
        end,
        ok
    end, lists:seq(0, length(Raw) - 1)),
    Raw.

is_cookie_name_valid(Name) ->
    case Name == <<"">> of
        true -> false;
        false -> begin
            lists:foreach(fun(B) ->
                case B < 33 orelse B > 126 of
                    true -> false;
                    false -> ok
                end,
                ok
            end, Name),
            true
        end
        end.

parse_cookie(Line) ->
    Parts = binary:split(string:trim(Line), <<";">>, [global]),
    case length(Parts) == 1 andalso lists:nth(1, Parts) == <<"">> of
        true -> error(<<"malformed cookie">>);
        false -> begin
            Index = 'string.index'(lists:nth(1, Parts), <<"=">>),
            Name = lists:nth(todo + 1, lists:nth(1, Parts)),
            Raw_value = lists:nth(todo + 1, lists:nth(1, Parts)),
            case not is_cookie_name_valid(Name) of
                true -> error(<<"malformed cookie">>);
                false -> begin
                    Value = parse_cookie_value(Raw_value, true),
                    C = #{name => Name, value => Value, raw => Line, {vbeam, type} => 'Cookie'},
                    lists:foreach(fun(_) ->
                        case length(lists:nth(I + 1, Parts)) == 0 of
                            true -> ok;
                            false -> ok
                        end,
                        Attr = lists:nth(I + 1, Parts),
                        Raw_val = <<"">>,
                        case todo of
                            true -> begin
                                Attr1 = lists:nth(todo + 1, lists:nth(I + 1, Parts)),
                                Raw_val1 = lists:nth(todo + 1, lists:nth(I + 1, Parts)),
                            end;
                            false -> ok
                        end,
                        Lower_attr = string:lowercase(Attr1),
                        Val = parse_cookie_value(Raw_val1, false),
                        case Lower_attr of
                            <<"samesite">> -> begin
                                Lower_val = string:lowercase(Val),
                                case Lower_val of
                                    <<"lax">> -> ok;
                                    <<"strict">> -> ok;
                                    <<"none">> -> ok;
                                    _ -> ok
                                end
                            end;
                            <<"secure">> -> begin
                                % TODO: unhandled stmt type
                            end;
                            <<"httponly">> -> begin
                                % TODO: unhandled stmt type
                            end;
                            <<"domain">> -> begin
                                % TODO: unhandled stmt type
                            end;
                            <<"max-age">> -> begin
                                Secs = binary_to_integer(Val),
                                case Secs /= 0 andalso lists:nth(1, Val) /= todo of
                                    true -> ok;
                                    false -> ok
                                end,
                                case Secs =< 0 of
                                    true -> ok;
                                    false -> ok
                                end,
                                % TODO: unhandled stmt type
                            end;
                            <<"path">> -> begin
                                % TODO: unhandled stmt type
                            end;
                            _ -> maps:get(unparsed, C) bsl lists:nth(I + 1, Parts)
                        end,
                        ok
                    end, Parts),
                    C
                end
                        end
        end
        end.

download_file(Url, Out_file_path) ->
    % TODO: unhandled stmt type
    S = get(Url),
    case 'Response.status'(S) /= ok of
        true -> error_with_code(maps:get(body, S), maps:get(status_code, S));
        false -> begin
            % TODO: unhandled stmt type
            write_file(Out_file_path, maps:get(body, S)),
            ok
        end
        end.

download_file_with_cookies(Url, Out_file_path, Cookies) ->
    % TODO: unhandled stmt type
    S = fetch(#{method => get, url => Url, cookies => Cookies, {vbeam, type} => 'FetchConfig'}),
    case 'Response.status'(S) /= ok of
        true -> error(<<"received http code ", (integer_to_binary(maps:get(status_code, S)))/binary>>);
        false -> begin
            % TODO: unhandled stmt type
            write_file(Out_file_path, maps:get(body, S)),
            ok
        end
        end.

download_file_with_progress(Url, Path, Params) ->
    D = todo,
    Config = maps:get(FetchConfig, Params),
    case maps:get(stop_copying_limit, Config) == -1 of
        true -> ok;
        false -> ok
    end,
    Req = prepare(Config),
    'Downloader.on_start'(D, Req, Path),
    Response = 'Request.do'(Req),
    % TODO: unhandled stmt type
    'Downloader.on_finish'(D, Req, Response),
    Response.

download_progres_cb(Request, Chunk, Body_so_far, Expected_size, Status_code) ->
    D = todo,
    Pd = todo,
    % TODO: unhandled stmt type
    case Status_code == 200 of
        true -> 'Downloader.on_chunk'(D, Request, Chunk, Body_so_far, Expected_size);
        false -> ok
    end,
    ok.

'SilentStreamingDownloader.on_start'(D, Request, Path) ->
    ok.

'SilentStreamingDownloader.on_chunk'(D, Request, Chunk, Already_received, Expected) ->
    'File.write'(maps:get(f, D), Chunk),
    ok.

'SilentStreamingDownloader.on_finish'(D, Request, Response) ->
    'File.close'(maps:get(f, D)),
    ok.

'TerminalStreamingDownloader.on_start'(D, Request, Path) ->
    'SilentStreamingDownloader.on_start'(maps:get(SilentStreamingDownloader, D), Request, Path),
    ok.

'TerminalStreamingDownloader.on_chunk'(D, Request, Chunk, Already_received, Expected) ->
    Now = now(),
    Elapsed = Now - maps:get(start_time, D),
    Ratio = todo / todo,
    Res = todo / Ratio,
    Estimated = todo,
    case todo < Res andalso Res < todo of
        true -> ok;
        false -> ok
    end,
    Speed = todo * todo / todo,
    Elapsed_s = 'Duration.seconds'(Elapsed),
    Estimated_s = 'Duration.seconds'(Estimated),
    Eta_s = f64_max(Estimated_s - Elapsed_s, 0.0),
    'SilentStreamingDownloader.on_chunk'(maps:get(SilentStreamingDownloader, D), Request, Chunk, Already_received, Expected),
    io:format("~s", [<<"\\rDownloading to `", (maps:get(path, D))/binary, "` ", (float_to_binary(todo * Ratio))/binary, "%, ", (float_to_binary(todo / (1024 * 1024)))/binary, "/", (float_to_binary(todo / (1024 * 1024)))/binary, "MB, ", (float_to_binary(Speed))/binary, "KB/s, elapsed: ", (float_to_binary(Elapsed_s))/binary, "s, eta: ", (float_to_binary(Eta_s))/binary, "s">>]),
    flush_stdout(),
    ok.

'TerminalStreamingDownloader.on_finish'(D, Request, Response) ->
    'SilentStreamingDownloader.on_finish'(maps:get(SilentStreamingDownloader, D), Request, Response),
    io:format("~s~n", [<<"">>]),
    flush_stdout(),
    ok.

'CommonHeader.str'(H) ->
    case H of
        accept -> <<"Accept">>;
        accept_ch -> <<"Accept-CH">>;
        accept_charset -> <<"Accept-Charset">>;
        accept_ch_lifetime -> <<"Accept-CH-Lifetime">>;
        accept_encoding -> <<"Accept-Encoding">>;
        accept_language -> <<"Accept-Language">>;
        accept_patch -> <<"Accept-Patch">>;
        accept_post -> <<"Accept-Post">>;
        accept_ranges -> <<"Accept-Ranges">>;
        access_control_allow_credentials -> <<"Access-Control-Allow-Credentials">>;
        access_control_allow_headers -> <<"Access-Control-Allow-Headers">>;
        access_control_allow_methods -> <<"Access-Control-Allow-Methods">>;
        access_control_allow_origin -> <<"Access-Control-Allow-Origin">>;
        access_control_expose_headers -> <<"Access-Control-Expose-Headers">>;
        access_control_max_age -> <<"Access-Control-Max-Age">>;
        access_control_request_headers -> <<"Access-Control-Request-Headers">>;
        access_control_request_method -> <<"Access-Control-Request-Method">>;
        age -> <<"Age">>;
        allow -> <<"Allow">>;
        alt_svc -> <<"Alt-Svc">>;
        authorization -> <<"Authorization">>;
        authority -> <<"Authority">>;
        cache_control -> <<"Cache-Control">>;
        clear_site_data -> <<"Clear-Site-Data">>;
        connection -> <<"Connection">>;
        content_disposition -> <<"Content-Disposition">>;
        content_encoding -> <<"Content-Encoding">>;
        content_language -> <<"Content-Language">>;
        content_length -> <<"Content-Length">>;
        content_location -> <<"Content-Location">>;
        content_range -> <<"Content-Range">>;
        content_security_policy -> <<"Content-Security-Policy">>;
        content_security_policy_report_only -> <<"Content-Security-Policy-Report-Only">>;
        content_type -> <<"Content-Type">>;
        cookie -> <<"Cookie">>;
        cross_origin_embedder_policy -> <<"Cross-Origin-Embedder-Policy">>;
        cross_origin_opener_policy -> <<"Cross-Origin-Opener-Policy">>;
        cross_origin_resource_policy -> <<"Cross-Origin-Resource-Policy">>;
        date -> <<"Date">>;
        device_memory -> <<"Device-Memory">>;
        digest -> <<"Digest">>;
        dnt -> <<"DNT">>;
        early_data -> <<"Early-Data">>;
        etag -> <<"ETag">>;
        expect -> <<"Expect">>;
        expect_ct -> <<"Expect-CT">>;
        expires -> <<"Expires">>;
        feature_policy -> <<"Feature-Policy">>;
        forwarded -> <<"Forwarded">>;
        from -> <<"From">>;
        host -> <<"Host">>;
        if_match -> <<"If-Match">>;
        if_modified_since -> <<"If-Modified-Since">>;
        if_none_match -> <<"If-None-Match">>;
        if_range -> <<"If-Range">>;
        if_unmodified_since -> <<"If-Unmodified-Since">>;
        index -> <<"Index">>;
        keep_alive -> <<"Keep-Alive">>;
        large_allocation -> <<"Large-Allocation">>;
        last_modified -> <<"Last-Modified">>;
        link -> <<"Link">>;
        location -> <<"Location">>;
        nel -> <<"NEL">>;
        origin -> <<"Origin">>;
        pragma -> <<"Pragma">>;
        proxy_authenticate -> <<"Proxy-Authenticate">>;
        proxy_authorization -> <<"Proxy-Authorization">>;
        range -> <<"Range">>;
        referer -> <<"Referer">>;
        referrer_policy -> <<"Referrer-Policy">>;
        retry_after -> <<"Retry-After">>;
        save_data -> <<"Save-Data">>;
        sec_fetch_dest -> <<"Sec-Fetch-Dest">>;
        sec_fetch_mode -> <<"Sec-Fetch-Mode">>;
        sec_fetch_site -> <<"Sec-Fetch-Site">>;
        sec_fetch_user -> <<"Sec-Fetch-User">>;
        sec_websocket_accept -> <<"Sec-WebSocket-Accept">>;
        sec_websocket_key -> <<"Sec-WebSocket-Key">>;
        server -> <<"Server">>;
        server_timing -> <<"Server-Timing">>;
        set_cookie -> <<"Set-Cookie">>;
        sourcemap -> <<"SourceMap">>;
        strict_transport_security -> <<"Strict-Transport-Security">>;
        te -> <<"TE">>;
        timing_allow_origin -> <<"Timing-Allow-Origin">>;
        tk -> <<"Tk">>;
        trailer -> <<"Trailer">>;
        transfer_encoding -> <<"Transfer-Encoding">>;
        upgrade -> <<"Upgrade">>;
        upgrade_insecure_requests -> <<"Upgrade-Insecure-Requests">>;
        user_agent -> <<"User-Agent">>;
        vary -> <<"Vary">>;
        via -> <<"Via">>;
        want_digest -> <<"Want-Digest">>;
        warning -> <<"Warning">>;
        www_authenticate -> <<"WWW-Authenticate">>;
        x_content_type_options -> <<"X-Content-Type-Options">>;
        x_dns_prefetch_control -> <<"X-DNS-Prefetch-Control">>;
        x_forwarded_for -> <<"X-Forwarded-For">>;
        x_forwarded_host -> <<"X-Forwarded-Host">>;
        x_forwarded_proto -> <<"X-Forwarded-Proto">>;
        x_frame_options -> <<"X-Frame-Options">>;
        x_xss_protection -> <<"X-XSS-Protection">>
    end.

'Header.free'(H) ->
    % TODO: unhandled stmt type
        ok.

new_header(Kvs) ->
    H = #{{vbeam, type} => 'Header'},
    lists:foreach(fun(Kv) ->
        ok
    end, Kvs),
    H.

new_header_from_map(Kvs) ->
    H = new_header(),
    'Header.add_map'(H, Kvs),
    H.

new_custom_header_from_map(Kvs) ->
    H = new_header(),
    'Header.add_custom_map'(H, Kvs),
    H.

'Header.add'(H, Key, Value) ->
    K = 'CommonHeader.str'(Key),
    todo,
    ok.

'Header.add_custom'(H, Key, Value) ->
    is_valid(Key),
    todo,
    ok.

'Header.add_map'(H, Kvs) ->
    lists:foreach(fun(V) ->
        'Header.add'(H, K, V),
        ok.
        ok
    end, Kvs),
        ok.

'Header.add_custom_map'(H, Kvs) ->
    lists:foreach(fun(V) ->
        'Header.add_custom'(H, K, V),
        ok
    end, Kvs),
    ok.

'Header.set'(H, Key, Value) ->
    Key_str = 'CommonHeader.str'(Key),
    % TODO: unhandled stmt type
    todo,
    ok.

'Header.set_custom'(H, Key, Value) ->
    is_valid(Key),
    Set = false,
    lists:foreach(fun(Kv) ->
        case maps:get(key, Kv) == Key of
            true -> case not Set of
                true -> begin
                    Set1 = true,
                end;
                false -> ok
            end;
            false -> ok
        end,
        ok
    end, maps:get(data, H)),
    case Set1 of
        true -> ok;
        false -> begin
            todo,
            ok
        end
        end.

'Header.delete'(H, Key) ->
    'Header.delete_custom'(H, 'CommonHeader.str'(Key)),
    ok.

'Header.delete_custom'(H, Key) ->
    % TODO: unhandled stmt type
        ok.

'Header.contains'(H, Key) ->
    case maps:get(cur_pos, H) == 0 of
        true -> false;
        false -> begin
            Key_str = 'CommonHeader.str'(Key),
            % TODO: unhandled stmt type
            false
        end
        end.

'Header.contains_custom'(H, Key, Flags) ->
    case maps:get(exact, Flags) of
        true -> begin
            % TODO: unhandled stmt type
            false
        end;
        false -> begin
            Lower_key = string:lowercase(Key),
            % TODO: unhandled stmt type
            false
        end
    end.

'Header.get'(H, Key) ->
    'Header.get_custom'(H, 'CommonHeader.str'(Key), #{{vbeam, type} => 'HeaderQueryConfig'}).

'Header.get_custom'(H, Key, Flags) ->
    case maps:get(exact, Flags) of
        true -> ok;
        false -> begin
            Lower_key = string:lowercase(Key),
            % TODO: unhandled stmt type
        end
    end,
    error(<<"none">>).

'Header.starting_with'(H, Key) ->
    lists:foreach(fun(Kv) ->
        case case string:prefix(maps:get(key, Kv), Key) of nomatch -> false; _ -> true end of
            true -> maps:get(key, Kv);
            false -> ok
        end,
        ok
    end, maps:get(data, H)),
    error(<<"none">>).

'Header.values'(H, Key) ->
    'Header.custom_values'(H, 'CommonHeader.str'(Key), #{{vbeam, type} => 'HeaderQueryConfig'}).

'Header.custom_values'(H, Key, Flags) ->
    case maps:get(cur_pos, H) == 0 of
        true -> [];
        false -> begin
            Res = [],
            case maps:get(exact, Flags) of
                true -> begin
                    % TODO: unhandled stmt type
                    Res
                end;
                false -> begin
                    Lower_key = string:lowercase(Key),
                    % TODO: unhandled stmt type
                    Res
                end
            end
        end
        end.

'Header.keys'(H) ->
    Res = [],
    % TODO: unhandled stmt type
    uniq(Res).

'Header.render'(H, Flags) ->
    Sb = new_builder(length(maps:get(data, H)) * 48),
    'Header.render_into_sb'(H, Sb, Flags),
    Res = 'Builder.str'(Sb),
    todo,
    Res.

'Header.render_into_sb'(H, Sb, Flags) ->
    % TODO: unhandled stmt type
        ok.

'Header.join'(H, Other) ->
    Combined = #{data => maps:get(data, H), cur_pos => maps:get(cur_pos, H), {vbeam, type} => 'Header'},
    lists:foreach(fun(K) ->
        lists:foreach(fun(V) ->
            'Header.add_custom'(Combined, K, V),
            ok
        end, 'Header.custom_values'(Other, K, #{exact => true, {vbeam, type} => 'HeaderQueryConfig'})),
        ok
    end, 'Header.keys'(Other)),
    Combined.

canonicalize(Name) ->
    case lists:member(Name, #{<<"accept">> => accept, <<"accept-ch">> => accept_ch, <<"accept-charset">> => accept_charset, <<"accept-ch-lifetime">> => accept_ch_lifetime, <<"accept-encoding">> => accept_encoding, <<"accept-language">> => accept_language, <<"accept-patch">> => accept_patch, <<"accept-post">> => accept_post, <<"accept-ranges">> => accept_ranges, <<"access-control-allow-credentials">> => access_control_allow_credentials, <<"access-control-allow-headers">> => access_control_allow_headers, <<"access-control-allow-methods">> => access_control_allow_methods, <<"access-control-allow-origin">> => access_control_allow_origin, <<"access-control-expose-headers">> => access_control_expose_headers, <<"access-control-max-age">> => access_control_max_age, <<"access-control-request-headers">> => access_control_request_headers, <<"access-control-request-method">> => access_control_request_method, <<"age">> => age, <<"allow">> => allow, <<"alt-svc">> => alt_svc, <<"authorization">> => authorization, <<"cache-control">> => cache_control, <<"clear-site-data">> => clear_site_data, <<"connection">> => connection, <<"content-disposition">> => content_disposition, <<"content-encoding">> => content_encoding, <<"content-language">> => content_language, <<"content-length">> => content_length, <<"content-location">> => content_location, <<"content-range">> => content_range, <<"content-security-policy">> => content_security_policy, <<"content-security-policy-report-only">> => content_security_policy_report_only, <<"content-type">> => content_type, <<"cookie">> => cookie, <<"cross-origin-embedder-policy">> => cross_origin_embedder_policy, <<"cross-origin-opener-policy">> => cross_origin_opener_policy, <<"cross-origin-resource-policy">> => cross_origin_resource_policy, <<"date">> => date, <<"device-memory">> => device_memory, <<"digest">> => digest, <<"dnt">> => dnt, <<"early-data">> => early_data, <<"etag">> => etag, <<"expect">> => expect, <<"expect-ct">> => expect_ct, <<"expires">> => expires, <<"feature-policy">> => feature_policy, <<"forwarded">> => forwarded, <<"from">> => from, <<"host">> => host, <<"if-match">> => if_match, <<"if-modified-since">> => if_modified_since, <<"if-none-match">> => if_none_match, <<"if-range">> => if_range, <<"if-unmodified-since">> => if_unmodified_since, <<"index">> => index, <<"keep-alive">> => keep_alive, <<"large-allocation">> => large_allocation, <<"last-modified">> => last_modified, <<"link">> => link, <<"location">> => location, <<"nel">> => nel, <<"origin">> => origin, <<"pragma">> => pragma, <<"proxy-authenticate">> => proxy_authenticate, <<"proxy-authorization">> => proxy_authorization, <<"range">> => range, <<"referer">> => referer, <<"referrer-policy">> => referrer_policy, <<"retry-after">> => retry_after, <<"save-data">> => save_data, <<"sec-fetch-dest">> => sec_fetch_dest, <<"sec-fetch-mode">> => sec_fetch_mode, <<"sec-fetch-site">> => sec_fetch_site, <<"sec-fetch-user">> => sec_fetch_user, <<"sec-websocket-accept">> => sec_websocket_accept, <<"sec_websocket_key">> => sec_websocket_key, <<"server">> => server, <<"server-timing">> => server_timing, <<"set-cookie">> => set_cookie, <<"sourcemap">> => sourcemap, <<"strict-transport-security">> => strict_transport_security, <<"te">> => te, <<"timing-allow-origin">> => timing_allow_origin, <<"tk">> => tk, <<"trailer">> => trailer, <<"transfer-encoding">> => transfer_encoding, <<"upgrade">> => upgrade, <<"upgrade-insecure-requests">> => upgrade_insecure_requests, <<"user-agent">> => user_agent, <<"vary">> => vary, <<"via">> => via, <<"want-digest">> => want_digest, <<"warning">> => warning, <<"www-authenticate">> => www_authenticate, <<"x-content-type-options">> => x_content_type_options, <<"x-dns-prefetch-control">> => x_dns_prefetch_control, <<"x-forwarded-for">> => x_forwarded_for, <<"x-forwarded-host">> => x_forwarded_host, <<"x-forwarded-proto">> => x_forwarded_proto, <<"x-frame-options">> => x_frame_options, <<"x-xss-protection">> => x_xss_protection}) of
        true -> 'CommonHeader.str'(maps:get(Name, #{<<"accept">> => accept, <<"accept-ch">> => accept_ch, <<"accept-charset">> => accept_charset, <<"accept-ch-lifetime">> => accept_ch_lifetime, <<"accept-encoding">> => accept_encoding, <<"accept-language">> => accept_language, <<"accept-patch">> => accept_patch, <<"accept-post">> => accept_post, <<"accept-ranges">> => accept_ranges, <<"access-control-allow-credentials">> => access_control_allow_credentials, <<"access-control-allow-headers">> => access_control_allow_headers, <<"access-control-allow-methods">> => access_control_allow_methods, <<"access-control-allow-origin">> => access_control_allow_origin, <<"access-control-expose-headers">> => access_control_expose_headers, <<"access-control-max-age">> => access_control_max_age, <<"access-control-request-headers">> => access_control_request_headers, <<"access-control-request-method">> => access_control_request_method, <<"age">> => age, <<"allow">> => allow, <<"alt-svc">> => alt_svc, <<"authorization">> => authorization, <<"cache-control">> => cache_control, <<"clear-site-data">> => clear_site_data, <<"connection">> => connection, <<"content-disposition">> => content_disposition, <<"content-encoding">> => content_encoding, <<"content-language">> => content_language, <<"content-length">> => content_length, <<"content-location">> => content_location, <<"content-range">> => content_range, <<"content-security-policy">> => content_security_policy, <<"content-security-policy-report-only">> => content_security_policy_report_only, <<"content-type">> => content_type, <<"cookie">> => cookie, <<"cross-origin-embedder-policy">> => cross_origin_embedder_policy, <<"cross-origin-opener-policy">> => cross_origin_opener_policy, <<"cross-origin-resource-policy">> => cross_origin_resource_policy, <<"date">> => date, <<"device-memory">> => device_memory, <<"digest">> => digest, <<"dnt">> => dnt, <<"early-data">> => early_data, <<"etag">> => etag, <<"expect">> => expect, <<"expect-ct">> => expect_ct, <<"expires">> => expires, <<"feature-policy">> => feature_policy, <<"forwarded">> => forwarded, <<"from">> => from, <<"host">> => host, <<"if-match">> => if_match, <<"if-modified-since">> => if_modified_since, <<"if-none-match">> => if_none_match, <<"if-range">> => if_range, <<"if-unmodified-since">> => if_unmodified_since, <<"index">> => index, <<"keep-alive">> => keep_alive, <<"large-allocation">> => large_allocation, <<"last-modified">> => last_modified, <<"link">> => link, <<"location">> => location, <<"nel">> => nel, <<"origin">> => origin, <<"pragma">> => pragma, <<"proxy-authenticate">> => proxy_authenticate, <<"proxy-authorization">> => proxy_authorization, <<"range">> => range, <<"referer">> => referer, <<"referrer-policy">> => referrer_policy, <<"retry-after">> => retry_after, <<"save-data">> => save_data, <<"sec-fetch-dest">> => sec_fetch_dest, <<"sec-fetch-mode">> => sec_fetch_mode, <<"sec-fetch-site">> => sec_fetch_site, <<"sec-fetch-user">> => sec_fetch_user, <<"sec-websocket-accept">> => sec_websocket_accept, <<"sec_websocket_key">> => sec_websocket_key, <<"server">> => server, <<"server-timing">> => server_timing, <<"set-cookie">> => set_cookie, <<"sourcemap">> => sourcemap, <<"strict-transport-security">> => strict_transport_security, <<"te">> => te, <<"timing-allow-origin">> => timing_allow_origin, <<"tk">> => tk, <<"trailer">> => trailer, <<"transfer-encoding">> => transfer_encoding, <<"upgrade">> => upgrade, <<"upgrade-insecure-requests">> => upgrade_insecure_requests, <<"user-agent">> => user_agent, <<"vary">> => vary, <<"via">> => via, <<"want-digest">> => want_digest, <<"warning">> => warning, <<"www-authenticate">> => www_authenticate, <<"x-content-type-options">> => x_content_type_options, <<"x-dns-prefetch-control">> => x_dns_prefetch_control, <<"x-forwarded-for">> => x_forwarded_for, <<"x-forwarded-host">> => x_forwarded_host, <<"x-forwarded-proto">> => x_forwarded_proto, <<"x-frame-options">> => x_frame_options, <<"x-xss-protection">> => x_xss_protection}));
        false -> iolist_to_binary(lists:join(<<"-">>, lists:map('string.capitalize'(It), binary:split(Name, <<"-">>, [global]))))
        end.

'HeaderKeyError.msg'(Err) ->
    <<"Invalid header key: '", (maps:get(header, Err))/binary, "'">>.

'HeaderKeyError.code'(Err) ->
    maps:get(code, Err).

is_valid(Header) ->
    lists:foreach(fun(C) ->
        case todo >= 128 orelse not is_token(C) of
            true -> todo;
            false -> ok
        end,
        ok
    end, Header),
    case length(Header) == 0 of
        true -> todo;
        false -> ok
        end.

is_token(B) ->
    case B of
        33; todo; 42; 43; 45; 46; todo; todo; todo; 124; 126 -> true;
        _ -> false
    end.

'Header.str'(H) ->
    'Header.render'(H, #{version => v1_1, {vbeam, type} => 'HeaderRenderConfig'}).

parse_headers(S) ->
    H = new_header(),
    Last_key = <<"">>,
    Last_value = <<"">>,
    lists:foreach(fun(Line) ->
        case length(Line) == 0 of
            true -> ok;
            false -> ok
        end,
        case lists:nth(1, Line) == todo orelse lists:nth(1, Line) == todo of
            true -> begin
                Last_value1 = <<" ", (string:trim(Line))/binary>>,
                % TODO: unhandled stmt type
            end;
            false -> case Last_key /= <<"">> of
                true -> 'Header.add_custom'(H, Last_key, Last_value1);
                false -> ok
            end
        end,
        Last_key1 = element(1, parse_header(Line)),
        Last_value2 = element(2, parse_header(Line)),
        ok
    end, binary:split(S, <<"\n">>, [global])),
    'Header.add_custom'(H, Last_key1, Last_value2),
    H.

parse_header(S) ->
    case not case binary:match(S, <<":">>) of nomatch -> false; _ -> true end of
        true -> error(<<"missing colon in header">>);
        false -> begin
            Words = 'string.split_nth'(S, <<":">>, 2),
            lists:nth(1, Words)
        end
        end.

parse_header_fast(S) ->
    Pos = 'string.index'(S, <<":">>),
    Pos.

new_request(Method, Url_, Data) ->
    Url = case Method == get andalso not case binary:match(Url_, <<"?">>) of nomatch -> false; _ -> true end of
        true -> <<(<<(Url_)/binary, (<<"?">>)/binary>>)/binary, (Data)/binary>>;
        false -> Url_
    end,
    #{method => Method, url => Url, data => Data, {vbeam, type} => 'Request'}.

get(Url) ->
    fetch(#{method => get, url => Url, {vbeam, type} => 'FetchConfig'}).

post(Url, Data) ->
    fetch(#{method => post, url => Url, data => Data, header => new_header(#{key => content_type, value => <<"text/plain">>, {vbeam, type} => 'HeaderConfig'}), {vbeam, type} => 'FetchConfig'}).

post_json(Url, Data) ->
    fetch(#{method => post, url => Url, data => Data, header => new_header(#{key => content_type, value => <<"application/json">>, {vbeam, type} => 'HeaderConfig'}), {vbeam, type} => 'FetchConfig'}).

post_form(Url, Data) ->
    fetch(#{method => post, url => Url, header => new_header(#{key => content_type, value => <<"application/x-www-form-urlencoded">>, {vbeam, type} => 'HeaderConfig'}), data => url_encode_form_data(Data), {vbeam, type} => 'FetchConfig'}).

post_form_with_cookies(Url, Data, Cookies) ->
    fetch(#{method => post, url => Url, header => new_header(#{key => content_type, value => <<"application/x-www-form-urlencoded">>, {vbeam, type} => 'HeaderConfig'}), data => url_encode_form_data(Data), cookies => Cookies, {vbeam, type} => 'FetchConfig'}).

post_multipart_form(Url, Conf) ->
    Body = element(1, multipart_form_body(maps:get(form, Conf), maps:get(files, Conf))),
    Boundary = element(2, multipart_form_body(maps:get(form, Conf), maps:get(files, Conf))),
    Header = maps:get(header, Conf),
    'Header.set'(Header, content_type, <<"multipart/form-data; boundary=\"", (Boundary)/binary, "\"">>),
    fetch(#{method => post, url => Url, header => Header, data => Body, {vbeam, type} => 'FetchConfig'}).

put(Url, Data) ->
    fetch(#{method => put, url => Url, data => Data, header => new_header(#{key => content_type, value => <<"text/plain">>, {vbeam, type} => 'HeaderConfig'}), {vbeam, type} => 'FetchConfig'}).

patch(Url, Data) ->
    fetch(#{method => patch, url => Url, data => Data, header => new_header(#{key => content_type, value => <<"text/plain">>, {vbeam, type} => 'HeaderConfig'}), {vbeam, type} => 'FetchConfig'}).

head(Url) ->
    fetch(#{method => head, url => Url, {vbeam, type} => 'FetchConfig'}).

delete(Url) ->
    fetch(#{method => delete, url => Url, {vbeam, type} => 'FetchConfig'}).

prepare(Config) ->
    case maps:get(url, Config) == <<"">> of
        true -> error(<<"http.fetch: empty url">>);
        false -> begin
            Url = build_url_from_fetch(Config),
            Req = #{method => maps:get(method, Config), url => Url, data => maps:get(data, Config), header => maps:get(header, Config), cookies => maps:get(cookies, Config), user_agent => maps:get(user_agent, Config), user_ptr => maps:get(user_ptr, Config), verbose => maps:get(verbose, Config), validate => maps:get(validate, Config), verify => maps:get(verify, Config), cert => maps:get(cert, Config), proxy => maps:get(proxy, Config), cert_key => maps:get(cert_key, Config), in_memory_verification => maps:get(in_memory_verification, Config), allow_redirect => maps:get(allow_redirect, Config), max_retries => maps:get(max_retries, Config), on_progress => maps:get(on_progress, Config), on_progress_body => maps:get(on_progress_body, Config), on_redirect => maps:get(on_redirect, Config), on_finish => maps:get(on_finish, Config), stop_copying_limit => maps:get(stop_copying_limit, Config), stop_receiving_limit => maps:get(stop_receiving_limit, Config), {vbeam, type} => 'Request'},
            Req
        end
        end.

fetch(Config) ->
    Req = prepare(Config),
    'Request.do'(Req).

get_text(Url) ->
    Resp = fetch(#{url => Url, method => get, {vbeam, type} => 'FetchConfig'}),
    maps:get(body, Resp).

url_encode_form_data(Data) ->
    Pieces = [],
    lists:foreach(fun(Value_) ->
        Key = query_escape(Key_),
        Value = query_escape(Value_),
        Pieces bsl <<(Key)/binary, "=", (Value)/binary>>,
        ok
    end, Data),
    iolist_to_binary(lists:join(<<"&">>, Pieces)).

build_url_from_fetch(Config) ->
    Url = parse(maps:get(url, Config)),
    case maps:size(maps:get(params, Config)) == 0 of
        true -> 'URL.str'(Url);
        false -> begin
            Pieces = [],
            lists:foreach(fun(Val) ->
                Pieces bsl <<(Key)/binary, "=", (Val)/binary>>,
                ok
            end, maps:get(params, Config)),
            Query = iolist_to_binary(lists:join(<<"&">>, Pieces)),
            case length(maps:get(raw_query, Url)) > 1 of
                true -> ok;
                false -> ok
            end,
            'URL.str'(Url)
        end
        end.

new_http_proxy(Raw_url) ->
    Url = parse(Raw_url),
    Scheme = maps:get(scheme, Url),
    case (not lists:member(Scheme, [<<"http">>, <<"https">>, <<"socks5">>])) of
        true -> error(<<"invalid scheme">>);
        false -> begin
            Username = <<"">>,
            Password = <<"">>,
            Str_url = 'URL.str'(Url),
            Host = maps:get(host, Url),
            Port = binary_to_integer('URL.port'(Url)),
            case Port == 0 of
                true -> case Scheme == <<"https">> of
                    true -> begin
                        Port1 = 443,
                        Host1 = <<(<<":">>)/binary, (integer_to_binary(Port1))/binary>>,
                    end;
                    false -> case Scheme == <<"http">> of
                        true -> begin
                            Port2 = 80,
                            Host2 = <<(<<":">>)/binary, (integer_to_binary(Port2))/binary>>,
                        end;
                        false -> ok
                    end
                end;
                false -> ok
            end,
            case Port2 == 0 of
                true -> error(<<"Unknown port">>);
                false -> begin
                    case todo of
                        true -> begin
                            Username1 = maps:get(username, U),
                            Password1 = maps:get(password, U),
                        end;
                        false -> ok
                    end,
                    #{scheme => Scheme, username => Username1, password => Password1, host => Host2, hostname => 'URL.hostname'(Url), port => Port2, url => Str_url, {vbeam, type} => 'HttpProxy'}
                end
                        end
        end
        end.

'HttpProxy.build_proxy_headers'(Pr, Host) ->
    Uheaders = [],
    Address = 'string.all_before_last'(Host, <<":">>),
    Uheaders bsl <<"Proxy-Connection: Keep-Alive\\r\\n">>,
    case maps:get(username, Pr) /= <<"">> of
        true -> begin
            Authinfo = <<"">>,
            Authinfo1 = maps:get(username, Pr),
            case maps:get(password, Pr) /= <<"">> of
                true -> ok;
                false -> ok
            end,
            Encoded_authinfo = encode(binary_to_list(Authinfo1)),
            Uheaders bsl <<"Proxy-Authorization: Basic ", (Encoded_authinfo)/binary, "\\r\\n">>
        end;
        false -> ok
    end,
    Version = v1_1,
    <<(<<(<<"CONNECT ", (Host)/binary, " ", (Version)/binary, "\\r\\nHost: ", (Address)/binary, "\\r\\n">>)/binary, (iolist_to_binary(lists:join(<<"">>, Uheaders)))/binary>>)/binary, (<<"\\r\\n">>)/binary>>.

'HttpProxy.http_do'(Pr, Host, _method, Path, Req) ->
    Host_name = element(1, split_address('URL.hostname'(Host))),
    Port = element(2, split_address('URL.hostname'(Host))),
    Port_part = case Port == 80 orelse Port == 0 of
        true -> <<"">>;
        false -> <<":", (integer_to_binary(Port))/binary>>
    end,
    S = 'Request.build_request_headers'(Req, maps:get(method, Req), Host_name, Port, <<(maps:get(scheme, Host))/binary, "://", (Host_name)/binary, (Port_part)/binary, (Path)/binary>>),
    case maps:get(scheme, Host) == <<"https">> of
        true -> begin
            Client = 'HttpProxy.ssl_dial'(Pr, <<(maps:get(host, Host))/binary, ":443">>),
            
        end;
        false -> case maps:get(scheme, Host) == <<"http">> of
            true -> begin
                Client1 = 'HttpProxy.dial'(Pr, <<(maps:get(host, Host))/binary, ":80">>),
                'TcpConn.set_read_timeout'(Client1, maps:get(read_timeout, Req)),
                'TcpConn.set_write_timeout'(Client1, maps:get(write_timeout, Req)),
                'TcpConn.write_string'(Client1, S),
                % TODO: unhandled stmt type
                Bytes = 'Request.read_all_from_client_connection'(Req, Client1),
                'TcpConn.close'(Client1),
                Response_text = '[]u8.bytestr'(Bytes),
                % TODO: unhandled stmt type
                case maps:get(on_finish, Req) /= todo of
                    true -> 'Request.on_finish'(Req, Req, todo);
                    false -> ok
                end,
                parse_response(Response_text)
            end;
            false -> ok
        end
    end,
    error(<<"Invalid Scheme">>).

'HttpProxy.dial'(Pr, Host) ->
    case lists:member(maps:get(scheme, Pr), [<<"http">>, <<"https">>]) of
        true -> begin
            Tcp = dial_tcp(maps:get(host, Pr)),
            'TcpConn.write'(Tcp, binary_to_list('HttpProxy.build_proxy_headers'(Pr, Host))),
            Bf = [],
            'TcpConn.read'(Tcp, Bf),
            Tcp
        end;
        false -> case maps:get(scheme, Pr) == <<"socks5">> of
            true -> socks5_dial(maps:get(host, Pr), Host, maps:get(username, Pr), maps:get(password, Pr));
            false -> error(<<"http_proxy dial: invalid proxy scheme">>)
        end
    end.

'HttpProxy.ssl_dial'(Pr, Host) ->
    case lists:member(maps:get(scheme, Pr), [<<"http">>, <<"https">>]) of
        true -> begin
            Tcp = dial_tcp(maps:get(host, Pr)),
            'TcpConn.write'(Tcp, binary_to_list('HttpProxy.build_proxy_headers'(Pr, Host))),
            Bf = [],
            'TcpConn.read'(Tcp, Bf),
            case not case binary:match('[]u8.bytestr'(Bf), <<"HTTP/1.1 200">>) of nomatch -> false; _ -> true end of
                true -> error(<<"ssl dial error: ", ('[]u8.bytestr'(Bf))/binary>>);
                false -> ok
            end,
            Ssl_conn = new_ssl_conn(#{verify => <<"">>, cert => <<"">>, cert_key => <<"">>, validate => false, in_memory_verification => false, {vbeam, type} => 'SSLConnectConfig'}),
            'SSLConn.connect'(Ssl_conn, Tcp, 'string.all_before_last'(Host, <<":">>)),
            Ssl_conn
        end;
        false -> case maps:get(scheme, Pr) == <<"socks5">> of
            true -> socks5_ssl_dial(maps:get(host, Pr), Host, maps:get(username, Pr), maps:get(password, Pr));
            false -> error(<<"http_proxy ssl_dial: invalid proxy scheme">>)
        end
    end.

'Method.str'(M) ->
    case M of
        get -> <<"GET">>;
        head -> <<"HEAD">>;
        post -> <<"POST">>;
        put -> <<"PUT">>;
        acl -> <<"ACL">>;
        baseline_control -> <<"BASELINE-CONTROL">>;
        bind -> <<"BIND">>;
        checkin -> <<"CHECKIN">>;
        checkout -> <<"CHECKOUT">>;
        connect -> <<"CONNECT">>;
        copy -> <<"COPY">>;
        delete -> <<"DELETE">>;
        label -> <<"LABEL">>;
        link -> <<"LINK">>;
        lock -> <<"LOCK">>;
        merge -> <<"MERGE">>;
        mkactivity -> <<"MKACTIVITY">>;
        mkcalendar -> <<"MKCALENDAR">>;
        mkcol -> <<"MKCOL">>;
        mkredirectref -> <<"MKREDIRECTREF">>;
        mkworkspace -> <<"MKWORKSPACE">>;
        move -> <<"MOVE">>;
        options -> <<"OPTIONS">>;
        orderpatch -> <<"ORDERPATCH">>;
        patch -> <<"PATCH">>;
        pri -> <<"PRI">>;
        propfind -> <<"PROPFIND">>;
        proppatch -> <<"PROPPATCH">>;
        rebind -> <<"REBIND">>;
        report -> <<"REPORT">>;
        search -> <<"SEARCH">>;
        trace -> <<"TRACE">>;
        unbind -> <<"UNBIND">>;
        uncheckout -> <<"UNCHECKOUT">>;
        unlink -> <<"UNLINK">>;
        unlock -> <<"UNLOCK">>;
        update -> <<"UPDATE">>;
        updateredirectref -> <<"UPDATEREDIRECTREF">>;
        version_control -> <<"VERSION-CONTROL">>
    end.

method_from_str(M) ->
    case M of
        <<"GET">> -> get;
        <<"HEAD">> -> head;
        <<"POST">> -> post;
        <<"PUT">> -> put;
        <<"ACL">> -> acl;
        <<"BASELINE-CONTROL">> -> baseline_control;
        <<"BIND">> -> bind;
        <<"CHECKIN">> -> checkin;
        <<"CHECKOUT">> -> checkout;
        <<"CONNECT">> -> connect;
        <<"COPY">> -> copy;
        <<"DELETE">> -> delete;
        <<"LABEL">> -> label;
        <<"LINK">> -> link;
        <<"LOCK">> -> lock;
        <<"MERGE">> -> merge;
        <<"MKACTIVITY">> -> mkactivity;
        <<"MKCALENDAR">> -> mkcalendar;
        <<"MKCOL">> -> mkcol;
        <<"MKREDIRECTREF">> -> mkredirectref;
        <<"MKWORKSPACE">> -> mkworkspace;
        <<"MOVE">> -> move;
        <<"OPTIONS">> -> options;
        <<"ORDERPATCH">> -> orderpatch;
        <<"PATCH">> -> patch;
        <<"PRI">> -> pri;
        <<"PROPFIND">> -> propfind;
        <<"PROPPATCH">> -> proppatch;
        <<"REBIND">> -> rebind;
        <<"REPORT">> -> report;
        <<"SEARCH">> -> search;
        <<"TRACE">> -> trace;
        <<"UNBIND">> -> unbind;
        <<"UNCHECKOUT">> -> uncheckout;
        <<"UNLINK">> -> unlink;
        <<"UNLOCK">> -> unlock;
        <<"UPDATE">> -> update;
        <<"UPDATEREDIRECTREF">> -> updateredirectref;
        <<"VERSION-CONTROL">> -> version_control;
        _ -> get
    end.

'Request.free'(Req) ->
    todo,
    ok.

'Request.add_header'(Req, Key, Val) ->
    'Header.add'(maps:get(header, Req), Key, Val),
    ok.

'Request.add_custom_header'(Req, Key, Val) ->
    'Header.add_custom'(maps:get(header, Req), Key, Val).

'Request.add_cookie'(Req, C) ->

'Request.cookie'(Req, Name) ->
    case todo of
        true -> #{name => Name, value => Value, {vbeam, type} => 'Cookie'};
        false -> todo
        end.

'Request.do'(Req) ->
    Url = parse(maps:get(url, Req)),
    Rurl = Url,
    Resp = #{{vbeam, type} => 'Response'},
    Nredirects = 0,
    % TODO: unhandled stmt type
    Resp.

'Request.method_and_url_to_response'(Req, Method, Url) ->
    Host_name = 'URL.hostname'(Url),
    Scheme = maps:get(scheme, Url),
    P = 'string.trim_left'('URL.escaped_path'(Url), <<"/">>),
    Path = case length('URL.query'(Url)) > 0 of
        true -> <<"/", (P)/binary, "?", ('Values.encode'('URL.query'(Url)))/binary>>;
        false -> <<"/", (P)/binary>>
    end,
    Nport = binary_to_integer('URL.port'(Url)),
    case Nport == 0 of
        true -> begin
            case Scheme == <<"http">> of
                true -> ok;
                false -> ok
            end,
            case Scheme == <<"https">> of
                true -> ok;
                false -> ok
            end
        end;
        false -> ok
    end,
    case Scheme == <<"https">> andalso maps:get(proxy, Req) == todo of
        true -> ok;
        false -> case Scheme == <<"http">> andalso maps:get(proxy, Req) == todo of
            true -> ok;
            false -> case maps:get(proxy, Req) /= todo of
                true -> ok;
                false -> ok
            end
        end
    end,
    error(<<"http.request.method_and_url_to_response: unsupported scheme: \"", (Scheme)/binary, "\"">>).

'Request.build_request_headers'(Req, Method, Host_name, Port, Path) ->
    Sb = new_builder(4096),
    Version = case maps:get(version, Req) == unknown of
        true -> v1_1;
        false -> maps:get(version, Req)
    end,
    'Builder.write_string'(Sb, 'Method.str'(Method)),
    'Builder.write_string'(Sb, <<" ">>),
    'Builder.write_string'(Sb, Path),
    'Builder.write_string'(Sb, <<" ">>),
    'Builder.write_string'(Sb, 'Version.str'(Version)),
    'Builder.write_string'(Sb, <<"\\r\\n">>),
    case not 'Header.contains'(maps:get(header, Req), host) of
        true -> begin
            'Builder.write_string'(Sb, <<"Host: ">>),
            case Port /= 80 andalso Port /= 443 andalso Port /= 0 of
                true -> 'Builder.write_string'(Sb, <<(Host_name)/binary, ":", (integer_to_binary(Port))/binary>>);
                false -> 'Builder.write_string'(Sb, Host_name)
            end,
            'Builder.write_string'(Sb, <<"\\r\\n">>)
        end;
        false -> ok
    end,
    case not 'Header.contains'(maps:get(header, Req), user_agent) of
        true -> begin
            Ua = maps:get(user_agent, Req),
            'Builder.write_string'(Sb, <<"User-Agent: ">>),
            'Builder.write_string'(Sb, Ua),
            'Builder.write_string'(Sb, <<"\\r\\n">>)
        end;
        false -> ok
    end,
    case not 'Header.contains'(maps:get(header, Req), content_length) of
        true -> begin
            'Builder.write_string'(Sb, <<"Content-Length: ">>),
            'Builder.write_string'(Sb, integer_to_binary(length(maps:get(data, Req)))),
            'Builder.write_string'(Sb, <<"\\r\\n">>)
        end;
        false -> ok
    end,
    Chkey = 'CommonHeader.str'(cookie),
    lists:foreach(fun(Key) ->
        case Key == Chkey of
            true -> ok;
            false -> ok
        end,
        Val = iolist_to_binary(lists:join(<<"; ">>, 'Header.custom_values'(maps:get(header, Req), Key, #{{vbeam, type} => 'HeaderQueryConfig'}))),
        'Builder.write_string'(Sb, Key),
        'Builder.write_string'(Sb, <<": ">>),
        'Builder.write_string'(Sb, Val),
        'Builder.write_string'(Sb, <<"\\r\\n">>),
        ok
    end, 'Header.keys'(maps:get(header, Req))),
    'Builder.write_string'(Sb, 'Request.build_request_cookies_header'(Req)),
    'Builder.write_string'(Sb, <<"Connection: close\\r\\n">>),
    'Builder.write_string'(Sb, <<"\\r\\n">>),
    'Builder.write_string'(Sb, maps:get(data, Req)),
    'Builder.str'(Sb).

'Request.build_request_cookies_header'(Req) ->
    case maps:size(maps:get(cookies, Req)) < 1 of
        true -> <<"">>;
        false -> begin
            Sb_cookie = new_builder(1024),
            Hvcookies = 'Header.values'(maps:get(header, Req), cookie),
            Total_cookies = maps:size(maps:get(cookies, Req)) + length(Hvcookies),
            'Builder.write_string'(Sb_cookie, <<"Cookie: ">>),
            Idx = 0,
            lists:foreach(fun(Val) ->
                'Builder.write_string'(Sb_cookie, Key),
                'Builder.write_string'(Sb_cookie, <<"=">>),
                'Builder.write_string'(Sb_cookie, Val),
                case Idx < Total_cookies - 1 of
                    true -> 'Builder.write_string'(Sb_cookie, <<"; ">>);
                    false -> ok
                end,
                todo,
                ok
            end, maps:get(cookies, Req)),
            lists:foreach(fun(C) ->
                'Builder.write_string'(Sb_cookie, C),
                case Idx < Total_cookies - 1 of
                    true -> 'Builder.write_string'(Sb_cookie, <<"; ">>);
                    false -> ok
                end,
                todo,
                ok
            end, Hvcookies),
            'Builder.write_string'(Sb_cookie, <<"\\r\\n">>),
            'Builder.str'(Sb_cookie)
        end
        end.

'Request.http_do'(Req, Host, Method, Path) ->
    Host_name = element(1, split_address(Host)),
    Port = element(2, split_address(Host)),
    S = 'Request.build_request_headers'(Req, Method, Host_name, Port, Path),
    Client = dial_tcp(Host),
    'TcpConn.set_read_timeout'(Client, maps:get(read_timeout, Req)),
    'TcpConn.set_write_timeout'(Client, maps:get(write_timeout, Req)),
    'TcpConn.write'(Client, binary_to_list(S)),
    % TODO: unhandled stmt type
    Bytes = 'Request.read_all_from_client_connection'(Req, Client),
    'TcpConn.close'(Client),
    Response_text = '[]u8.bytestr'(Bytes),
    % TODO: unhandled stmt type
    case maps:get(on_finish, Req) /= todo of
        true -> 'Request.on_finish'(Req, Req, todo);
        false -> ok
    end,
    parse_response(Response_text).

'Request.receive_all_data_from_cb_in_builder'(Req, Content, Con, Receive_chunk_cb) ->
    Buff = [64 * 1024],
    Bp = todo,
    Readcounter = 0,
    Body_pos = todo,
    Old_len = todo,
    New_len = todo,
    Expected_size = todo,
    Status_code = -1,
    % TODO: unhandled stmt type
    ok.

read_from_tcp_connection_cb(Con, Buf, Bufsize) ->
    R = todo,
    'TcpConn.read_ptr'(R, Buf, Bufsize).

'Request.read_all_from_client_connection'(Req, R) ->
    Content = new_builder(4096),
    'Request.receive_all_data_from_cb_in_builder'(Req, Content, todo, Net.http.read_from_tcp_connection_cb),
    Content.

'Request.referer'(Req) ->
    'Header.get'(maps:get(header, Req), referer).

parse_request(Reader) ->
    Request = parse_request_head(Reader),
    Body = [],
    case todo of
        true -> begin
            N = binary_to_integer(Length),
            case N > 0 of
                true -> begin
                    Body1 = [],
                    Count = 0,
                    % TODO: unhandled stmt type
                end;
                false -> ok
            end
        end;
        false -> ok
    end,
    Request.

parse_request_head(Reader) ->
    Line = 'BufferedReader.read_line'(Reader, #{{vbeam, type} => 'BufferedReadLineConfig'}),
    Method = element(1, parse_request_line(Line)),
    Target = element(2, parse_request_line(Line)),
    Version = element(3, parse_request_line(Line)),
    Header = new_header(),
    Line1 = 'BufferedReader.read_line'(Reader, #{{vbeam, type} => 'BufferedReadLineConfig'}),
    % TODO: unhandled stmt type
    Request_cookies = #{},
    lists:foreach(fun(Cookie) ->
        ok
    end, read_cookies(Header, <<"">>)),
    #{method => Method, url => 'URL.str'(Target), header => Header, host => 'Header.get'(Header, host), version => Version, cookies => Request_cookies, {vbeam, type} => 'Request'}.

parse_request_head_str(S) ->
    Pos0 = 'string.index_'(S, <<"\\n">>),
    case Pos0 == -1 of
        true -> error(<<"malformed request: no request line found">>);
        false -> begin
            Line0 = string:trim(lists:nth(todo + 1, S)),
            Method = element(1, parse_request_line(Line0)),
            Target = element(2, parse_request_line(Line0)),
            Version = element(3, parse_request_line(Line0)),
            Header = new_header(),
            Lines = binary:split(lists:nth(todo + 1, S), <<"\\n">>, [global]),
            lists:foreach(fun(Line_raw) ->
                Line = 'string.trim_right'(Line_raw, <<"\\r">>),
                case Line == <<"">> of
                    true -> ok;
                    false -> ok
                end,
                case not case binary:match(Line, <<":">>) of nomatch -> false; _ -> true end of
                    true -> ok;
                    false -> ok
                end,
                Pos = parse_header_fast(Line),
                Key = 'string.substr_unsafe'(Line, 0, Pos),
                Val_start = Pos + 1,
                % TODO: unhandled stmt type
                case Val_start < length(Line) of
                    true -> begin
                        Value = 'string.substr_unsafe'(Line, Val_start, length(Line)),
                        'Header.add_custom'(Header, Key, Value)
                    end;
                    false -> ok
                end,
                ok
            end, Lines),
            Request_cookies = #{},
            lists:foreach(fun(Cookie) ->
                ok
            end, read_cookies(Header, <<"">>)),
            #{method => Method, url => 'URL.str'(Target), header => Header, host => 'Header.get'(Header, host), version => Version, cookies => Request_cookies, {vbeam, type} => 'Request'}
        end
        end.

parse_request_str(S) ->
    Request = parse_request_head_str(S),
    Body_pos = 'string.index_'(S, <<"\\r\\n\\r\\n">>),
    case Body_pos /= -1 of
        true -> ok;
        false -> ok
    end,
    Request.

parse_request_line(Line) ->
    Words = binary:split(Line, <<" ">>, [global]),
    case length(Words) /= 3 of
        true -> error(<<"bad request header">>);
        false -> begin
            Method_str = lists:nth(1, Words),
            Target_str = lists:nth(2, Words),
            Version_str = lists:nth(3, Words),
            Method = method_from_str(Method_str),
            Target = parse(Target_str),
            Version = version_from_str(Version_str),
            case Version == unknown of
                true -> error(<<"unsupported version">>);
                false -> Method
                        end
        end
        end.

parse_form(Body) ->
    Form = #{},
    case 'string.match_glob'(Body, <<"{*}">>) of
        true -> ok;
        false -> begin
            Words = binary:split(Body, <<"&">>, [global]),
            lists:foreach(fun(Word) ->
                Kv = 'string.split_nth'(Word, <<"=">>, 2),
                case length(Kv) /= 2 of
                    true -> ok;
                    false -> ok
                end,
                Key = query_unescape(lists:nth(1, Kv)),
                Val = query_unescape(lists:nth(2, Kv)),
                ok
            end, Words),
        end
    end,
    Form.

'UnexpectedExtraAttributeError.msg'(Err) ->
    <<"Encountered unexpected extra attributes: ", (maps:get(attributes, Err))/binary>>.

'MultiplePathAttributesError.msg'(Err) ->
    <<"Expected at most one path attribute">>.

multipart_form_body(Form, Files) ->
    Rboundary = ulid(),
    Sb = new_builder(1024),
    lists:foreach(fun(Value) ->
        'Builder.write_string'(Sb, <<"\\r\\n--">>),
        'Builder.write_string'(Sb, Rboundary),
        'Builder.write_string'(Sb, <<"\\r\\nContent-Disposition: form-data; name=\"">>),
        'Builder.write_string'(Sb, Name),
        'Builder.write_string'(Sb, <<"\"\\r\\n\\r\\n">>),
        'Builder.write_string'(Sb, Value),
        ok
    end, Form),
    lists:foreach(fun(Fs) ->
        lists:foreach(fun(F) ->
            'Builder.write_string'(Sb, <<"\\r\\n--">>),
            'Builder.write_string'(Sb, Rboundary),
            'Builder.write_string'(Sb, <<"\\r\\nContent-Disposition: form-data; name=\"">>),
            'Builder.write_string'(Sb, Name),
            'Builder.write_string'(Sb, <<"\"; filename=\"">>),
            'Builder.write_string'(Sb, maps:get(filename, F)),
            'Builder.write_string'(Sb, <<"\"\\r\\nContent-Type: ">>),
            'Builder.write_string'(Sb, maps:get(content_type, F)),
            'Builder.write_string'(Sb, <<"\\r\\n\\r\\n">>),
            'Builder.write_string'(Sb, maps:get(data, F)),
            ok
        end, Fs),
        ok
    end, Files),
    'Builder.write_string'(Sb, <<"\\r\\n--">>),
    'Builder.write_string'(Sb, Rboundary),
    'Builder.write_string'(Sb, <<"--">>),
    'Builder.str'(Sb).

parse_multipart_form(Body, Boundary) ->
    Form = #{},
    Files = #{},
    Sections = binary:split(Body, Boundary, [global]),
    Fields = lists:nth(todo + 1, Sections),
    Line_segments = [],
    lists:foreach(fun(Field) ->
        'LineSegmentIndexes.clear'(Line_segments),
        Line_idx = 0,
        Line_start = 0,
        lists:foreach(fun(C) ->
            case Line_idx >= 6 of
                true -> ok;
                false -> ok
            end,
            case C == todo of
                true -> begin
                    Line_segments bsl #{start => Line_start, end => Cidx, {vbeam, type} => 'LineSegmentIndexes'},
                    Line_start1 = Cidx + 1,
                    todo
                end;
                false -> ok
            end,
            ok
        end, Field),
        Line_segments bsl #{start => Line_start1, end => length(Field), {vbeam, type} => 'LineSegmentIndexes'},
        case length(Line_segments) < 2 of
            true -> ok;
            false -> ok
        end,
        Line1 = lists:nth(todo + 1, Field),
        Line2 = case length(Line_segments) == 2 of
            true -> <<"">>;
            false -> lists:nth(todo + 1, Field)
        end,
        Disposition = parse_disposition(string:trim(Line1)),
        Name = maps:get(<<"name">>, Disposition),
        case todo of
            true -> begin
                case length(Line_segments) < 5 of
                    true -> ok;
                    false -> ok
                end,
                case not case string:prefix(string:lowercase(Line2), <<"content-type:">>) of nomatch -> false; _ -> true end of
                    true -> ok;
                    false -> ok
                end,
                Content_type = string:trim(lists:nth(2, 'string.split_nth'(Line2, <<":">>, 2))),
                Data = lists:nth(todo + 1, Field),
                maps:get(Name, Files) bsl #{filename => Filename, content_type => Content_type, data => Data, {vbeam, type} => 'FileData'},
                % TODO: unhandled stmt type
            end;
            false -> ok
        end,
        case length(Line_segments) < 4 of
            true -> ok;
            false -> ok
        end,
        ok
    end, Fields),
    Form.

parse_disposition(Line) ->
    Data = #{},
    lists:foreach(fun(Word) ->
        Kv = 'string.split_nth'(Word, <<"=">>, 2),
        case length(Kv) /= 2 of
            true -> ok;
            false -> ok
        end,
        Key = 'string.trim_left'(string:lowercase(lists:nth(1, Kv)), <<" \\t">>),
        Value = lists:nth(2, Kv),
        case case string:prefix(Value, <<"\"">>) of nomatch -> false; _ -> true end andalso case binary:longest_common_suffix([Value, <<"\"">>]) of 0 -> false; _ -> true end of
            true -> ok;
            false -> ok
        end,
        ok
    end, binary:split(Line, <<";">>, [global])),
    Data.

is_no_need_retry_error(Err_code) ->
    lists:member(Err_code, ['IError.code'('v.net':'error_with_code'(<<"net: port out of range">>, 0 + 5)), 'IError.code'('v.net':'error_with_code'(<<"net: no udp remote">>, 0 + 6)), 'IError.code'('v.net':'error_with_code'(<<"net: connect timed out">>, 0 + 8)), 0 + 9]).

'Response.free'(Resp) ->
    todo,
    ok.

'Response.bytes'(Resp) ->
    binary_to_list('Response.bytestr'(Resp)).

'Response.bytestr'(Resp) ->
    <<(<<(<<"HTTP/", (maps:get(http_version, Resp))/binary, " ", (integer_to_binary(maps:get(status_code, Resp)))/binary, " ", (maps:get(status_msg, Resp))/binary, "\\r\\n">>)/binary, (<<('Header.render'(maps:get(header, Resp), #{version => 'Response.version'(Resp), {vbeam, type} => 'HeaderRenderConfig'}))/binary, "\\r\\n">>)/binary>>)/binary, (maps:get(body, Resp))/binary>>.

parse_response(Resp) ->
    Version = element(1, parse_status_line('string.all_before'(Resp, <<"\\r\\n">>))),
    Status_code = element(2, parse_status_line('string.all_before'(Resp, <<"\\r\\n">>))),
    Status_msg = element(3, parse_status_line('string.all_before'(Resp, <<"\\r\\n">>))),
    Start_idx = element(1, find_headers_range(Resp)),
    End_idx = element(2, find_headers_range(Resp)),
    Header = parse_headers('string.substr'(Resp, Start_idx, End_idx)),
    Body = 'string.substr'(Resp, End_idx, length(Resp)),
    case 'Header.get'(Header, transfer_encoding) == <<"chunked">> of
        true -> ok;
        false -> ok
    end,
    #{http_version => Version, status_code => Status_code, status_msg => Status_msg, header => Header, body => Body, {vbeam, type} => 'Response'}.

parse_status_line(Line) ->
    case length(Line) < 5 orelse string:lowercase(lists:nth(todo + 1, Line)) /= <<"http/">> of
        true -> error(<<"response does not start with HTTP/, line: `", (Line)/binary, "`">>);
        false -> begin
            Data = 'string.split_nth'(Line, <<" ">>, 3),
            case length(Data) /= 3 of
                true -> error(<<"expected at least 3 tokens, but found: ", (integer_to_binary(length(Data)))/binary>>);
                false -> begin
                    Version = 'string.substr'(lists:nth(1, Data), 5, length(lists:nth(1, Data))),
                    Digits = 'string.split_nth'(Version, <<".">>, 3),
                    case length(Digits) /= 2 of
                        true -> error(<<"HTTP version malformed, found: `", (Digits)/binary, "`">>);
                        false -> begin
                            lists:foreach(fun(Digit) ->
                                atoi(Digit),
                                ok
                            end, Digits),
                            Version
                        end
                                        end
                end
                        end
        end
        end.

'Response.cookies'(R) ->
    Cookies = [],
    lists:foreach(fun(Cookie) ->
        Cookies bsl parse_cookie(Cookie),
        ok
    end, 'Header.values'(maps:get(header, R), set_cookie)),
    Cookies.

'Response.status'(R) ->
    status_from_int(maps:get(status_code, R)).

'Response.set_status'(R, S) ->

'Response.version'(R) ->
    case maps:get(http_version, R) of
        <<"1.0">> -> v1_0;
        <<"1.1">> -> v1_1;
        <<"2.0">> -> v2_0;
        _ -> unknown
    end.

'Response.set_version'(R, V) ->
    case V == unknown of
        true -> ok;
        false -> begin
            Maj = element(1, 'Version.protos'(V)),
            Min = element(2, 'Version.protos'(V)),
        end
        end.

new_response(Conf) ->
    Resp = #{body => maps:get(body, Conf), header => maps:get(header, Conf), {vbeam, type} => 'Response'},
    case maps:get(body, Resp) /= <<"">> andalso not 'Header.contains'(maps:get(header, Resp), content_length) of
        true -> 'Header.add'(maps:get(header, Resp), content_length, integer_to_binary(length(maps:get(body, Resp))));
        false -> ok
    end,
    'Response.set_status'(Resp, maps:get(status, Conf)),
    'Response.set_version'(Resp, maps:get(version, Conf)),
    Resp.

find_headers_range(Data) ->
    Start_idx = 'string.index'(Data, <<"\\n">>) + 1,
    Count = 0,
    % TODO: unhandled stmt type
    error(<<"no end index found">>).

'Server.listen_and_serve'(S) ->
    case maps:get(handler, S) is todo of
        true -> io:format(standard_error, "~s~n", [<<"Server handler not set, using debug handler">>]);
        false -> ok
    end,
    L = 'TcpListener.addr'(maps:get(listener, S)),
    case 'Addr.family'(L) == unspec of
        true -> begin
            Listening_address = case maps:get(addr, S) == <<"">> orelse maps:get(addr, S) == <<":0">> of
                true -> <<"localhost:0">>;
                false -> maps:get(addr, S)
            end,
            Listen_family = ip,
            L1 = 'TcpListener.addr'(maps:get(listener, S)),
        end;
        false -> ok
    end,
    'TcpListener.set_accept_timeout'(maps:get(listener, S), maps:get(accept_timeout, S)),
    Ch = todo,
    Ws = [],
    lists:foreach(fun(Wid) ->
        Ws bsl new_handler_worker(Wid, Ch, maps:get(handler, S), maps:get(max_keep_alive_requests, S)),
        ok
    end, lists:seq(0, maps:get(worker_num, S) - 1)),
    case maps:get(show_startup_message, S) of
        true -> begin
            vbeam_io:println(<<"Listening on http://", (maps:get(addr, S))/binary, "/">>),
            flush_stdout()
        end;
        false -> ok
    end,
    timer:sleep(20 * todo),
    case maps:get(on_running, S) /= todo of
        true -> 'Server.on_running'(S, S);
        false -> ok
    end,
    % TODO: unhandled stmt type
    case maps:get(state, S) == stopped of
        true -> 'Server.close'(S);
        false -> ok
    end.

'Server.stop'(S) ->
    case maps:get(on_stopped, S) /= todo of
        true -> 'Server.on_stopped'(S, S);
        false -> ok
    end.

'Server.close'(S) ->
    'TcpListener.close'(maps:get(listener, S)),
    case maps:get(on_closed, S) /= todo of
        true -> 'Server.on_closed'(S, S);
        false -> ok
    end.

'Server.status'(S) ->
    maps:get(state, S).

'Server.wait_till_running'(S, Params) ->
    I = 0,
    % TODO: unhandled stmt type
    case I >= maps:get(max_retries, Params) of
        true -> error(<<"maximum retries reached">>);
        false -> begin
            timer:sleep(maps:get(retry_period_ms, Params)),
            I
        end
        end.

new_handler_worker(Wid, Ch, Handler, Max_keep_alive_requests) ->
    W = #{id => Wid, ch => Ch, handler => Handler, max_keep_alive_requests => Max_keep_alive_requests, {vbeam, type} => 'HandlerWorker'},
    todo.

'HandlerWorker.process_requests'(W) ->
    % TODO: unhandled stmt type
        ok.

'HandlerWorker.handle_conn'(W, Conn) ->
    % TODO: unhandled stmt type
    Reader = new_buffered_reader(#{reader => Conn, {vbeam, type} => 'BufferedReaderConfig'}),
    % TODO: unhandled stmt type
    Request_count = 0,
    % TODO: unhandled stmt type
        ok.

'DebugHandler.handle'(D, Req) ->
    R = #{body => maps:get(data, Req), header => maps:get(header, Req), {vbeam, type} => 'Response'},
    'Response.set_status'(R, ok),
    'Response.set_version'(R, maps:get(version, Req)),
    R.

status_from_int(Code) ->
    case Code of
        100 -> cont;
        101 -> switching_protocols;
        102 -> processing;
        103 -> checkpoint_draft;
        todo -> unassigned;
        200 -> ok;
        201 -> created;
        202 -> accepted;
        203 -> non_authoritative_information;
        204 -> no_content;
        205 -> reset_content;
        206 -> partial_content;
        207 -> multi_status;
        208 -> already_reported;
        todo -> unassigned;
        226 -> im_used;
        todo -> unassigned;
        300 -> multiple_choices;
        301 -> moved_permanently;
        302 -> found;
        303 -> see_other;
        304 -> not_modified;
        305 -> use_proxy;
        306 -> switch_proxy;
        307 -> temporary_redirect;
        308 -> permanent_redirect;
        todo -> unassigned;
        400 -> bad_request;
        401 -> unauthorized;
        402 -> payment_required;
        403 -> forbidden;
        404 -> not_found;
        405 -> method_not_allowed;
        406 -> not_acceptable;
        407 -> proxy_authentication_required;
        408 -> request_timeout;
        409 -> conflict;
        410 -> gone;
        411 -> length_required;
        412 -> precondition_failed;
        413 -> request_entity_too_large;
        414 -> request_uri_too_long;
        415 -> unsupported_media_type;
        416 -> requested_range_not_satisfiable;
        417 -> expectation_failed;
        418 -> im_a_teapot;
        todo -> unassigned;
        421 -> misdirected_request;
        422 -> unprocessable_entity;
        423 -> locked;
        424 -> failed_dependency;
        425 -> unordered_collection;
        426 -> upgrade_required;
        428 -> precondition_required;
        429 -> too_many_requests;
        431 -> request_header_fields_too_large;
        todo -> unassigned;
        451 -> unavailable_for_legal_reasons;
        todo -> unassigned;
        500 -> internal_server_error;
        501 -> not_implemented;
        502 -> bad_gateway;
        503 -> service_unavailable;
        504 -> gateway_timeout;
        505 -> http_version_not_supported;
        506 -> variant_also_negotiates;
        507 -> insufficient_storage;
        508 -> loop_detected;
        509 -> bandwidth_limit_exceeded;
        510 -> not_extended;
        511 -> network_authentication_required;
        todo -> unassigned;
        _ -> unknown
    end.

'Status.str'(Code) ->
    case Code of
        cont -> <<"Continue">>;
        switching_protocols -> <<"Switching Protocols">>;
        processing -> <<"Processing">>;
        checkpoint_draft -> <<"Checkpoint Draft">>;
        ok -> <<"OK">>;
        created -> <<"Created">>;
        accepted -> <<"Accepted">>;
        non_authoritative_information -> <<"Non Authoritative Information">>;
        no_content -> <<"No Content">>;
        reset_content -> <<"Reset Content">>;
        partial_content -> <<"Partial Content">>;
        multi_status -> <<"Multi Status">>;
        already_reported -> <<"Already Reported">>;
        im_used -> <<"IM Used">>;
        multiple_choices -> <<"Multiple Choices">>;
        moved_permanently -> <<"Moved Permanently">>;
        found -> <<"Found">>;
        see_other -> <<"See Other">>;
        not_modified -> <<"Not Modified">>;
        use_proxy -> <<"Use Proxy">>;
        switch_proxy -> <<"Switch Proxy">>;
        temporary_redirect -> <<"Temporary Redirect">>;
        permanent_redirect -> <<"Permanent Redirect">>;
        bad_request -> <<"Bad Request">>;
        unauthorized -> <<"Unauthorized">>;
        payment_required -> <<"Payment Required">>;
        forbidden -> <<"Forbidden">>;
        not_found -> <<"Not Found">>;
        method_not_allowed -> <<"Method Not Allowed">>;
        not_acceptable -> <<"Not Acceptable">>;
        proxy_authentication_required -> <<"Proxy Authentication Required">>;
        request_timeout -> <<"Request Timeout">>;
        conflict -> <<"Conflict">>;
        gone -> <<"Gone">>;
        length_required -> <<"Length Required">>;
        precondition_failed -> <<"Precondition Failed">>;
        request_entity_too_large -> <<"Request Entity Too Large">>;
        request_uri_too_long -> <<"Request URI Too Long">>;
        unsupported_media_type -> <<"Unsupported Media Type">>;
        requested_range_not_satisfiable -> <<"Requested Range Not Satisfiable">>;
        expectation_failed -> <<"Expectation Failed">>;
        im_a_teapot -> <<"Im a teapot">>;
        misdirected_request -> <<"Misdirected Request">>;
        unprocessable_entity -> <<"Unprocessable Entity">>;
        locked -> <<"Locked">>;
        failed_dependency -> <<"Failed Dependency">>;
        unordered_collection -> <<"Unordered Collection">>;
        upgrade_required -> <<"Upgrade Required">>;
        precondition_required -> <<"Precondition Required">>;
        too_many_requests -> <<"Too Many Requests">>;
        request_header_fields_too_large -> <<"Request Header Fields Too Large">>;
        unavailable_for_legal_reasons -> <<"Unavailable For Legal Reasons">>;
        internal_server_error -> <<"Internal Server Error">>;
        not_implemented -> <<"Not Implemented">>;
        bad_gateway -> <<"Bad Gateway">>;
        service_unavailable -> <<"Service Unavailable">>;
        gateway_timeout -> <<"Gateway Timeout">>;
        http_version_not_supported -> <<"HTTP Version Not Supported">>;
        variant_also_negotiates -> <<"Variant Also Negotiates">>;
        insufficient_storage -> <<"Insufficient Storage">>;
        loop_detected -> <<"Loop Detected">>;
        bandwidth_limit_exceeded -> <<"Bandwidth Limit Exceeded">>;
        not_extended -> <<"Not Extended">>;
        network_authentication_required -> <<"Network Authentication Required">>;
        unassigned -> <<"Unassigned">>;
        _ -> <<"Unknown">>
    end.

'Status.int'(Code) ->
    case lists:member(Code, [unknown, unassigned]) of
        true -> 0;
        false -> todo
        end.

'Status.is_valid'(Code) ->
    Number = 'Status.int'(Code),
    Number >= 100 andalso Number < 600.

'Status.is_error'(Code) ->
    Number = 'Status.int'(Code),
    Number >= 400 andalso Number < 600.

'Status.is_success'(Code) ->
    Number = 'Status.int'(Code),
    Number >= 100 andalso Number < 400.

fast_request_words(Line) ->
    Space1 = 'string.index'(Line, <<" ">>),
    Space2 = 'string.index_after'(Line, <<" ">>, Space1 + 1),
    Space1.

'Version.str'(V) ->
    case V of
        v1_1 -> <<"HTTP/1.1">>;
        v2_0 -> <<"HTTP/2.0">>;
        v1_0 -> <<"HTTP/1.0">>;
        unknown -> <<"unknown">>
    end.

version_from_str(V) ->
    case string:lowercase(V) of
        <<"http/1.1">> -> v1_1;
        <<"http/2.0">> -> v2_0;
        <<"http/1.0">> -> v1_0;
        _ -> unknown
    end.

'Version.protos'(V) ->
    case V of
        v1_1 -> 1;
        v2_0 -> 2;
        v1_0 -> 1;
        unknown -> 0
    end.

'SameSite__static__from'(Input) ->
    error(<<"invalid value">>).

'CommonHeader__static__from'(Input) ->
    error(<<"invalid value">>).

'Method__static__from'(Input) ->
    error(<<"invalid value">>).

'ServerStatus__static__from'(Input) ->
    error(<<"invalid value">>).

'Status__static__from'(Input) ->
    error(<<"invalid value">>).

'Version__static__from'(Input) ->
    error(<<"invalid value">>).
