-module('v.toml.scanner').
-export([new_scanner/1, new_simple/1, new_simple_text/1, new_simple_file/1, 'Scanner.scan'/1, 'Scanner.free'/1, 'Scanner.remaining'/1, 'Scanner.next'/1, 'Scanner.skip'/1, 'Scanner.skip_n'/2, 'Scanner.at'/1, 'Scanner.at_crlf'/1, 'Scanner.peek'/2, 'Scanner.reset'/1, 'Scanner.new_token'/4, 'Scanner.ignore_line'/1, 'Scanner.inc_line_number'/1, 'Scanner.extract_key'/1, 'Scanner.extract_string'/1, 'Scanner.extract_multiline_string'/1, 'Scanner.handle_escapes'/3, 'Scanner.extract_number'/1, 'Scanner.extract_nan_or_inf_number'/1, 'Scanner.excerpt'/3, 'Scanner.state'/1, 'Scanner.validate_and_skip_headers'/1, 'Scanner.check_utf16_or_32_bom'/1]).

new_scanner(Config) ->
    S = #{config => Config, text => 'Config.read_input'(maps:get(input, Config)), {vbeam, type} => 'Scanner'},
    S.

new_simple(Config) ->
    #{config => Config, text => 'Config.read_input'(maps:get(input, Config)), {vbeam, type} => 'Scanner'}.

new_simple_text(Text) ->
    In_config = #{text => Text, {vbeam, type} => 'Config'},
    Config = #{input => In_config, {vbeam, type} => 'Config'},
    #{config => Config, text => 'Config.read_input'(maps:get(input, Config)), {vbeam, type} => 'Scanner'}.

new_simple_file(Path) ->
    In_config = #{file_path => Path, {vbeam, type} => 'Config'},
    Config = #{input => In_config, {vbeam, type} => 'Config'},
    #{config => Config, text => 'Config.read_input'(maps:get(input, Config)), {vbeam, type} => 'Scanner'}.

'Scanner.scan'(S) ->
    'Scanner.validate_and_skip_headers'(S),
    % TODO: unhandled stmt type
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"unknown character code at ", (integer_to_binary(maps:get(pos, S)))/binary, " (", (integer_to_binary(maps:get(line_nr, S)))/binary, ",", (integer_to_binary(maps:get(col, S)))/binary, ") near ...", ('Scanner.excerpt'(S, maps:get(pos, S), 5))/binary, "...">>),
    'Scanner.new_token'(S, unknown, <<"">>, 0).

'Scanner.free'(S) ->
    todo,
    ok.

'Scanner.remaining'(S) ->
    length(maps:get(text, S)) - maps:get(pos, S).

'Scanner.next'(S) ->
    case maps:get(pos, S) < length(maps:get(text, S)) of
        true -> C;
        false -> todo
        end.

'Scanner.skip'(S) ->
    case maps:get(pos, S) + 1 < length(maps:get(text, S)) of
        true -> begin
            todo,
            todo
        end;
        false -> ok
    end.

'Scanner.skip_n'(S, N) ->
    case maps:get(pos, S) > length(maps:get(text, S)) of
        true -> ok;
        false -> ok
    end,

'Scanner.at'(S) ->
    case maps:get(pos, S) < length(maps:get(text, S)) of
        true -> lists:nth(maps:get(pos, S) + 1, maps:get(text, S));
        false -> todo
        end.

'Scanner.at_crlf'(S) ->
    'Scanner.at'(S) == todo andalso 'Scanner.peek'(S, 1) == todo.

'Scanner.peek'(S, N) ->
    case maps:get(pos, S) + N < length(maps:get(text, S)) of
        true -> lists:nth(maps:get(pos, S) + N + 1, maps:get(text, S));
        false -> todo
        end.

'Scanner.reset'(S) ->

'Scanner.new_token'(S, Kind, Lit, Len) ->
    Col = maps:get(col, S) - Len + 1,
    case maps:get(line_nr, S) == 1 of
        true -> ok;
        false -> ok
    end,
    #{kind => Kind, lit => Lit, col => case Col < 1 of
        true -> 1;
        false -> Col
    end, line_nr => maps:get(line_nr, S) + 1, pos => maps:get(pos, S) - maps:get(header_len, S) - Len + 1, len => Len, {vbeam, type} => 'Token'}.

'Scanner.ignore_line'(S) ->
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<" ignoring until EOL...">>),
    Start = maps:get(pos, S),
    % TODO: unhandled stmt type
    lists:nth(todo + 1, maps:get(text, S)).

'Scanner.inc_line_number'(S) ->
    todo,

'Scanner.extract_key'(S) ->
    todo,
    todo,
    Start = maps:get(pos, S),
    % TODO: unhandled stmt type
    Key = lists:nth(todo + 1, maps:get(text, S)),
    Key.

'Scanner.extract_string'(S) ->
    todo,
    todo,
    Quote = todo,
    Start = maps:get(pos, S),
    Lit = 'u8.ascii_str'(Quote),
    Is_multiline = lists:nth(maps:get(pos, S) + 1 + 1, maps:get(text, S)) == Quote andalso lists:nth(maps:get(pos, S) + 2 + 1, maps:get(text, S)) == Quote,
    case Is_multiline of
        true -> Mls;
        false -> begin
            % TODO: unhandled stmt type
            Lit
        end
        end.

'Scanner.extract_multiline_string'(S) ->
    Quote = todo,
    Start = maps:get(pos, S),
    Lit = <<(<<('u8.ascii_str'(Quote))/binary, ('u8.ascii_str'(Quote))/binary>>)/binary, ('u8.ascii_str'(Quote))/binary>>,
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"multi-line `", ('u8.ascii_str'(Quote))/binary, ('u8.ascii_str'(lists:nth(maps:get(pos, S) + 1 + 1, maps:get(text, S))))/binary, ('u8.ascii_str'(lists:nth(maps:get(pos, S) + 2 + 1, maps:get(text, S))))/binary, "` string started at pos ", (integer_to_binary(Start))/binary, " (", (integer_to_binary(maps:get(line_nr, S)))/binary, ",", (integer_to_binary(maps:get(col, S)))/binary, ") (quote type: ", ('u8.ascii_str'(Quote))/binary, " / ", (integer_to_binary(Quote))/binary, ")">>),
    % TODO: unhandled stmt type
    Lit.

'Scanner.handle_escapes'(S, Quote, Is_multiline) ->
    C = todo,
    Lit = 'u8.ascii_str'(C),
    Is_literal_string = Quote == todo,
    case not Is_literal_string of
        true -> case 'Scanner.peek'(S, 1) == todo andalso 'u8.is_hex_digit'(todo) andalso 'u8.is_hex_digit'(todo) andalso 'u8.is_hex_digit'(todo) andalso 'u8.is_hex_digit'(todo) of
            true -> begin
                Lit1 = lists:nth(todo + 1, maps:get(text, S)),
                printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"gulp escaped unicode `", (Lit1)/binary, "`">>),
                Lit1
            end;
            false -> case 'Scanner.peek'(S, 1) == Quote of
                true -> begin
                    case (not Is_multiline andalso 'Scanner.peek'(S, 2) == todo) orelse (Is_multiline andalso 'Scanner.peek'(S, 2) == Quote andalso 'Scanner.peek'(S, 3) == Quote andalso 'Scanner.peek'(S, 4) == todo) of
                        true -> begin
                            printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"ignore special case escaped `", (Lit1)/binary, "` at end of string">>),
                            <<"">>
                        end;
                        false -> ok
                    end,
                    Lit2 = 'u8.ascii_str'(Quote),
                    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"gulp escaped `", (Lit2)/binary, "`">>),
                    Lit2
                end;
                false -> ok
            end
        end;
        false -> ok
    end,
    case Is_literal_string of
        true -> case 'Scanner.peek'(S, 1) == Quote of
            true -> begin
                printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"ignore escape `", (Lit2)/binary, ('u8.ascii_str'(todo))/binary, "` in literal string">>),
                <<"">>
            end;
            false -> ok
        end;
        false -> ok
    end,
    Lit3 = 'u8.ascii_str'(todo),
    printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"gulp escaped `", (Lit3)/binary, "`">>),
    Lit3.

'Scanner.extract_number'(S) ->
    todo,
    todo,
    Start = maps:get(pos, S),
    C = 'Scanner.at'(S),
    Is_digit = 'u8.is_digit'(todo),
    case not (Is_digit orelse lists:member(C, [todo, todo])) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" ", ('u8.ascii_str'(todo))/binary, " is not a number at ", ('Scanner.excerpt'(S, maps:get(pos, S), 10))/binary>>)/binary>>);
        false -> begin
            todo,
            todo,
            % TODO: unhandled stmt type
            Key = lists:nth(todo + 1, maps:get(text, S)),
            printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"identified number \"", (Key)/binary, "\" in range [", (integer_to_binary(Start))/binary, " .. ", (integer_to_binary(maps:get(pos, S)))/binary, "]">>),
            Key
        end
        end.

'Scanner.extract_nan_or_inf_number'(S) ->
    todo,
    todo,
    Start = maps:get(pos, S),
    C = 'Scanner.at'(S),
    case (not lists:member(C, [todo, todo, todo, todo])) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" ", ('u8.ascii_str'(todo))/binary, " is not a number at ", ('Scanner.excerpt'(S, maps:get(pos, S), 10))/binary>>)/binary>>);
        false -> begin
            todo,
            todo,
            % TODO: unhandled stmt type
            Key = lists:nth(todo + 1, maps:get(text, S)),
            printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"identified special number \"", (Key)/binary, "\" in range [", (integer_to_binary(Start))/binary, " .. ", (integer_to_binary(maps:get(pos, S)))/binary, "]">>),
            Key
        end
        end.

'Scanner.excerpt'(S, Pos, Margin) ->
    Start = case Pos > 0 andalso Pos >= Margin of
        true -> Pos - Margin;
        false -> 0
    end,
    End = case Pos + Margin < length(maps:get(text, S)) of
        true -> Pos + Margin;
        false -> length(maps:get(text, S))
    end,
    binary:replace(lists:nth(todo + 1, maps:get(text, S)), <<"\\n">>, <<"\\n">>, [global]).

'Scanner.state'(S) ->
    #{col => maps:get(col, S), line_nr => maps:get(line_nr, S), pos => maps:get(pos, S), {vbeam, type} => 'State'}.

'Scanner.validate_and_skip_headers'(S) ->
    'Scanner.check_utf16_or_32_bom'(S),
    case 'Scanner.at'(S) == 16#EF andalso 'Scanner.peek'(S, 1) == 16#BB andalso 'Scanner.peek'(S, 2) == 16#BF of
        true -> begin
            printdbg(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>, <<"skipping UTF-8 byte order mark (BOM)">>),
            'Scanner.skip_n'(S, maps:get(header_len, S))
        end;
        false -> ok
    end,
    'Scanner.check_utf16_or_32_bom'(S),
    ok.

'Scanner.check_utf16_or_32_bom'(S) ->
    case ('Scanner.at'(S) == 16#FF andalso 'Scanner.peek'(S, 1) == 16#FE andalso 'Scanner.peek'(S, 2) == 16#00 andalso 'Scanner.peek'(S, 3) == 16#00) orelse ('Scanner.at'(S) == 16#00 andalso 'Scanner.peek'(S, 1) == 16#00 andalso 'Scanner.peek'(S, 2) == 16#FE andalso 'Scanner.peek'(S, 3) == 16#FF) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" UTF-32 is not a valid TOML encoding at ", (integer_to_binary(maps:get(pos, S)))/binary, " (", (integer_to_binary(maps:get(line_nr, S)))/binary, ",", (integer_to_binary(maps:get(col, S)))/binary, ") near ...", ('Scanner.excerpt'(S, maps:get(pos, S), 5))/binary, "...">>)/binary>>);
        false -> 
            case ('Scanner.at'(S) == 16#FE andalso 'Scanner.peek'(S, 1) == 16#FF) orelse ('Scanner.at'(S) == 16#FF andalso 'Scanner.peek'(S, 1) == 16#FE) of
                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" UTF-16 is not a valid TOML encoding at ", (integer_to_binary(maps:get(pos, S)))/binary, " (", (integer_to_binary(maps:get(line_nr, S)))/binary, ",", (integer_to_binary(maps:get(col, S)))/binary, ") near ...", ('Scanner.excerpt'(S, maps:get(pos, S), 5))/binary, "...">>)/binary>>);
                false -> ok
                        end
                end.
