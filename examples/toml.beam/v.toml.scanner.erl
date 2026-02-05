-module('v.toml.scanner').
-export([new_scanner/1, new_simple/1, new_simple_text/1, new_simple_file/1, 'Scanner.scan'/1, 'Scanner.free'/1, 'Scanner.remaining'/1, 'Scanner.next'/1, 'Scanner.skip'/1, 'Scanner.skip_n'/2, 'Scanner.at'/1, 'Scanner.at_crlf'/1, 'Scanner.peek'/2, 'Scanner.reset'/1, 'Scanner.new_token'/4, 'Scanner.ignore_line'/1, 'Scanner.inc_line_number'/1, 'Scanner.extract_key'/1, 'Scanner.extract_string'/1, 'Scanner.extract_multiline_string'/1, 'Scanner.handle_escapes'/3, 'Scanner.extract_number'/1, 'Scanner.extract_nan_or_inf_number'/1, 'Scanner.excerpt'/3, 'Scanner.state'/1, 'Scanner.validate_and_skip_headers'/1, 'Scanner.check_utf16_or_32_bom'/1]).
% TODO: const digit_extras = [`_`, `.`, `x`, `o`, `b`, `e`, `E`];
% TODO: const end_of_text = u32(~0);

new_scanner(Config) ->
    S = &#{config => Config, text => 'Config.read_input'(maps:get(input, Config)), {vbeam, type} => 'Scanner'},
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
    % TODO: for {
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"unknown character code at ", (integer_to_binary(maps:get(pos, S)))/binary, " (", (integer_to_binary(maps:get(line_nr, S)))/binary, ",", (integer_to_binary(maps:get(col, S)))/binary, ") near ...", ('Scanner.excerpt'(S, maps:get(pos, S), 5))/binary, "...">>),
    'Scanner.new_token'(S, unknown, <<"">>, 0).

'Scanner.free'(S) ->
    todo,
    ok.

'Scanner.remaining'(S) ->
    length(maps:get(text, S)) - maps:get(pos, S).

'Scanner.next'(S) ->
    case maps:get(pos, S) < length(maps:get(text, S)) of
        true -> begin
            Opos = maps:get(pos, S),
            todo,
            todo,
            C = lists:nth(Opos + 1, maps:get(text, S)),
            C
        end;
        false -> ok
    end,
    todo.

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
        false -> ok
    end,
    todo.

'Scanner.at_crlf'(S) ->
    'Scanner.at'(S) == todo && 'Scanner.peek'(S, 1) == todo.

'Scanner.peek'(S, N) ->
    case maps:get(pos, S) + N < length(maps:get(text, S)) of
        true -> begin
            case N - 1 < 0 && maps:get(pos, S) + N - 1 >= 0 of
                true -> lists:nth(maps:get(pos, S) + N - 1 + 1, maps:get(text, S));
                false -> ok
            end,
            lists:nth(maps:get(pos, S) + N + 1, maps:get(text, S))
        end;
        false -> ok
    end,
    todo.

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
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<" ignoring until EOL...">>),
    Start = maps:get(pos, S),
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
    lists:nth(todo + 1, maps:get(text, S)).

'Scanner.inc_line_number'(S) ->
    todo,

'Scanner.extract_key'(S) ->
    todo,
    todo,
    Start = maps:get(pos, S),
    % TODO: for s.pos < s.text.len {
    Key = lists:nth(todo + 1, maps:get(text, S)),
    Key.

'Scanner.extract_string'(S) ->
    todo,
    todo,
    Quote = todo,
    Start = maps:get(pos, S),
    Lit = 'u8.ascii_str'(Quote),
    Is_multiline = lists:nth(maps:get(pos, S) + 1 + 1, maps:get(text, S)) == Quote && lists:nth(maps:get(pos, S) + 2 + 1, maps:get(text, S)) == Quote,
    case Is_multiline of
        true -> begin
            Mls = 'Scanner.extract_multiline_string'(S),
            Mls
        end;
        false -> ok
    end,
    % TODO: for {
    Lit.

'Scanner.extract_multiline_string'(S) ->
    Quote = todo,
    Start = maps:get(pos, S),
    Lit = 'u8.ascii_str'(Quote) + 'u8.ascii_str'(Quote) + 'u8.ascii_str'(Quote),
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"multi-line `", ('u8.ascii_str'(Quote))/binary, ('u8.ascii_str'(lists:nth(maps:get(pos, S) + 1 + 1, maps:get(text, S))))/binary, ('u8.ascii_str'(lists:nth(maps:get(pos, S) + 2 + 1, maps:get(text, S))))/binary, "` string started at pos ", (integer_to_binary(Start))/binary, " (", (integer_to_binary(maps:get(line_nr, S)))/binary, ",", (integer_to_binary(maps:get(col, S)))/binary, ") (quote type: ", ('u8.ascii_str'(Quote))/binary, " / ", (integer_to_binary(Quote))/binary, ")">>),
    % TODO: for {
    Lit.

'Scanner.handle_escapes'(S, Quote, Is_multiline) ->
    C = todo,
    Lit = 'u8.ascii_str'(C),
    Is_literal_string = Quote == todo,
    case !Is_literal_string of
        true -> case 'Scanner.peek'(S, 1) == todo && 'u8.is_hex_digit'(todo) && 'u8.is_hex_digit'(todo) && 'u8.is_hex_digit'(todo) && 'u8.is_hex_digit'(todo) of
            true -> begin
                Lit1 = lists:nth(todo + 1, maps:get(text, S)),
                printdbg(todo + <<".">> + todo + <<".">> + todo, <<"gulp escaped unicode `", (Lit1)/binary, "`">>),
                Lit1
            end;
            false -> case 'Scanner.peek'(S, 1) == Quote of
                true -> begin
                    case (!Is_multiline && 'Scanner.peek'(S, 2) == todo) || (Is_multiline && 'Scanner.peek'(S, 2) == Quote && 'Scanner.peek'(S, 3) == Quote && 'Scanner.peek'(S, 4) == todo) of
                        true -> begin
                            printdbg(todo + <<".">> + todo + <<".">> + todo, <<"ignore special case escaped `", (Lit1)/binary, "` at end of string">>),
                            <<"">>
                        end;
                        false -> ok
                    end,
                    Lit2 = 'u8.ascii_str'(Quote),
                    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"gulp escaped `", (Lit2)/binary, "`">>),
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
                printdbg(todo + <<".">> + todo + <<".">> + todo, <<"ignore escape `", (Lit2)/binary, ('u8.ascii_str'(todo))/binary, "` in literal string">>),
                <<"">>
            end;
            false -> ok
        end;
        false -> ok
    end,
    Lit3 = 'u8.ascii_str'(todo),
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"gulp escaped `", (Lit3)/binary, "`">>),
    Lit3.

'Scanner.extract_number'(S) ->
    todo,
    todo,
    Start = maps:get(pos, S),
    C = 'Scanner.at'(S),
    Is_digit = 'u8.is_digit'(todo),
    case !(Is_digit || C in [todo, todo]) of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" ", ('u8.ascii_str'(todo))/binary, " is not a number at ", ('Scanner.excerpt'(S, maps:get(pos, S), 10))/binary>>);
        false -> ok
    end,
    todo,
    todo,
    % TODO: for s.pos < s.text.len {
    Key = lists:nth(todo + 1, maps:get(text, S)),
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"identified number \"", (Key)/binary, "\" in range [", (integer_to_binary(Start))/binary, " .. ", (integer_to_binary(maps:get(pos, S)))/binary, "]">>),
    Key.

'Scanner.extract_nan_or_inf_number'(S) ->
    todo,
    todo,
    Start = maps:get(pos, S),
    C = 'Scanner.at'(S),
    case C !in [todo, todo, todo, todo] of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" ", ('u8.ascii_str'(todo))/binary, " is not a number at ", ('Scanner.excerpt'(S, maps:get(pos, S), 10))/binary>>);
        false -> ok
    end,
    todo,
    todo,
    % TODO: for s.pos < s.text.len {
    Key = lists:nth(todo + 1, maps:get(text, S)),
    printdbg(todo + <<".">> + todo + <<".">> + todo, <<"identified special number \"", (Key)/binary, "\" in range [", (integer_to_binary(Start))/binary, " .. ", (integer_to_binary(maps:get(pos, S)))/binary, "]">>),
    Key.

'Scanner.excerpt'(S, Pos, Margin) ->
    Start = case Pos > 0 && Pos >= Margin of
        true -> Pos - Margin;
        false -> 0
    end,
    End = case Pos + Margin < length(maps:get(text, S)) of
        true -> Pos + Margin;
        false -> length(maps:get(text, S))
    end,
    'string.replace'(lists:nth(todo + 1, maps:get(text, S)), <<"\\n">>, <<"\\n">>).

'Scanner.state'(S) ->
    #{col => maps:get(col, S), line_nr => maps:get(line_nr, S), pos => maps:get(pos, S), {vbeam, type} => 'State'}.

'Scanner.validate_and_skip_headers'(S) ->
    'Scanner.check_utf16_or_32_bom'(S),
    case 'Scanner.at'(S) == 0xEF && 'Scanner.peek'(S, 1) == 0xBB && 'Scanner.peek'(S, 2) == 0xBF of
        true -> begin
            printdbg(todo + <<".">> + todo + <<".">> + todo, <<"skipping UTF-8 byte order mark (BOM)">>),
            'Scanner.skip_n'(S, maps:get(header_len, S))
        end;
        false -> ok
    end,
    'Scanner.check_utf16_or_32_bom'(S),
    ok.

'Scanner.check_utf16_or_32_bom'(S) ->
    case ('Scanner.at'(S) == 0xFF && 'Scanner.peek'(S, 1) == 0xFE && 'Scanner.peek'(S, 2) == 0x00 && 'Scanner.peek'(S, 3) == 0x00) || ('Scanner.at'(S) == 0x00 && 'Scanner.peek'(S, 1) == 0x00 && 'Scanner.peek'(S, 2) == 0xFE && 'Scanner.peek'(S, 3) == 0xFF) of
        true -> begin
            'Scanner.skip_n'(S, maps:get(header_len, S)),
            error(todo + <<".">> + todo + <<".">> + todo + <<" UTF-32 is not a valid TOML encoding at ", (integer_to_binary(maps:get(pos, S)))/binary, " (", (integer_to_binary(maps:get(line_nr, S)))/binary, ",", (integer_to_binary(maps:get(col, S)))/binary, ") near ...", ('Scanner.excerpt'(S, maps:get(pos, S), 5))/binary, "...">>)
        end;
        false -> ok
    end,
    case ('Scanner.at'(S) == 0xFE && 'Scanner.peek'(S, 1) == 0xFF) || ('Scanner.at'(S) == 0xFF && 'Scanner.peek'(S, 1) == 0xFE) of
        true -> begin
            'Scanner.skip_n'(S, maps:get(header_len, S)),
            error(todo + <<".">> + todo + <<".">> + todo + <<" UTF-16 is not a valid TOML encoding at ", (integer_to_binary(maps:get(pos, S)))/binary, " (", (integer_to_binary(maps:get(line_nr, S)))/binary, ",", (integer_to_binary(maps:get(col, S)))/binary, ") near ...", ('Scanner.excerpt'(S, maps:get(pos, S), 5))/binary, "...">>)
        end;
        false -> ok
    end,
    ok.
