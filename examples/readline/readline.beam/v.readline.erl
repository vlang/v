-module('v.readline').
-export(['Readline.enable_raw_mode'/1, 'Readline.enable_raw_mode_nosig'/1, 'Readline.disable_raw_mode'/1, 'Readline.read_char'/1, 'Readline.read_line_utf8'/2, 'Readline.read_line'/2, read_line_utf8/1, read_line/1]).

'Readline.enable_raw_mode'(R) ->
    case tcgetattr(0, maps:get(orig_termios, R)) /= 0 of
        true -> ok;
        false -> begin
            Raw = maps:get(orig_termios, R),
            tcsetattr(0, 1, Raw),
        end
        end.

'Readline.enable_raw_mode_nosig'(R) ->
    case tcgetattr(0, maps:get(orig_termios, R)) /= 0 of
        true -> ok;
        false -> begin
            Raw = maps:get(orig_termios, R),
            tcsetattr(0, 1, Raw),
        end
        end.

'Readline.disable_raw_mode'(R) ->
    case maps:get(is_raw, R) of
        true -> begin
            tcsetattr(0, 1, maps:get(orig_termios, R)),
        end;
        false -> ok
    end.

'Readline.read_char'(R) ->
    todo.

'Readline.read_line_utf8'(R, Prompt) ->
    case length(maps:get(previous_lines, R)) =< 1 of
        true -> begin
            maps:get(previous_lines, R) bsl [],
            maps:get(previous_lines, R) bsl []
        end;
        false -> ok
    end,
    print(maps:get(prompt, R)),
    Line = get_raw_line(),
    case length(Line) >= 0 of
        true -> ok;
        false -> ok
    end,
    case length(maps:get(current, R)) == 0 of
        true -> error(<<"empty line">>);
        false -> maps:get(current, R)
        end.

'Readline.read_line'(R, Prompt) ->
    S = 'Readline.read_line_utf8'(R, Prompt),
    '[]rune.string'(S).

read_line_utf8(Prompt) ->
    R = #{{vbeam, type} => 'Readline'},
    S = 'Readline.read_line_utf8'(R, Prompt),
    S.

read_line(Prompt) ->
    R = #{{vbeam, type} => 'Readline'},
    S = 'Readline.read_line'(R, Prompt),
    S.
