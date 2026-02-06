-module('v.term').
-export([format_esc/1, format/3, format_rgb/6, rgb/4, bg_rgb/4, hex/2, bg_hex/2, reset/1, bold/1, dim/1, italic/1, underline/1, slow_blink/1, rapid_blink/1, inverse/1, hidden/1, strikethrough/1, black/1, red/1, green/1, yellow/1, blue/1, magenta/1, cyan/1, white/1, bg_black/1, bg_red/1, bg_green/1, bg_yellow/1, bg_blue/1, bg_magenta/1, bg_cyan/1, bg_white/1, gray/1, bright_black/1, bright_red/1, bright_green/1, bright_yellow/1, bright_blue/1, bright_magenta/1, bright_cyan/1, bright_white/1, bright_bg_black/1, bright_bg_red/1, bright_bg_green/1, bright_bg_yellow/1, bright_bg_blue/1, bright_bg_magenta/1, bright_bg_cyan/1, bright_bg_white/1, highlight_command/1, write_color/3, writeln_color/3, set_cursor_position/1, move/2, cursor_up/1, cursor_down/1, cursor_forward/1, cursor_back/1, erase_display/1, erase_toend/0, erase_tobeg/0, erase_clear/0, erase_del_clear/0, erase_line/1, erase_line_toend/0, erase_line_tobeg/0, erase_line_clear/0, show_cursor/0, hide_cursor/0, clear_previous_line/0, get_terminal_size/0, clear/0, input_character/0, supports_sixel/0, enable_echo/1, graphics_num_colors/0, key_pressed/0, can_show_color_on_stdout/0, can_show_color_on_stderr/0, failed/1, ok_message/1, fail_message/1, warn_message/1, colorize/2, ecolorize/2, strip_ansi/1, h_divider/1, header_left/2, header/2, imax/2, supports_escape_sequences/1, utf8_getchar/0, utf8_len/1, 'TextStyle__static__from'/1, 'FgColor__static__from'/1, 'BgColor__static__from'/1]).

format_esc(Code) ->
    <<"[", (Code)/binary, "m">>.

format(Msg, Open, Close) ->
    <<"[", (Open)/binary, "m", (Msg)/binary, "[", (Close)/binary, "m">>.

format_rgb(R, G, B, Msg, Open, Close) ->
    <<"[", (Open)/binary, ";2;", (integer_to_binary(R))/binary, ";", (integer_to_binary(G))/binary, ";", (integer_to_binary(B))/binary, "m", (Msg)/binary, "[", (Close)/binary, "m">>.

rgb(R, G, B, Msg) ->
    format_rgb(R, G, B, Msg, <<"38">>, <<"39">>).

bg_rgb(R, G, B, Msg) ->
    format_rgb(R, G, B, Msg, <<"48">>, <<"49">>).

hex(Hex, Msg) ->
    format_rgb(Hex bsr 16, (Hex bsr 8) band 16#FF, Hex band 16#FF, Msg, <<"38">>, <<"39">>).

bg_hex(Hex, Msg) ->
    format_rgb(Hex bsr 16, (Hex bsr 8) band 16#FF, Hex band 16#FF, Msg, <<"48">>, <<"49">>).

reset(Msg) ->
    format(Msg, <<"0">>, <<"0">>).

bold(Msg) ->
    format(Msg, <<"1">>, <<"22">>).

dim(Msg) ->
    format(Msg, <<"2">>, <<"22">>).

italic(Msg) ->
    format(Msg, <<"3">>, <<"23">>).

underline(Msg) ->
    format(Msg, <<"4">>, <<"24">>).

slow_blink(Msg) ->
    format(Msg, <<"5">>, <<"25">>).

rapid_blink(Msg) ->
    format(Msg, <<"6">>, <<"26">>).

inverse(Msg) ->
    format(Msg, <<"7">>, <<"27">>).

hidden(Msg) ->
    format(Msg, <<"8">>, <<"28">>).

strikethrough(Msg) ->
    format(Msg, <<"9">>, <<"29">>).

black(Msg) ->
    format(Msg, <<"30">>, <<"39">>).

red(Msg) ->
    format(Msg, <<"31">>, <<"39">>).

green(Msg) ->
    format(Msg, <<"32">>, <<"39">>).

yellow(Msg) ->
    format(Msg, <<"33">>, <<"39">>).

blue(Msg) ->
    format(Msg, <<"34">>, <<"39">>).

magenta(Msg) ->
    format(Msg, <<"35">>, <<"39">>).

cyan(Msg) ->
    format(Msg, <<"36">>, <<"39">>).

white(Msg) ->
    format(Msg, <<"37">>, <<"39">>).

bg_black(Msg) ->
    format(Msg, <<"40">>, <<"49">>).

bg_red(Msg) ->
    format(Msg, <<"41">>, <<"49">>).

bg_green(Msg) ->
    format(Msg, <<"42">>, <<"49">>).

bg_yellow(Msg) ->
    format(Msg, <<"43">>, <<"49">>).

bg_blue(Msg) ->
    format(Msg, <<"44">>, <<"49">>).

bg_magenta(Msg) ->
    format(Msg, <<"45">>, <<"49">>).

bg_cyan(Msg) ->
    format(Msg, <<"46">>, <<"49">>).

bg_white(Msg) ->
    format(Msg, <<"47">>, <<"49">>).

gray(Msg) ->
    bright_black(Msg).

bright_black(Msg) ->
    format(Msg, <<"90">>, <<"39">>).

bright_red(Msg) ->
    format(Msg, <<"91">>, <<"39">>).

bright_green(Msg) ->
    format(Msg, <<"92">>, <<"39">>).

bright_yellow(Msg) ->
    format(Msg, <<"93">>, <<"39">>).

bright_blue(Msg) ->
    format(Msg, <<"94">>, <<"39">>).

bright_magenta(Msg) ->
    format(Msg, <<"95">>, <<"39">>).

bright_cyan(Msg) ->
    format(Msg, <<"96">>, <<"39">>).

bright_white(Msg) ->
    format(Msg, <<"97">>, <<"39">>).

bright_bg_black(Msg) ->
    format(Msg, <<"100">>, <<"49">>).

bright_bg_red(Msg) ->
    format(Msg, <<"101">>, <<"49">>).

bright_bg_green(Msg) ->
    format(Msg, <<"102">>, <<"49">>).

bright_bg_yellow(Msg) ->
    format(Msg, <<"103">>, <<"49">>).

bright_bg_blue(Msg) ->
    format(Msg, <<"104">>, <<"49">>).

bright_bg_magenta(Msg) ->
    format(Msg, <<"105">>, <<"49">>).

bright_bg_cyan(Msg) ->
    format(Msg, <<"106">>, <<"49">>).

bright_bg_white(Msg) ->
    format(Msg, <<"107">>, <<"49">>).

highlight_command(Command) ->
    bright_white(bg_cyan(<<" ", (Command)/binary, " ">>)).

write_color(B, S, Config) ->
    Codes = [],
    lists:foreach(fun(Style) ->
        Codes bsl integer_to_binary(todo),
        ok
    end, maps:get(styles, Config)),
    case todo of
        true -> Codes bsl integer_to_binary(todo);
        false -> ok
    end,
    case todo of
        true -> Codes bsl integer_to_binary(todo);
        false -> ok
    end,
    case maps:get(custom, Config) /= <<"">> of
        true -> Codes bsl maps:get(custom, Config);
        false -> ok
    end,
    case length(Codes) > 0 of
        true -> begin
            Code_str = iolist_to_binary(lists:join(<<";">>, Codes)),
            'Builder.write_string'(B, <<"[", (Code_str)/binary, "m", (S)/binary, "[0m">>)
        end;
        false -> 'Builder.write_string'(B, S)
    end.

writeln_color(B, S, Color) ->
    write_color(B, S, Color),
    'Builder.writeln'(B, <<"">>),
    ok.

set_cursor_position(C) ->
    io:format("~s", [<<(<<"[", (integer_to_binary(maps:get(y, C)))/binary, ";", (integer_to_binary(maps:get(x, C)))/binary>>)/binary, (<<"H">>)/binary>>]),
    flush_stdout(),
    ok.

move(N, Direction) ->
    io:format("~s", [<<"[", (integer_to_binary(N))/binary, (Direction)/binary>>]),
    flush_stdout(),
    ok.

cursor_up(N) ->
    move(N, <<"A">>),
    ok.

cursor_down(N) ->
    move(N, <<"B">>),
    ok.

cursor_forward(N) ->
    move(N, <<"C">>),
    ok.

cursor_back(N) ->
    move(N, <<"D">>),
    ok.

erase_display(T) ->
    io:format("~s", [<<(<<(<<"[">>)/binary, (T)/binary>>)/binary, (<<"J">>)/binary>>]),
    flush_stdout(),
    ok.

erase_toend() ->
    erase_display(<<"0">>),
    ok.

erase_tobeg() ->
    erase_display(<<"1">>),
    ok.

erase_clear() ->
    io:format("~s", [<<"\\033[H\\033[J">>]),
    flush_stdout(),
    ok.

erase_del_clear() ->
    erase_display(<<"3">>),
    ok.

erase_line(T) ->
    io:format("~s", [<<(<<(<<"[">>)/binary, (T)/binary>>)/binary, (<<"K">>)/binary>>]),
    flush_stdout(),
    ok.

erase_line_toend() ->
    erase_line(<<"0">>),
    ok.

erase_line_tobeg() ->
    erase_line(<<"1">>),
    ok.

erase_line_clear() ->
    erase_line(<<"2">>),
    ok.

show_cursor() ->
    io:format("~s", [<<"[?25h">>]),
    flush_stdout(),
    ok.

hide_cursor() ->
    io:format("~s", [<<"[?25l">>]),
    flush_stdout(),
    ok.

clear_previous_line() ->
    io:format("~s", [<<"\\r[1A[2K">>]),
    flush_stdout(),
    ok.

get_terminal_size() ->
    80.

clear() ->
    io:format("~s", [<<"[2J">>]),
    io:format("~s", [<<"[H">>]),
    true.

input_character() ->
    -1.

supports_sixel() ->
    false.

enable_echo(Enable) ->
    ok.

graphics_num_colors() ->
    0.

key_pressed() ->
    todo.

can_show_color_on_stdout() ->
    case Can_show_color_on_stdout_cache of
        1 -> true;
        -1 -> false;
        _ -> ok
    end,
    Status = supports_escape_sequences(1),
    Can_show_color_on_stdout_cache = case Status of
        true -> 1;
        false -> -1
    end,
    Status.

can_show_color_on_stderr() ->
    case Can_show_color_on_stderr_cache of
        1 -> true;
        -1 -> false;
        _ -> ok
    end,
    Status = supports_escape_sequences(2),
    Can_show_color_on_stderr_cache = case Status of
        true -> 1;
        false -> -1
    end,
    Status.

failed(S) ->
    case can_show_color_on_stdout() of
        true -> bg_red(bold(white(S)));
        false -> S
        end.

ok_message(S) ->
    case can_show_color_on_stdout() of
        true -> green(<<" ", (S)/binary, " ">>);
        false -> S
        end.

fail_message(S) ->
    failed(<<" ", (S)/binary, " ">>).

warn_message(S) ->
    case can_show_color_on_stdout() of
        true -> bright_yellow(<<" ", (S)/binary, " ">>);
        false -> S
        end.

colorize(Cfn, S) ->
    case can_show_color_on_stdout() of
        true -> cfn(S);
        false -> S
        end.

ecolorize(Cfn, S) ->
    case can_show_color_on_stderr() of
        true -> cfn(S);
        false -> S
        end.

strip_ansi(Text) ->
    Input = new(Text),
    Output = [],
    Ch = 0,
    % TODO: unhandled stmt type
    '[]u8.bytestr'(Output).

h_divider(Divider) ->
    Cols = element(1, get_terminal_size()),
    Result = <<"">>,
    case length(Divider) > 0 of
        true -> ok;
        false -> ok
    end,
    lists:nth(todo + 1, Result).

header_left(Text, Divider) ->
    Plain_text = strip_ansi(Text),
    Xcols = element(1, get_terminal_size()),
    Cols = imax(1, Xcols),
    Relement = case length(Divider) > 0 of
        true -> Divider;
        false -> <<" ">>
    end,
    Hstart = lists:nth(todo + 1, 'string.repeat'(Relement, 4)),
    Remaining_cols = imax(0, (Cols - (length(Hstart) + 1 + length(Plain_text) + 1))),
    Hend = lists:nth(todo + 1, 'string.repeat'(Relement, (Remaining_cols + 1) div length(Relement))),
    <<(Hstart)/binary, " ", (Text)/binary, " ", (Hend)/binary>>.

header(Text, Divider) ->
    case length(Text) == 0 of
        true -> h_divider(Divider);
        false -> begin
            Xcols = element(1, get_terminal_size()),
            Cols = imax(1, Xcols),
            Tlimit = imax(1, case Cols > length(Text) + 2 + 2 * length(Divider) of
                true -> length(Text);
                false -> Cols - 3 - 2 * length(Divider)
            end),
            Tlimit_aligned = case (Tlimit rem 2) /= (Cols rem 2) of
                true -> Tlimit + 1;
                false -> Tlimit
            end,
            Tstart = imax(0, (Cols - Tlimit_aligned) div 2),
            Ln = <<"">>,
            case length(Divider) > 0 of
                true -> ok;
                false -> ok
            end,
            case length(Ln) == 1 of
                true -> <<(<<(<<(<<(Ln)/binary, (<<" ">>)/binary>>)/binary, (lists:nth(todo + 1, Text))/binary>>)/binary, (<<" ">>)/binary>>)/binary, (Ln)/binary>>;
                false -> <<(<<(<<(<<(lists:nth(todo + 1, Ln))/binary, (<<" ">>)/binary>>)/binary, (lists:nth(todo + 1, Text))/binary>>)/binary, (<<" ">>)/binary>>)/binary, (lists:nth(todo + 1, Ln))/binary>>
                        end
        end
        end.

imax(X, Y) ->
    case X > Y of
        true -> X;
        false -> Y
    end.

supports_escape_sequences(Fd) ->
    Vcolors_override = getenv(<<"VCOLORS">>),
    % TODO: unhandled stmt type
    case Vcolors_override == <<"always">> of
        true -> true;
        false -> 
            case Vcolors_override == <<"never">> of
                true -> false;
                false -> begin
                    Env_term = getenv(<<"TERM">>),
                    % TODO: unhandled stmt type
                    case Env_term == <<"dumb">> of
                        true -> false;
                        false -> 
                                        end
                end
                        end
                end.

utf8_getchar() ->
    C = input_character(),
    case C == -1 of
        true -> todo;
        false -> begin
            Len = utf8_len(todo),
            case C < 0 of
                true -> 0;
                false -> case Len == 0 of
                    true -> C;
                    false -> case Len == 1 of
                        true -> -1;
                        false -> begin
                            Uc = C band ((1 bsl (7 - Len)) - 1),
                            % TODO: unhandled stmt type
                            Uc
                        end
                    end
                end
            end
        end
        end.

utf8_len(C) ->
    B = 0,
    X = C,
    case (X band 240) /= 0 of
        true -> ok;
        false -> ok
    end,
    case (X band 12) /= 0 of
        true -> ok;
        false -> ok
    end,
    case (X band 2) == 0 of
        true -> todo;
        false -> ok
    end,
    B.

'TextStyle__static__from'(Input) ->
    error(<<"invalid value">>).

'FgColor__static__from'(Input) ->
    error(<<"invalid value">>).

'BgColor__static__from'(Input) ->
    error(<<"invalid value">>).
