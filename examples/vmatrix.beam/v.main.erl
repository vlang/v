-module('v.main').
-export([main/0, rain/0, update_rain_column/3, random_rain_column/2, print_rain_column/2, print_at/3, random_symbol/0, random_dim/1, init_terminal/0]).

main() ->
    init_terminal(),
    rain(),
    ok.

rain() ->
    Rain_columns = [],
    Width = 0,
    Height = 0,
    % TODO: unhandled stmt type
    ok
update_rain_column(Rc, Width, Height) ->
    case maps:get(head, Rc) > Height + length(Rc) of
        true -> ok;
        false -> ok
    end.

random_rain_column(Max_col, Max_height) ->
    #{col => int_in_range(1, Max_col + 1), len => int_in_range(4, Max_height div 4 * 3), {vbeam, type} => 'RainColumn'}.

print_rain_column(Rc, Height) ->
    case maps:get(head, Rc) =< Height of
        true -> print_at(gray(random_symbol()), maps:get(col, Rc), maps:get(head, Rc));
        false -> ok
    end,
    case (maps:get(head, Rc) - 1) =< Height of
        true -> begin
            Symbol = random_dim(green(random_symbol())),
            print_at(Symbol, maps:get(col, Rc), maps:get(head, Rc) - 1)
        end;
        false -> ok
    end,
    Tail = maps:get(head, Rc) - length(Rc) + 1,
    case Tail > 0 andalso Tail =< Height of
        true -> print_at(<<" ">>, maps:get(col, Rc), Tail);
        false -> ok
    end.

print_at(S, X, Y) ->
    set_cursor_position(#{x => X, y => Y, {vbeam, type} => 'Coord'}),
    print(S),
    ok.

random_symbol() ->
    Idx = int_in_range(0, length(<<"0123456789!@#$%^&*()-=+[]{}|;:<>?~bdjpqtvz">>)),
    'u8.ascii_str'(lists:nth(Idx + 1, <<"0123456789!@#$%^&*()-=+[]{}|;:<>?~bdjpqtvz">>)).

random_dim(S) ->
    I = int_in_range(0, 10),
    case I == 1 of
        true -> dim(S);
        false -> S
    end.

init_terminal() ->
    Old_state = #{{vbeam, type} => 'Termios'},
    tcgetattr(0, Old_state),
    at_exit(todo),
    signal_opt(int, todo),
    New_state = Old_state,
    'Termios.disable_echo'(New_state),
    set_state(0, New_state),
    print(<<"\\e[?1049h\\e[?25l">>),
    ok.
