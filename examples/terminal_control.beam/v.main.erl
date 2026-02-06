-module('v.main').
-export([main/0, sleeping_line/4, standing_line/4]).

main() ->
    erase_clear(),
    sleeping_line(5, 5, 5, <<"*">>),
    standing_line(5, 5, 5, <<"*">>),
    sleeping_line(5, 10, 5, <<"*">>),
    standing_line(9, 5, 5, <<"*">>),
    cursor_down(5),
    print(<<"\\n">>),
    vbeam_io:println(bold(red(<<"It Worked!">>))),
    ok.

sleeping_line(X, Y, Size, Ch) ->
    I = 0,
    % TODO: unhandled stmt type
    ok
standing_line(X, Y, Size, Ch) ->
    I = 0,
    % TODO: unhandled stmt type
    ok