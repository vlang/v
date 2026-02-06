-module('v.main').
-export([main/0]).

main() ->
    case length(init:get_plain_arguments()) == 1 of
        true -> begin
            io:format(standard_error, "~s~n", [<<"A small `tail -f file` like program, written in V.">>]),
            io:format(standard_error, "~s~n", [<<"Usage: `v run examples/vtail.v your_long_file.log`">>]),
            exit(0)
        end;
        false -> ok
    end,
    Tfile = lists:nth(2, init:get_plain_arguments()),
    F = open_file(Tfile, <<"r">>),
    'File.seek'(F, 0, end),
    Read_pos = 'File.tell'(F),
    Buf = [],
    % TODO: unhandled stmt type
        ok.
