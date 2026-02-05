-module('v.main').
-export([main/0]).

main() ->
    case length(arguments()) == 1 of
        true -> begin
            eprintln(<<"A small `tail -f file` like program, written in V.">>),
            eprintln(<<"Usage: `v run examples/vtail.v your_long_file.log`">>),
            exit(0)
        end;
        false -> ok
    end,
    Tfile = lists:nth(2, arguments()),
    F = open_file(Tfile, <<"r">>),
    'File.seek'(F, 0, end),
    Read_pos = 'File.tell'(F),
    Buf = [],
    % TODO: for {
