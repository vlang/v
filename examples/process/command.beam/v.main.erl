-module('v.main').
-export([exec/1, main/0]).

exec(Path) ->
    Out = <<"">>,
    Line = <<"">>,
    Cmd = #{path => Path, {vbeam, type} => 'Command'},
    'Command.start'(Cmd),
    % TODO: unhandled stmt type
    Out.

main() ->
    Out = <<"">>,
    exec(<<"bash -c 'find /tmp/'">>),
    Out1 = exec(<<"echo to stdout">>),
    Out2 = exec(<<"echo to stderr 1>&2">>),
    vbeam_io:println(<<"'", (Out2)/binary, "'">>),
    % TODO: unhandled stmt type
        ok.
