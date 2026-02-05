-module('v.main').
-export([exec/1, main/0]).

exec(Path) ->
    Out = <<"">>,
    Line = <<"">>,
    Cmd = #{path => Path, {vbeam, type} => 'Command'},
    'Command.start'(Cmd),
    % TODO: for {
    Out.

main() ->
    Out = <<"">>,
    exec(<<"bash -c 'find /tmp/'">>),
    Out1 = exec(<<"echo to stdout">>),
    Out2 = exec(<<"echo to stderr 1>&2">>),
    vbeam_io:println(<<"'", (Out2)/binary, "'">>),
    % TODO: assert out == 'to stderr'
