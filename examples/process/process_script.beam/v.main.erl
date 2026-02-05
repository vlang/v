-module('v.main').
-export([exec/2, main/0]).

exec(Path, Redirect) ->
    Line = <<"">>,
    Line_err = <<"">>,
    Cmd = new_process(<<"/bin/bash">>),
    case Redirect of
        true -> 'Process.set_args'(Cmd, [<<"-c">>, <<"/bin/bash /tmp/test.sh 2>&1">>]);
        false -> 'Process.set_args'(Cmd, [Path])
    end,
    'Process.set_redirect_stdio'(Cmd),
    'Process.run'(Cmd),
    % TODO: for cmd.is_alive() {
    case maps:get(code, Cmd) > 0 of
        true -> begin
            io:format("~s~n", [<<"ERROR:">>]),
            vbeam_io:println(Cmd)
        end;
        false -> ok
    end.

main() ->
    Script = <<"\necho line 1\n#will use some stderr now\necho redirect 1 to 2  1>&2\necho line 3\n">>,
    write_file(<<"/tmp/test.sh">>, Script),
    exec(<<"/tmp/test.sh">>, false),
    exec(<<"/tmp/test.sh">>, true),
    ok.
