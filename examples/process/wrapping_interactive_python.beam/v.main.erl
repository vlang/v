-module('v.main').
-export([main/0]).

main() ->
    Python_exe = find_abs_path_of_executable(<<"python">>),
    P = new_process(Python_exe),
    % TODO: defer {dump(p.code);p.close();p.wait();}
    'Process.set_args'(P, [<<"-i">>, <<"-q">>, <<"-u">>]),
    'Process.set_redirect_stdio'(P),
    'Process.run'(P),
    todo,
    io:format("~s~n", [<<"This is a simple V wrapper/shell for the Python interpreter.">>]),
    io:format("~s~n", [<<"Try typing some python code here, or type `bye` to end your session:">>]),
    % TODO: for p.is_alive() {
