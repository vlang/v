-module('v.main').
-export([main/0]).

main() ->
    execve(find_abs_path_of_executable(<<"ls">>), [<<"-lh">>, <<"-s">>], []),
    execve(find_abs_path_of_executable(<<"bash">>), [<<"-c">>, <<"ls -lah -s">>], []),
    execve(find_abs_path_of_executable(<<"man">>), [<<"true">>], [<<"MANWIDTH=60">>]),
    ok.
