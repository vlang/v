-module('v.main').
-export([main/0]).

main() ->
    find_abs_path_of_executable(<<"inotifywait">>),
    Cmd = start_new_command(<<"inotifywait -q -r -m -e move,modify,create,delete .">>),
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
        ok.
