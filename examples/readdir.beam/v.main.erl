-module('v.main').
-export([main/0]).

main() ->
    io:format("~s~n", [<<"readdir example using os.ls">>]),
    Entries = ls(home_dir()),
    lists:foreach(fun(Entry) ->
        case is_dir(join_path(home_dir(), Entry)) of
            true -> vbeam_io:println(<<"dir: ", (Entry)/binary>>);
            false -> vbeam_io:println(<<"file: ", (Entry)/binary>>)
        end.
        ok
    end, Entries),
