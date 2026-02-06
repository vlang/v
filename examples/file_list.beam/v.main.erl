-module('v.main').
-export([main/0]).

main() ->
    Files = ls(<<".">>),
    F = create(<<"file_list.txt">>),
    lists:foreach(fun(File) ->
        case is_file(File) of
            true -> 'File.write_string'(F, <<(File)/binary, (<<"\\r\\n">>)/binary>>);
            false -> ok
        end,
        ok
    end, Files),
    'File.close'(F),
    ok.
