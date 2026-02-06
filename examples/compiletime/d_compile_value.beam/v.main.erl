-module('v.main').
-export([println_padded/1, main/0]).

println_padded(Str) ->
    lists:foreach(fun(_) ->
        print(todo),
        ok
    end, lists:seq(0, todo - 1)),
    vbeam_io:println(<<" ", (Str)/binary>>),
    ok.

main() ->
    Header = todo,
    case Header of
        true -> println_padded(<<"Compile Value Example">>);
        false -> ok
    end,
    App = #{{vbeam, type} => 'App'},
    lists:foreach(fun(Job) ->
        println_padded(<<(maps:get(id, Job))/binary, " ", (integer_to_binary(I + 1))/binary>>),
        ok
    end, maps:get(jobs, App)),
    case todo of
        true -> vbeam_io:println(<<"\nAvailable compile time flags:\n -d pad=<i64>\n -d pad_char=<character>\n -d id=\"<string>\"\n -d jobs=<i64>\n -d header=<bool>\nYou can turn this message off with:\n -d footer=false">>);
        false -> ok
    end.
