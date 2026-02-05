-module('v.main').
-export([increment/1, main/0]).

increment(Data) ->
    todo,
    ok.

main() ->
    Data = #{{vbeam, type} => 'SharedData'},
    Threads = [todo, todo],
    lists:foreach(fun(T) ->
        'thread.wait'(T),
        ok
    end, Threads),
    todo,
    ok.
