-module('v.main').
-export([find_pattern/3, print_pattern/1, visited_init/1, main/0]).

find_pattern(Adj, Start, Target) ->
    Visited = visited_init(Adj),
    Queue = #{{vbeam, type} => 'Queue[[]string]'},
    Res = [],
    'Queue[[]string].push'(Queue, [Start]),
    % TODO: unhandled stmt type
    ok    Res.

print_pattern(Pat) ->
    lists:foreach(fun(P) ->
        vbeam_io:println(P),
        ok.
        ok
    end, Pat),

visited_init(Adj) ->
    Temp = #{},
    lists:foreach(fun(_) ->
        ok
    end, Adj),
    Temp.

main() ->
    Adj = #{<<"A">> => [<<"B">>, <<"C">>], <<"B">> => [<<"A">>, <<"D">>, <<"E">>], <<"C">> => [<<"A">>, <<"F">>], <<"D">> => [<<"B">>], <<"E">> => [<<"B">>, <<"F">>], <<"F">> => [<<"C">>, <<"E">>]},
    Path = find_pattern(Adj, <<"A">>, <<"F">>),
    io:format("~s~n", [<<"The all pattern path from node A to node F is:">>]),
    print_pattern(Path),
    ok.
