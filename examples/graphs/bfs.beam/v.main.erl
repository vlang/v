-module('v.main').
-export([main/0, breadth_first_search_path/3]).

main() ->
    Graph = #{<<"A">> => [<<"B">>, <<"C">>], <<"B">> => [<<"A">>, <<"D">>, <<"E">>], <<"C">> => [<<"A">>, <<"F">>], <<"D">> => [<<"B">>], <<"E">> => [<<"B">>, <<"F">>], <<"F">> => [<<"C">>, <<"E">>]},
    vbeam_io:println(<<"Graph: ", (Graph)/binary>>),
    Path = breadth_first_search_path(Graph, <<"A">>, <<"F">>),
    vbeam_io:println(<<"The shortest path from node A to node F is: ", (Path)/binary>>),
    % TODO: unhandled stmt type
        ok.

breadth_first_search_path(Graph, Vertex, Target) ->
    Path = [],
    Visited = [],
    Queue = [],
    Queue bsl [[Vertex], Path],
    % TODO: unhandled stmt type
    Path.
