-module('v.main').
-export([main/0, breadth_first_search_path/3, departure/1, visited_init/1, build_path_reverse/4]).

main() ->
    Graph = #{<<"A">> => [<<"B">>, <<"C">>], <<"B">> => [<<"A">>, <<"D">>, <<"E">>], <<"C">> => [<<"A">>, <<"F">>], <<"D">> => [<<"B">>], <<"E">> => [<<"B">>, <<"F">>], <<"F">> => [<<"C">>, <<"E">>]},
    vbeam_io:println(<<"Graph: ", (Graph)/binary>>),
    Path = breadth_first_search_path(Graph, <<"A">>, <<"F">>),
    vbeam_io:println(<<"\\n The shortest path from node A to node F is: ", (lists:reverse(Path))/binary>>),
    ok.

breadth_first_search_path(Graph, Start, Target) ->
    Path = [],
    Queue = [],
    Visited = visited_init(Graph),
    Queue bsl Start,
    % TODO: unhandled stmt type
    Path1 = [<<"Path not found, problem in the Graph, start or end nodes! ">>],
    Path1.

departure(Queue) ->
    X = lists:nth(1, Queue),
    lists:delete(0, Queue),
    X.

visited_init(A_graph) ->
    Temp = #{},
    lists:foreach(fun(_) ->
        ok
    end, A_graph),
    Temp.

build_path_reverse(Graph, Start, Final, Visited) ->
    io:format("~s", [<<"\\n\\n Nodes visited (true) or no (false): ", (Visited)/binary>>]),
    Array_of_nodes = maps:keys(Graph),
    Current = Final,
    Path = [],
    Path bsl Current,
    % TODO: unhandled stmt type
    Path.
