-module('v.main').
-export([main/0, depth_first_search_path/3, visited_init/1, build_path_reverse/4]).

main() ->
    Graph_01 = #{<<"A">> => [<<"B">>, <<"C">>], <<"B">> => [<<"A">>, <<"D">>, <<"E">>], <<"C">> => [<<"A">>, <<"F">>], <<"D">> => [<<"B">>], <<"E">> => [<<"F">>, <<"B">>, <<"F">>], <<"F">> => [<<"C">>, <<"E">>]},
    Graph_02 = #{<<"A">> => [<<"B">>, <<"C">>, <<"D">>], <<"B">> => [<<"E">>], <<"C">> => [<<"F">>], <<"D">> => [<<"E">>], <<"E">> => [<<"H">>], <<"F">> => [<<"H">>], <<"G">> => [<<"H">>], <<"H">> => [<<"E">>, <<"F">>, <<"G">>]},
    Path_01 = depth_first_search_path(Graph_01, <<"A">>, <<"F">>),
    vbeam_io:println(<<"\\n Graph_01: a first path from node A to node F is: ", (lists:reverse(Path_01))/binary>>),
    Path_02 = depth_first_search_path(Graph_02, <<"A">>, <<"H">>),
    vbeam_io:println(<<"\\n Graph_02: a first path from node A to node H is: ", (lists:reverse(Path_02))/binary>>),
    ok.

depth_first_search_path(Graph, Start, Target) ->
    Path = [],
    Stack = [],
    Visited = visited_init(Graph),
    Stack bsl Start,
    % TODO: unhandled stmt type
    Path1 = [<<"Path not found, problem in the Graph, start or end nodes! ">>],
    Path1.

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
