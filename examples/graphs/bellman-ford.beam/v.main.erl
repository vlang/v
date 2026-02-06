-module('v.main').
-export([build_map_edges_from_graph/1, print_sol/1, bellman_ford/2, main/0]).

build_map_edges_from_graph(G) ->
    N = length(G),
    Edges_map = #{},
    Edge = 0,
    lists:foreach(fun(I) ->
        lists:foreach(fun(J) ->
            case lists:nth(J + 1, lists:nth(I + 1, G)) /= 0 of
                true -> begin
                    todo
                end;
                false -> ok
            end,
            ok
        end, lists:seq(0, N - 1)),
        ok
    end, lists:seq(0, N - 1)),
    Edges_map.

print_sol(Dist) ->
    N_vertex = length(Dist),
    print(<<"\\n Vertex   Distance from Source">>),
    lists:foreach(fun(I) ->
        print(<<"\\n   ", (integer_to_binary(I))/binary, "   -->   ", (integer_to_binary(lists:nth(I + 1, Dist)))/binary>>),
        ok.
        ok
    end, lists:seq(0, N_vertex - 1)),

bellman_ford(Graph, Src) ->
    Edges = build_map_edges_from_graph(Graph),
    N_edges = maps:size(Edges),
    N_vertex = length(Graph),
    Dist = [],
    lists:foreach(fun(_) ->
        {U, V, Weight} = lists:foldl(fun(J, {UAcc, VAcc, WeightAcc}) ->
            UOut = maps:get(src, maps:get(J, Edges)),
            VOut = maps:get(dest, maps:get(J, Edges)),
            WeightOut = maps:get(weight, maps:get(J, Edges)),
            case lists:nth(U + 1, Dist) /= 999999 andalso lists:nth(U + 1, Dist) + Weight < lists:nth(V + 1, Dist) of
                true -> ok;
                false -> ok
            end,
            {UOut, VOut, WeightOut}
        end, {U, V, Weight}, lists:seq(0, N_edges - 1)),
        ok
    end, lists:seq(0, N_vertex - 1)),
    {U1, V1, Weight1} = lists:foldl(fun(J, {UAcc, VAcc, WeightAcc}) ->
        UOut = maps:get(src, maps:get(J, Edges)),
        VOut = maps:get(dest, maps:get(J, Edges)),
        WeightOut = maps:get(weight, maps:get(J, Edges)),
        case lists:nth(U1 + 1, Dist) /= 999999 andalso lists:nth(U1 + 1, Dist) + Weight1 < lists:nth(V1 + 1, Dist) of
            true -> begin
                print(<<"\\n Graph contains negative weight cycle">>),
            end;
            false -> ok
        end,
        {UOut, VOut, WeightOut}
    end, {U, V, Weight}, lists:seq(0, N_vertex - 1)),
    print_sol(Dist),
    ok.

main() ->
    Graph_01 = [[0, -1, 4, 0, 0], [0, 0, 3, 2, 2], [0, 0, 0, 0, 0], [0, 1, 5, 0, 0], [0, 0, 0, -3, 0]],
    Graph_02 = [[0, 2, 0, 6, 0], [2, 0, 3, 8, 5], [0, 3, 0, 0, 7], [6, 8, 0, 0, 9], [0, 5, 7, 9, 0]],
    Graph_03 = [[0, 10, 6, 5], [10, 0, 0, 15], [6, 0, 0, 4], [5, 15, 4, 0]],
    Graph = [],
    lists:foreach(fun(G_value) ->
        Graph1 = '[][]int.clone'(G_value),
        Start_node = 0,
        vbeam_io:println(<<"\\n\\n Graph ", (integer_to_binary(Index + 1))/binary, " using Bellman-Ford algorithm (source node: ", (integer_to_binary(Start_node))/binary, ")">>),
        bellman_ford(Graph1, Start_node),
        ok
    end, [Graph_01, Graph_02, Graph_03]),
    io:format("~s~n", [<<"\\n BYE -- OK">>]),
    ok.
