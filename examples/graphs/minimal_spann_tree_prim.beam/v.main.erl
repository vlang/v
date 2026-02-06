-module('v.main').
-export([push_pq/3, updating_priority/3, departure_priority/1, all_adjacents/2, print_solution/2, prim_mst/2, main/0]).

push_pq(Prior_queue, Data, Priority) ->
    Temp = [],
    Pg_len = length(Prior_queue),
    I = 0,
    % TODO: unhandled stmt type
    ok    Temp bsl #{data => Data, priority => Priority, {vbeam, type} => 'NODE'},
    % TODO: unhandled stmt type
    ok    Prior_queue = '[]T.clone'(Temp),

updating_priority(Prior_queue, Search_data, New_priority) ->
    I = 0,
    Pg_len = length(Prior_queue),
    % TODO: unhandled stmt type
    ok
departure_priority(Prior_queue) ->
    X = maps:get(data, lists:nth(1, Prior_queue)),
    '[]T.delete'(Prior_queue, 0),
    X.

all_adjacents(G, V) ->
    Temp = [],
    lists:foreach(fun(I) ->
        case lists:nth(I + 1, lists:nth(V + 1, G)) > 0 of
            true -> Temp bsl I;
            false -> ok
        end,
        ok
    end, lists:seq(0, (length(G)) - 1)),
    Temp.

print_solution(Path, G) ->
    print(<<"   Edge \\tWeight\\n">>),
    Sum = 0,
    lists:foreach(fun(Node) ->
        case lists:nth(Node + 1, Path) == -1 of
            true -> print(<<"\\n ", (integer_to_binary(Node))/binary, " <== reference or start node">>);
            false -> begin
                print(<<"\\n ", (integer_to_binary(Node))/binary, " <--> ", (integer_to_binary(lists:nth(Node + 1, Path)))/binary, " \\t", (integer_to_binary(lists:nth(lists:nth(Node + 1, Path) + 1, lists:nth(Node + 1, G))))/binary>>),
                Sum1 = lists:nth(lists:nth(Node + 1, Path) + 1, lists:nth(Node + 1, G)),
            end
        end,
        ok
    end, lists:seq(0, (length(Path)) - 1)),
    print(<<"\\n Minimum Cost Spanning Tree: ", (integer_to_binary(Sum1))/binary, "\\n\\n">>),
    ok.

prim_mst(G, S) ->
    Pq_queue = [],
    push_pq(Pq_queue, S, 0),
    N = length(G),
    Dist = [],
    Path = [],
    % TODO: unhandled stmt type
    ok    print_solution(Path, G),
    ok.

main() ->
    Graph_01 = [[0, 4, 0, 0, 0, 0, 0, 8, 0], [4, 0, 8, 0, 0, 0, 0, 11, 0], [0, 8, 0, 7, 0, 4, 0, 0, 2], [0, 0, 7, 0, 9, 14, 0, 0, 0], [0, 0, 0, 9, 0, 10, 0, 0, 0], [0, 0, 4, 14, 10, 0, 2, 0, 0], [0, 0, 0, 0, 0, 2, 0, 1, 6], [8, 11, 0, 0, 0, 0, 1, 0, 7], [0, 0, 2, 0, 0, 0, 6, 7, 0]],
    Graph_02 = [[0, 2, 0, 6, 0], [2, 0, 3, 8, 5], [0, 3, 0, 0, 7], [6, 8, 0, 0, 9], [0, 5, 7, 9, 0]],
    Graph_03 = [[0, 10, 6, 5], [10, 0, 0, 15], [6, 0, 0, 4], [5, 15, 4, 0]],
    Graph = [],
    lists:foreach(fun(G_value) ->
        vbeam_io:println(<<"\\n Minimal Spanning Tree of graph ", (integer_to_binary(Index + 1))/binary, " using PRIM algorithm">>),
        Graph1 = '[][]int.clone'(G_value),
        Start_node = 0,
        prim_mst(Graph1, Start_node),
        ok
    end, [Graph_01, Graph_02, Graph_03]),
    io:format("~s~n", [<<"\\n BYE -- OK">>]),
    ok.
