-module('v.main').
-export([push_pq/3, updating_priority/3, departure_priority/1, all_adjacents/2, print_solution/1, print_paths_dist/2, dijkstra/2, main/0]).

push_pq(Prior_queue, Data, Priority) ->
    Temp = [],
    Pq_len = length(Prior_queue),
    I = 0,
    % TODO: for i < pq_len && priority > prior_queue[i].priority {
    Temp << #{data => Data, priority => Priority, {vbeam, type} => 'NODE'},
    % TODO: for i < pq_len {
    Prior_queue = '[]T.clone'(Temp),

updating_priority(Prior_queue, Search_data, New_priority) ->
    I = 0,
    Pq_len = length(Prior_queue),
    % TODO: for i < pq_len {

departure_priority(Prior_queue) ->
    X = maps:get(data, lists:nth(1, Prior_queue)),
    '[]T.delete'(Prior_queue, 0),
    X.

all_adjacents(G, V) ->
    Temp = [],
    lists:foreach(fun(I) ->
        case lists:nth(I + 1, lists:nth(V + 1, G)) > 0 of
            true -> Temp << I;
            false -> ok
        end,
        ok
    end, lists:seq(0, (length(G)) - 1)),
    Temp.

print_solution(Dist) ->
    print(<<"Vertex \\tDistance from Source">>),
    lists:foreach(fun(Node) ->
        print(<<"\\n ", (integer_to_binary(Node))/binary, " ==> \\t ", (integer_to_binary(lists:nth(Node + 1, Dist)))/binary>>),
        ok.
        ok
    end, lists:seq(0, (length(Dist)) - 1)),

print_paths_dist(Path, Dist) ->
    print(<<"\\n Read the nodes from right to left (a path): \\n">>),
    I = lists:foldl(fun(Node, IAcc) ->
        print(<<"\\n ", (integer_to_binary(Node))/binary, " ">>),
        ok.
        IOut = Node,
        % TODO: for path[i] != -1 {
        print(<<"\\t PATH COST: ", (integer_to_binary(lists:nth(Node + 1, Dist)))/binary>>),
        ok.
        IOut
    end, I, lists:seq(1, (length(Path)) - 1)),

dijkstra(G, S) ->
    Pq_queue = [],
    push_pq(Pq_queue, S, 0),
    N = length(G),
    Dist = [],
    Path = [],
    % TODO: for pq_queue.len != 0 {
    print_solution(Dist),
    print_paths_dist(Path, Dist),
    ok.

main() ->
    Graph_01 = [[0, 4, 0, 0, 0, 0, 0, 8, 0], [4, 0, 8, 0, 0, 0, 0, 11, 0], [0, 8, 0, 7, 0, 4, 0, 0, 2], [0, 0, 7, 0, 9, 14, 0, 0, 0], [0, 0, 0, 9, 0, 10, 0, 0, 0], [0, 0, 4, 14, 10, 0, 2, 0, 0], [0, 0, 0, 0, 0, 2, 0, 1, 6], [8, 11, 0, 0, 0, 0, 1, 0, 7], [0, 0, 2, 0, 0, 0, 6, 7, 0]],
    Graph_02 = [[0, 2, 0, 6, 0], [2, 0, 3, 8, 5], [0, 3, 0, 0, 7], [6, 8, 0, 0, 9], [0, 5, 7, 9, 0]],
    Graph_03 = [[0, 10, 6, 5], [10, 0, 0, 15], [6, 0, 0, 4], [5, 15, 4, 0]],
    Graph = [],
    lists:foreach(fun(G_value) ->
        Graph1 = '[][]int.clone'(G_value),
        Start_node = 0,
        vbeam_io:println(<<"\\n\\n Graph ", (integer_to_binary(Index + 1))/binary, " using Dijkstra algorithm (source node: ", (integer_to_binary(Start_node))/binary, ")">>),
        dijkstra(Graph1, Start_node),
        ok
    end, [Graph_01, Graph_02, Graph_03]),
    io:format("~s~n", [<<"\\n BYE -- OK">>]),
    ok.
