-module('v.main').
-export([dfs_recursive/4, visited_init/1, main/0]).

dfs_recursive(U, Visited, Graph, Top_sorting) ->
    print(<<" Visiting: ", (U)/binary, " -> ">>),
    lists:foreach(fun(V) ->
        case maps:get(V, Visited) == false of
            true -> dfs_recursive(V, Visited, Graph, Top_sorting);
            false -> ok
        end,
        ok
    end, maps:get(U, Graph)),
    Top_sorting << U,
    ok.

visited_init(A_graph) ->
    Array_of_keys = 'map[string][]string.keys'(A_graph),
    Temp = #{},
    lists:foreach(fun(I) ->
        ok
    end, Array_of_keys),
    Temp.

main() ->
    Graph_01 = #{<<"A">> => [<<"C">>, <<"B">>], <<"B">> => [<<"D">>], <<"C">> => [<<"D">>], <<"D">> => []},
    Graph_02 = #{<<"A">> => [<<"B">>, <<"C">>, <<"D">>], <<"B">> => [<<"E">>], <<"C">> => [<<"F">>], <<"D">> => [<<"G">>], <<"E">> => [<<"H">>], <<"F">> => [<<"H">>], <<"G">> => [<<"H">>], <<"H">> => []},
    Graph_03 = #{<<"5">> => [<<"11">>], <<"7">> => [<<"11">>, <<"8">>], <<"3">> => [<<"8">>, <<"10">>], <<"11">> => [<<"2">>, <<"9">>, <<"10">>], <<"8">> => [<<"9">>], <<"2">> => [], <<"9">> => [], <<"10">> => []},
    Graph = #{},
    lists:foreach(fun(G_value) ->
        vbeam_io:println(<<"Topological sorting for the graph ", (integer_to_binary(Index))/binary, " using a DFS recursive">>),
        ok.
        Graph1 = 'map[string][]string.clone'(G_value),
        Visited = visited_init(Graph1),
        Top_sorting = [],
        lists:foreach(fun(I) ->
            case maps:get(I, Visited) != true of
                true -> dfs_recursive(I, Visited, Graph1, Top_sorting);
                false -> ok
            end.
            ok
        end, 'map[string][]string.keys'(Graph1)),
        print(<<"\\n A topological sorting of graph ", (integer_to_binary(Index))/binary, " : ">>),
        ok.
        vbeam_io:println('[]string.reverse'(Top_sorting)),
        ok.
        io:format("~s~n", [<<"">>]),
        ok.
        ok
    end, [Graph_01, Graph_02, Graph_03]),
