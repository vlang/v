-module('v.main').
-export([topog_sort_greedy/1, all_fathers/2, min_degree/1, in_degree/1, remove_node_from_graph/2, main/0]).

topog_sort_greedy(Graph) ->
    N_nodes = maps:size(Graph),
    Top_order = [],
    Count = 0,
    V_degree = in_degree(Graph),
    print(<<"V Degree ", (V_degree)/binary>>),
    Small_degree = min_degree(V_degree),
    New_graph = remove_node_from_graph(Small_degree, Graph),
    Top_order bsl Small_degree,
    todo,
    % TODO: unhandled stmt type
    ok    Top_order.

all_fathers(Node, A_map) ->
    Array_of_keys = 'map[string][]string.keys'(A_map),
    All_incident = [],
    lists:foreach(fun(I) ->
        case lists:member(Node, maps:get(I, A_map)) of
            true -> All_incident bsl I;
            false -> ok
        end,
        ok
    end, Array_of_keys),
    All_incident.

min_degree(A_map) ->
    Array_of_keys = 'map[string]int.keys'(A_map),
    Key_min = '[]string.first'(Array_of_keys),
    Val_min = maps:get(Key_min, A_map),
    lists:foreach(fun(I) ->
        case Val_min > maps:get(I, A_map) of
            true -> begin
                Val_min1 = maps:get(I, A_map),
                Key_min1 = I,
            end;
            false -> ok
        end,
        ok
    end, Array_of_keys),
    Key_min1.

in_degree(A_map) ->
    Array_of_keys = 'map[string][]string.keys'(A_map),
    Degree = #{},
    lists:foreach(fun(I) ->
        ok
    end, Array_of_keys),
    Degree.

remove_node_from_graph(Node, A_map) ->
    New_graph = 'map[string][]string.clone'(A_map),
    'map[string][]string.delete'(New_graph, Node),
    All_nodes = 'map[string][]string.keys'(New_graph),
    lists:foreach(fun(Key) ->
        I = '[]string.index'(maps:get(Key, New_graph), Node),
        case I >= 0 of
            true -> '[]string.delete'(maps:get(Key, New_graph), I);
            false -> ok
        end,
        ok
    end, All_nodes),
    New_graph.

main() ->
    Graph_01 = #{<<"A">> => [<<"C">>, <<"B">>], <<"B">> => [<<"D">>], <<"C">> => [<<"D">>], <<"D">> => []},
    Graph_02 = #{<<"A">> => [<<"B">>, <<"C">>, <<"D">>], <<"B">> => [<<"E">>], <<"C">> => [<<"F">>], <<"D">> => [<<"G">>], <<"E">> => [<<"H">>], <<"F">> => [<<"H">>], <<"G">> => [<<"H">>], <<"H">> => []},
    Graph_03 = #{<<"5">> => [<<"11">>], <<"7">> => [<<"11">>, <<"8">>], <<"3">> => [<<"8">>, <<"10">>], <<"11">> => [<<"2">>, <<"9">>, <<"10">>], <<"8">> => [<<"9">>], <<"2">> => [], <<"9">> => [], <<"10">> => []},
    vbeam_io:println(<<"\\nA Topological Sort of G1:  ", (topog_sort_greedy(Graph_01))/binary>>),
    vbeam_io:println(<<"\\nA Topological Sort of G2:  ", (topog_sort_greedy(Graph_02))/binary>>),
    vbeam_io:println(<<"\\nA Topological Sort of G3:  ", (topog_sort_greedy(Graph_03))/binary>>),
    ok.
