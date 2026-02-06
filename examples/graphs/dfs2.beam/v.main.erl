-module('v.main').
-export([main/0, depth_first_search_path/3, 'Solution.find_pattern'/6, print_pattern/1, visited_init/1]).

main() ->
    Graph_01 = #{<<"A">> => [<<"B">>, <<"C">>], <<"B">> => [<<"A">>, <<"D">>, <<"E">>], <<"C">> => [<<"A">>, <<"F">>], <<"D">> => [<<"B">>], <<"E">> => [<<"F">>, <<"B">>, <<"F">>], <<"F">> => [<<"C">>, <<"E">>]},
    Graph_02 = #{<<"A">> => [<<"B">>, <<"C">>, <<"D">>], <<"B">> => [<<"E">>], <<"C">> => [<<"F">>], <<"D">> => [<<"E">>], <<"E">> => [<<"H">>], <<"F">> => [<<"H">>], <<"G">> => [<<"H">>], <<"H">> => [<<"E">>, <<"F">>, <<"G">>]},
    Path_01 = depth_first_search_path(Graph_01, <<"A">>, <<"F">>),
    io:format("~s~n", [<<"\\n Graph_01: all path pattern from node A to node F is:">>]),
    print_pattern(Path_01),
    Path_02 = depth_first_search_path(Graph_02, <<"A">>, <<"H">>),
    io:format("~s~n", [<<"\\n Graph_02: all path pattern from node A to node F is:">>]),
    print_pattern(Path_02),
    ok.

depth_first_search_path(Adj, Start, Target) ->
    Sol = #{{vbeam, type} => 'Solution'},
    Path = [],
    Visited = visited_init(Adj),
    'Solution.find_pattern'(Sol, Adj, Visited, Start, Target, Path),
    maps:get(pattern, Sol).

'Solution.find_pattern'(S, Adj, Visited, Node, Target, Path) ->
    Path bsl Node,
    case Node == Target of
        true -> begin
            io:format("~s", [<<"\\n Founded pattern: ", (Path)/binary>>]),
            maps:get(pattern, S) bsl [Path]
        end;
        false -> ok
    end,
    io:format("~s", [<<"\\n Exploring of node ", (Node)/binary, " (true/false): ", (maps:get(Node, Adj))/binary>>]),
    lists:foreach(fun(_) ->
        case not maps:get(lists:nth(I + 1, maps:get(Node, Adj)), Visited) of
            true -> begin
                Temp = Path,
                'Solution.find_pattern'(S, Adj, Visited, lists:nth(I + 1, maps:get(Node, Adj)), Target, Temp)
            end;
            false -> ok
        end,
        ok
    end, maps:get(Node, Adj)),
    io:format("~s", [<<"\\n Current: ", (Node)/binary, " (only not visited) \\n Visited: ", (Visited)/binary>>]),
    ok.

print_pattern(Pat) ->
    lists:foreach(fun(P) ->
        vbeam_io:println(P),
        ok.
        ok
    end, Pat),
        ok.

visited_init(Adj) ->
    Temp = #{},
    lists:foreach(fun(_) ->
        ok
    end, Adj),
    Temp.
