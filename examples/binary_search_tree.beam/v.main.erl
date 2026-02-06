-module('v.main').
-export(['Tree.size'/1, 'Tree.insert'/2, 'Tree.search'/2, 'Tree.min'/1, 'Tree.delete'/2, main/0]).

'Tree.size'(Tree) ->
    case Tree of
        todo -> 0;
        todo -> 1 + 'Tree.size'(maps:get(left, Tree)) + 'Tree.size'(maps:get(right, Tree))
    end.

'Tree.insert'(Tree, X) ->
    case Tree of
        todo -> #{value => X, left => Tree, right => Tree, {vbeam, type} => 'Node'};
        todo -> case X == maps:get(value, Tree) of
            true -> Tree;
            false -> case X < maps:get(value, Tree) of
                true -> #{left => 'Tree.insert'(maps:get(left, Tree), X), {vbeam, type} => 'Node'};
                false -> #{right => 'Tree.insert'(maps:get(right, Tree), X), {vbeam, type} => 'Node'}
            end
        end
    end.

'Tree.search'(Tree, X) ->
    case Tree of
        todo -> false;
        todo -> case X == maps:get(value, Tree) of
            true -> true;
            false -> case X < maps:get(value, Tree) of
                true -> 'Tree.search'(maps:get(left, Tree), X);
                false -> 'Tree.search'(maps:get(right, Tree), X)
            end
        end
    end.

'Tree.min'(Tree) ->
    case Tree of
        todo -> todo;
        todo -> case maps:get(value, Tree) < 'Tree.min'(maps:get(left, Tree)) of
            true -> maps:get(value, Tree);
            false -> 'Tree.min'(maps:get(left, Tree))
        end
    end.

'Tree.delete'(Tree, X) ->
    case Tree of
        todo -> Tree;
        todo -> case maps:get(left, Tree) !is todo andalso maps:get(right, Tree) !is todo of
            true -> case X < maps:get(value, Tree) of
                true -> #{left => 'Tree.delete'(maps:get(left, Tree), X), {vbeam, type} => 'Node'};
                false -> case X > maps:get(value, Tree) of
                    true -> #{right => 'Tree.delete'(maps:get(right, Tree), X), {vbeam, type} => 'Node'};
                    false -> #{value => 'Tree.min'(maps:get(right, Tree)), right => 'Tree.delete'(maps:get(right, Tree), 'Tree.min'(maps:get(right, Tree))), {vbeam, type} => 'Node'}
                end
            end;
            false -> case maps:get(left, Tree) !is todo of
                true -> case X == maps:get(value, Tree) of
                    true -> maps:get(left, Tree);
                    false -> #{left => 'Tree.delete'(maps:get(left, Tree), X), {vbeam, type} => 'Node'}
                end;
                false -> case X == maps:get(value, Tree) of
                    true -> maps:get(right, Tree);
                    false -> #{right => 'Tree.delete'(maps:get(right, Tree), X), {vbeam, type} => 'Node'}
                end
            end
        end
    end.

main() ->
    Tree = todo,
    Vals = [0.2, 0.0, 0.5, 0.3, 0.6, 0.8, 0.9, 1.0, 0.1, 0.4, 0.7],
    lists:foreach(fun(I) ->
        Tree1 = 'Tree[f64].insert'(Tree, I),
        ok
    end, Vals),
    vbeam_io:println(<<"[1] after insertion tree size is ", (integer_to_binary('Tree[f64].size'(Tree1)))/binary>>),
    Del_vals = [-0.3, 0.0, 0.3, 0.6, 1.0, 1.5],
    lists:foreach(fun(I) ->
        Tree2 = 'Tree[f64].delete'(Tree1, I),
        ok
    end, Del_vals),
    io:format("~s", [<<"[2] after deletion tree size is ", (integer_to_binary('Tree[f64].size'(Tree2)))/binary, ", ">>]),
    io:format("~s", [<<"and these elements were deleted: ">>]),
    lists:foreach(fun(I) ->
        case not 'Tree[f64].search'(Tree2, I) of
            true -> io:format("~s", [<<(float_to_binary(I))/binary, " ">>]);
            false -> ok
        end,
        ok
    end, Vals),
    io:format("~s~n", [<<"">>]),
    ok.
