-module('v.main').
-export([size/1, main/0]).
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]

size(Tree) ->
    case Tree of
        todo -> todo;
        todo -> 1 + size(maps:get(left, Tree)) + size(maps:get(right, Tree))
    end.

main() ->
    Node1 = #{value => 30, left => #{{vbeam, type} => 'Empty'}, right => #{{vbeam, type} => 'Empty'}, {vbeam, type} => 'Node'},
    Node2 = #{value => 20, left => #{{vbeam, type} => 'Empty'}, right => #{{vbeam, type} => 'Empty'}, {vbeam, type} => 'Node'},
    Tree = #{value => 10, left => Node1, right => Node2, {vbeam, type} => 'Node'},
    vbeam_io:println(<<"tree structure:\\n ", (Tree)/binary>>),
    vbeam_io:println(<<"tree size: ", (integer_to_binary(size(Tree)))/binary>>),
    ok.
