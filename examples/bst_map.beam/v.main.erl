-module('v.main').
-export(['KeyVal.=='/2, 'KeyVal.<'/2, main/0]).

'KeyVal.=='(A, B) ->
    maps:get(key, A) == maps:get(key, B).

'KeyVal.<'(A, B) ->
    maps:get(key, A) < maps:get(key, B).

main() ->
    Bst = #{{vbeam, type} => 'KeyVal]'},
    'KeyVal].insert'(Bst, #{key => 1, val => 12, {vbeam, type} => 'KeyVal'}),
    vbeam_io:println('KeyVal].in_order_traversal'(Bst)),
    'KeyVal].insert'(Bst, #{key => 2, val => 34, {vbeam, type} => 'KeyVal'}),
    'KeyVal].insert'(Bst, #{key => -2, val => 203, {vbeam, type} => 'KeyVal'}),
    lists:foreach(fun(Elem) ->
        vbeam_io:println(integer_to_binary(maps:get(val, Elem))),
        ok.
        ok
    end, 'KeyVal].in_order_traversal'(Bst)),
        ok.
