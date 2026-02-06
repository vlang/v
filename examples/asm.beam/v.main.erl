-module('v.main').
-export([main/0]).

main() ->
    A = 100,
    B = 20,
    C = 0,
    % TODO: unhandled stmt type
    ok    vbeam_io:println(<<"a: ", (integer_to_binary(A))/binary>>),
    vbeam_io:println(<<"b: ", (integer_to_binary(B))/binary>>),
    vbeam_io:println(<<"c: ", (integer_to_binary(C))/binary>>),
    ok.
