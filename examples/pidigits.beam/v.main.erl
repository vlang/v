-module('v.main').
-export([main/0]).

main() ->
    unbuffer_stdout(),
    Digits_printed = 0,
    K = integer_from_int(1),
    N1 = integer_from_int(4),
    N2 = integer_from_int(3),
    D = integer_from_int(1),
    U = integer_from_int(0),
    V = integer_from_int(0),
    W = integer_from_int(0),
    % TODO: unhandled stmt type
    ok