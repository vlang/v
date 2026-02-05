-module('v.main').
-export([main/0]).
% TODO: const digits_to_print = os.args[1].int();
% TODO: const zero = math.big.integer_from_int(0);
% TODO: const one = math.big.integer_from_int(1);
% TODO: const two = math.big.integer_from_int(2);
% TODO: const three = math.big.integer_from_int(3);
% TODO: const four = math.big.integer_from_int(4);
% TODO: const ten = math.big.integer_from_int(10);

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
    % TODO: for {
