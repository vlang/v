-module('v.crypto.rand').
-export([int_u64/1, bytes_to_u64/1, int_big/1]).

int_u64(Max) ->
    Bitlen = len_64(Max),
    case Bitlen == 0 of
        true -> todo;
        false -> ok
    end,
    K = (Bitlen + 7) / 8,
    B = todo,
    case B == todo of
        true -> ok;
        false -> ok
    end,
    N = todo,
    % TODO: for {
    N.

bytes_to_u64(B) ->
    Ws = 8,
    Z = [],
    I = length(B),
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
    case I > 0 of
        true -> begin
            D = todo,
            % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
        end;
        false -> ok
    end,
    Z.

int_big(N) ->
    case maps:get(signum, N) < 1 of
        true -> error(<<"`n` cannot be 0 or negative.">>);
        false -> ok
    end,
    Max = N - integer_from_int(1),
    Len = 'Integer.bit_len'(Max),
    case Len == 0 of
        true -> Max;
        false -> ok
    end,
    K = (Len + 7) / 8,
    B = todo,
    case B == 0 of
        true -> ok;
        false -> ok
    end,
    Result = #{{vbeam, type} => 'Integer'},
    % TODO: for {
    Result.
