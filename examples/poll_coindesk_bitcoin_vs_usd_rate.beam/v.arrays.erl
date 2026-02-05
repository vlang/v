-module('v.arrays').
-export([uniq/1, uniq_only/1, uniq_only_repeated/1, uniq_all_repeated/1, distinct/1]).

uniq(A) ->
    Res = [],
    J = -1,
    case length(A) > 0 of
        true -> begin
            J1 = 0,
            Res << lists:nth(1, A)
        end;
        false -> ok
    end,
    lists:foreach(fun(E) ->
        case lists:nth(J1 + 1, A) == E of
            true -> ok;
            false -> ok
        end,
        J2 = Idx,
        Res << E,
        ok
    end, A),
    Res.

uniq_only(A) ->
    case length(A) == 0 of
        true -> [];
        false -> ok
    end,
    case length(A) == 1 of
        true -> 'unknown.clone'(A);
        false -> ok
    end,
    case length(A) == 2 of
        true -> begin
            case lists:nth(1, A) != lists:nth(2, A) of
                true -> 'unknown.clone'(A);
                false -> ok
            end,
            []
        end;
        false -> ok
    end,
    Res = [],
    case lists:nth(1, A) != lists:nth(2, A) of
        true -> Res << lists:nth(1, A);
        false -> ok
    end,
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
    case lists:nth(length(A) - 2 + 1, A) != lists:nth(length(A) - 1 + 1, A) of
        true -> Res << lists:nth(length(A) - 1 + 1, A);
        false -> ok
    end,
    Res.

uniq_only_repeated(A) ->
    case length(A) == 0 || length(A) == 1 of
        true -> [];
        false -> ok
    end,
    Res = [],
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
    case lists:nth(length(A) - 2 + 1, A) == lists:nth(length(A) - 1 + 1, A) of
        true -> Res << lists:nth(length(A) - 1 + 1, A);
        false -> ok
    end,
    Res.

uniq_all_repeated(A) ->
    case length(A) == 0 || length(A) == 1 of
        true -> [];
        false -> ok
    end,
    case length(A) == 2 of
        true -> case lists:nth(1, A) == lists:nth(2, A) of
            true -> 'unknown.clone'(A);
            false -> ok
        end;
        false -> ok
    end,
    Res = [],
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
    Res.

distinct(A) ->
    uniq('unknown.sorted'(A, A < B)).
