-module('v.os.cmdline').
-export([options/2, option/3, options_before/2, options_after/2, only_non_options/1, only_options/1]).

options(Args, Param) ->
    Flags = [],
    lists:foreach(fun(V) ->
        case V == Param of
            true -> case I + 1 < length(Args) of
                true -> Flags bsl lists:nth(I + 1 + 1, Args);
                false -> ok
            end;
            false -> ok
        end,
        ok
    end, Args),
    Flags.

option(Args, Param, Def) ->
    Found = false,
    lists:foreach(fun(Arg) ->
        case Found of
            true -> Arg;
            false -> case Param == Arg of
                true -> ok;
                false -> ok
            end
        end,
        ok
    end, Args),
    Def.

options_before(Args, What) ->
    Args_before = [],
    lists:foreach(fun(A) ->
        case lists:member(A, What) of
            true -> ok;
            false -> ok
        end,
        Args_before bsl A,
        ok
    end, Args),
    Args_before.

options_after(Args, What) ->
    Found = false,
    Args_after = [],
    lists:foreach(fun(A) ->
        case lists:member(A, What) of
            true -> begin
                Found1 = true,
                % TODO: unhandled stmt type
                ok            end;
            false -> ok
        end,
        case Found1 of
            true -> Args_after bsl A;
            false -> ok
        end,
        ok
    end, Args),
    Args_after.

only_non_options(Args) ->
    '[]string.filter'(Args, not 'string.starts_with'(It, <<"-">>)).

only_options(Args) ->
    '[]string.filter'(Args, 'string.starts_with'(It, <<"-">>)).
