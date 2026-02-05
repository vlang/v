-module('v.main').
-export([main/0, check_error/1]).

main() ->
    from(<<"asd">>),
    from(<<"">>),
    ok.

check_error(Err) ->
    case Err of
        todo -> io:format("~s~n", [<<"wrong format">>]);
        todo -> io:format("~s~n", [<<"empty input">>]);
        _ -> io:format("~s~n", [<<"unknown error">>])
    end.
