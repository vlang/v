-module('v.main').
-export([main/0, print_generation/1, next_generation/1]).

main() ->
    Arg = <<"31">>,
    case length(init:get_plain_arguments()) /= 2 of
        true -> begin
            io:format("~s~n", [<<"Usage: rule110 [<n>]">>]),
            io:format("~s~n", [<<"Using default `n` value: 31">>])
        end;
        false -> ok
    end,
    N = binary_to_integer(Arg),
    case N > 200 orelse N < 3 of
        true -> begin
            io:format(standard_error, "~s~n", [<<"`n` must be between 3 and 200!">>]),
            exit(1)
        end;
        false -> ok
    end,
    io:format("~s", [<<"\\n">>]),
    Title = <<" Rule 110 V Implementation ">>,
    Title_len = length(Title),
    case N > Title_len of
        true -> begin
            lists:foreach(fun(_) ->
                io:format("~s", [<<"=">>]),
                ok
            end, lists:seq(0, (N - Title_len) div 2 - 1)),
            io:format("~s", [Title]),
            lists:foreach(fun(_) ->
                io:format("~s", [<<"=">>]),
                ok
            end, lists:seq(0, (N - Title_len) div 2 - 1)),
        end;
        false -> vbeam_io:println(lists:nth(todo + 1, Title))
    end,
    Generation_bin = [],
    lists:foreach(fun(I) ->
        ok
    end, lists:seq(0, N - 1)),
    io:format("~s", [<<"\\n">>]),
    lists:foreach(fun(_) ->
        print_generation(Generation_bin),
        ok.
        next_generation(Generation_bin),
        ok.
        ok
    end, lists:seq(0, N - 1)),
        ok.

print_generation(Arr) ->
    Symbols = [<<" ">>, <<"*">>],
    lists:foreach(fun(I) ->
        io:format("~s", [lists:nth(lists:nth(I + 1, Arr) + 1, Symbols)]),
        ok
    end, lists:seq(0, length(Arr) - 1)),
    io:format("~s", [<<"\\n">>]),
    ok.

next_generation(Gen) ->
    Arr = Gen,
    Prev = 0,
    Curr = 0,
    Next = 0,
    {Curr1, Next1} = lists:foldl(fun(I, {CurrAcc, NextAcc}) ->
        case (I - 1) rem length(Gen) < 0 of
            true -> ok;
            false -> ok
        end,
        CurrOut = lists:nth(I + 1, Gen),
        NextOut = lists:nth((I + 1) rem length(Gen) + 1, Gen),
        case Prev == 1 of
            true -> case Curr1 == 1 of
                true -> case Next1 == 1 of
                    true -> ok;
                    false -> ok
                end;
                false -> case Next1 == 1 of
                    true -> ok;
                    false -> ok
                end
            end;
            false -> case Curr1 == 1 of
                true -> case Next1 == 1 of
                    true -> ok;
                    false -> ok
                end;
                false -> case Next1 == 1 of
                    true -> ok;
                    false -> ok
                end
            end
        end,
        {CurrOut, NextOut}
    end, {Curr, Next}, lists:seq(0, length(Arr) - 1)),
    Gen = Arr,
