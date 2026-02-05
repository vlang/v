-module('v.main').
-export([main/0]).
% TODO: const args = arguments();

main() ->
    case length(arguments()) != 2 of
        true -> begin
            io:format("~s~n", [<<"usage: fibonacci [rank]">>]),
        end;
        false -> ok
    end,
    Stop = 'string.int'(lists:nth(2, arguments())),
    case Stop > 92 of
        true -> begin
            io:format("~s~n", [<<"rank must be 92 or less">>]),
        end;
        false -> ok
    end,
    A = todo,
    B = todo,
    C = todo,
    vbeam_io:println(integer_to_binary(A + B + C)),
    {A1, B1, C1} = lists:foldl(fun(_, {AAcc, BAcc, CAcc}) ->
        AOut = BAcc,
        BOut = CAcc,
        COut = AAcc + BAcc,
        vbeam_io:println(integer_to_binary(C1)),
        ok.
        {AOut, BOut, COut}
    end, {A, B, C}, lists:seq(0, Stop - 1)),
