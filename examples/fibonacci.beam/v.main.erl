-module('v.main').
-export([main/0]).

main() ->
    case length(init:get_plain_arguments()) /= 2 of
        true -> ok;
        false -> begin
            Stop = binary_to_integer(lists:nth(2, init:get_plain_arguments())),
            case Stop > 92 of
                true -> ok;
                false -> begin
                    A = todo,
                    B = todo,
                    C = todo,
                    vbeam_io:println(integer_to_binary(A + B + C)),
                    {A1, B1, C1} = lists:foldl(fun(_, {AAcc, BAcc, CAcc}) ->
                        AOut = BAcc,
                        BOut = CAcc,
                        COut = AAcc + BAcc,
                        vbeam_io:println(integer_to_binary(C1)),
                        ok
                        {AOut, BOut, COut}
                    end, {A, B, C}, lists:seq(0, Stop - 1)),
                                        ok
                end
                        end
        end
        end.
