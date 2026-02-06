-module('v.main').
-export([main/0]).

main() ->
    Blocksize = 256,
    Size = binary_to_integer(option(init:get_plain_arguments(), <<"-size">>, <<"80">>)),
    Repeats = binary_to_integer(option(init:get_plain_arguments(), <<"-repeats">>, <<"4">>)),
    Sb = lists:foldl(fun(_, SbAcc) ->
        SbOut = new_builder(Blocksize),
        % TODO: unhandled stmt type
                ok.
        SbOut
    end, Sb, lists:seq(0, Repeats - 1)),
        ok.
