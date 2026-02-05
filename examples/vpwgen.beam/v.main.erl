-module('v.main').
-export([main/0]).

main() ->
    Blocksize = 256,
    Size = 'string.int'(option(arguments(), <<"-size">>, <<"80">>)),
    Repeats = 'string.int'(option(arguments(), <<"-repeats">>, <<"4">>)),
    Sb = lists:foldl(fun(_, SbAcc) ->
        SbOut = new_builder(Blocksize),
        % TODO: for {
        SbOut
    end, Sb, lists:seq(0, Repeats - 1)),
