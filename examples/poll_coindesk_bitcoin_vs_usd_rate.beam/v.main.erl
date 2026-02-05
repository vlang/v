-module('v.main').
-export([main/0, show_result/3]).
% TODO: const url = 'https://api.coinbase.com/v2/prices/BTC-USD/spot';

main() ->
    use_stdout(),
    Old_rate = todo,
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]

show_result(I, Res, Delta) ->
    case Delta == 0 of
        true -> ok;
        false -> ok
    end,
    Sdelta = float_to_binary(Delta),
    Color = case Delta > 0 of
        true -> Term.green;
        false -> case Delta == 0 of
            true -> Term.white;
            false -> Term.red
        end
    end,
    Cdelta = colorize(Color, Sdelta),
    info(<<(Cdelta)/binary, ", ", (float_to_binary(maps:get(amount, Res)))/binary, " USD/BTC, cycle: ", (integer_to_binary(I))/binary>>),
    ok.
