-module('v.rand.config').
-export(['ShuffleConfigStruct.validate_for'/2]).

'ShuffleConfigStruct.validate_for'(Config, A) ->
    case maps:get(start, Config) < 0 || maps:get(start, Config) >= length(A) of
        true -> error(<<"argument 'config.start' must be in range [0, a.len)">>);
        false -> ok
    end,
    case maps:get(end, Config) < 0 || maps:get(end, Config) > length(A) of
        true -> error(<<"argument 'config.end' must be in range [0, a.len]">>);
        false -> ok
    end,
    case maps:get(end, Config) != 0 && maps:get(end, Config) <= maps:get(start, Config) of
        true -> error(<<"argument 'config.end' must be greater than 'config.start'">>);
        false -> ok
    end.
