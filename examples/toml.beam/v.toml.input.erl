-module('v.toml.input').
-export(['Config.read_input'/1]).

'Config.read_input'(C) ->
    case maps:get(file_path, C) != <<"">> && maps:get(text, C) != <<"">> of
        true -> error(todo + <<".">> + todo + <<" ", (maps:get(name, todo))/binary, " should contain only one of the fields `file_path` OR `text` filled out">>);
        false -> ok
    end,
    case maps:get(file_path, C) == <<"">> && maps:get(text, C) == <<"">> of
        true -> <<"">>;
        false -> ok
    end,
    case maps:get(text, C) != <<"">> of
        true -> maps:get(text, C);
        false -> ok
    end,
    Text = read_file(maps:get(file_path, C)),
    Text.
