-module('v.toml.input').
-export(['Config.read_input'/1]).

'Config.read_input'(C) ->
    case maps:get(file_path, C) /= <<"">> andalso maps:get(text, C) /= <<"">> of
        true -> error(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" ", (maps:get(name, todo))/binary, " should contain only one of the fields `file_path` OR `text` filled out">>)/binary>>);
        false -> 
            case maps:get(file_path, C) == <<"">> andalso maps:get(text, C) == <<"">> of
                true -> <<"">>;
                false -> 
                    case maps:get(text, C) /= <<"">> of
                        true -> maps:get(text, C);
                        false -> begin
                            Text = read_file(maps:get(file_path, C)),
                            Text
                        end
                                        end
                                        end
                end.
