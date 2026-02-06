-module('v.toml.to').
-export([json/1, any_to_json/1, json_any/1]).

json(Doa) ->
    case Doa of
        todo -> any_to_json(ast_to_any(maps:get(table, maps:get(ast, Doa))));
        todo -> any_to_json(Doa)
    end.

any_to_json(A) ->
    case A of
        todo -> <<"null">>;
        todo -> 'Any.json_str'(todo);
        todo -> 'Any.json_str'(todo);
        todo -> 'Any.json_str'(todo);
        todo -> 'Any.json_str'(todo);
        todo -> 'Any.json_str'(todo);
        todo -> 'Any.json_str'(todo);
        todo -> 'Any.json_str'(todo);
        todo -> 'Any.json_str'(todo);
        todo -> 'Any.json_str'(todo);
        todo -> 'Any.json_str'(todo);
        todo -> begin
            Str = <<"{">>,
            lists:foreach(fun(Val) ->
                Json_key = todo,
                Str1 = <<" ", ('Any.json_str'(Json_key))/binary, ": ", (any_to_json(Val))/binary, ",">>,
                ok
            end, A),
            Str2 = 'string.trim_right'(Str1, <<",">>),
            Str3 = <<" }">>,
            Str3
        end;
        todo -> begin
            Str4 = <<"[">>,
            lists:foreach(fun(Val) ->
                Str5 = <<" ", (any_to_json(Val))/binary, ",">>,
                ok
            end, A),
            Str6 = 'string.trim_right'(Str5, <<",">>),
            Str7 = <<" ]">>,
            Str7
        end
    end.

json_any(A) ->
    case A of
        todo -> #{{vbeam, type} => 'Null'};
        todo -> todo;
        todo -> todo;
        todo -> todo;
        todo -> todo;
        todo -> todo;
        todo -> todo;
        todo -> todo;
        todo -> todo;
        todo -> todo;
        todo -> todo;
        todo -> begin
            Jmap = #{},
            lists:foreach(fun(Val) ->
                ok
            end, A),
            Jmap
        end;
        todo -> begin
            Jarr = [],
            lists:foreach(fun(Val) ->
                Jarr bsl json_any(Val),
                ok.
                ok
            end, A),
            Jarr
        end
    end.
