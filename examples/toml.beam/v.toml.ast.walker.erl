-module('v.toml.ast.walker').
-export(['Inspector.visit'/2, inspect/3, walk/2, walk_and_modify/2]).
% TODO: [unhandled stmt str type: v.ast.InterfaceDecl ]
% TODO: [unhandled stmt str type: v.ast.InterfaceDecl ]
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]

'Inspector.visit'(I, Value) ->
    'Inspector.inspector_callback'(I, Value, maps:get(data, I)),
    ok.

inspect(Value, Data, Inspector_callback) ->
    walk(#{inspector_callback => Inspector_callback, data => Data, {vbeam, type} => 'Inspector'}, Value),
    ok.

walk(Visitor, Value) ->
    case Value is todo of
        true -> begin
            Value_map = todo,
            lists:foreach(fun(Val) ->
                walk(Visitor, &Val),
                ok
            end, Value_map),
        end;
        false -> ok
    end,
    case Value is todo of
        true -> begin
            Value_array = todo,
            lists:foreach(fun(Val) ->
                walk(Visitor, &Val),
                ok
            end, Value_array),
        end;
        false -> 'Visitor.visit'(Visitor, Value)
    end,
    ok.

walk_and_modify(Modifier, Value) ->
    case Value is todo of
        true -> begin
            Value_map = todo,
            lists:foreach(fun(Val) ->
                walk_and_modify(Modifier, &Val),
                ok
            end, Value_map),
        end;
        false -> ok
    end,
    case Value is todo of
        true -> begin
            Value_array = todo,
            lists:foreach(fun(Val) ->
                walk_and_modify(Modifier, &Val),
                ok
            end, Value_array),
        end;
        false -> 'Modifier.modify'(Modifier, Value)
    end,
    ok.
