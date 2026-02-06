-module('v.x.json2').
-export(['Decoder.increment'/2, 'Decoder.skip_whitespace'/2, 'Decoder.check_json_format'/1, 'Decoder.check_string'/1, 'Decoder.check_number'/1, 'Decoder.check_boolean'/1, 'Decoder.check_null'/1, 'Decoder.check_array'/1, 'Decoder.check_object'/1, 'LinkedList.push'/2, 'LinkedList.last'/1, 'ValueInfo].str'/1, 'LinkedList.str'/1, 'LinkedList.free'/1, 'JsonDecodeError.msg'/1, 'Decoder.checker_error'/2, 'Decoder.decode_error'/2, decode/2, get_dynamic_from_element/1, 'Decoder.decode_value'/2, 'Decoder.decode_string'/2, 'Decoder.decode_array'/2, 'Decoder.decode_map'/2, create_value_from_optional/1, 'Decoder.decode_enum'/2, 'Decoder.decode_number'/2, 'Decoder.decode_number_from_string'/1, copy_type/1, 'Decoder.get_decoded_sumtype_workaround'/2, 'Decoder.check_element_type_valid'/3, get_array_element_type/1, 'Decoder.check_array_type_valid'/3, 'Decoder.get_array_type_workaround'/2, get_map_element_type/1, 'Decoder.check_map_type_valid'/3, 'Decoder.check_map_empty_valid'/2, 'Decoder.get_map_type_workaround'/2, 'Decoder.check_struct_type_valid'/3, 'Decoder.get_struct_type_workaround'/2, 'Decoder.init_sumtype_by_value_kind'/3, 'Decoder.decode_sumtype'/2, raw_decode/1, encode/2, 'Encoder.encode_value'/2, 'Encoder.encode_string'/2, 'Encoder.encode_boolean'/2, 'Encoder.encode_number'/2, 'Encoder.encode_null'/1, 'Encoder.encode_array'/2, 'Encoder.encode_map'/2, 'Encoder.encode_enum'/2, 'Encoder.encode_sumtype'/2, check_not_empty/1, 'Encoder.cached_field_infos'/1, 'Encoder.encode_struct'/2, 'Encoder.encode_struct_fields'/5, 'Encoder.encode_custom'/2, 'Encoder.encode_custom2'/2, 'Encoder.increment_level'/1, 'Encoder.decrement_level'/1, 'Encoder.add_indent'/1, encode_pretty/1, 'Any.str'/1, 'Any.str'/1, 'Any.str'/1, 'Any.json_str'/1, 'Any.prettify_json_str'/1, 'Any.i8'/1, 'Any.i16'/1, 'Any.int'/1, 'Any.i32'/1, 'Any.i64'/1, 'Any.u8'/1, 'Any.u16'/1, 'Any.u32'/1, 'Any.u64'/1, 'Any.f32'/1, 'Any.f64'/1, 'Any.bool'/1, 'Any.arr'/1, 'Any.as_array'/1, 'Any.as_map'/1, 'Any.as_map_of_strings'/1, 'Any.to_time'/1, map_from/1, flatten_array/1, 'Token.full_col'/1, 'Scanner.move'/1, 'Scanner.move_pos_with_newlines'/1, 'Scanner.move_pos'/3, 'Scanner.error'/2, 'Scanner.tokenize'/3, 'Scanner.text_scan'/1, 'Scanner.num_scan'/1, 'Scanner.invalid_token'/1, 'Scanner.scan'/1, 'Null.from_json_null'/1, 'Null.to_json'/1, 'TokenKind__static__from'/1, 'ValueKind__static__from'/1]).

'Decoder.increment'(Checker, Message) ->
    case maps:get(checker_idx, Checker) + 1 == length(maps:get(json, Checker)) of
        true -> begin
            case Message == <<"">> of
                true -> todo;
                false -> ok
            end,
            'Decoder.checker_error'(Checker, <<(<<"EOF: ">>)/binary, (Message)/binary>>)
        end;
        false -> ok
    end,
    todo,
    ok.

'Decoder.skip_whitespace'(Checker, Message) ->
    % TODO: unhandled stmt type
    ok    ok.

'Decoder.check_json_format'(Checker) ->
    'Decoder.skip_whitespace'(Checker, <<"empty json">>),
    Start_idx_position = maps:get(checker_idx, Checker),
    Actual_value_info_pointer = todo,
    case lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) of
        todo -> begin
            'ValueInfo].push'(maps:get(values_info, Checker), #{position => maps:get(checker_idx, Checker), value_kind => string, {vbeam, type} => 'ValueInfo'}),
            Actual_value_info_pointer1 = 'ValueInfo].last'(maps:get(values_info, Checker)),
            'Decoder.check_string'(Checker)
        end;
        todo; todo -> begin
            'ValueInfo].push'(maps:get(values_info, Checker), #{position => maps:get(checker_idx, Checker), value_kind => number, {vbeam, type} => 'ValueInfo'}),
            Actual_value_info_pointer2 = 'ValueInfo].last'(maps:get(values_info, Checker)),
            'Decoder.check_number'(Checker)
        end;
        todo; todo -> begin
            'ValueInfo].push'(maps:get(values_info, Checker), #{position => maps:get(checker_idx, Checker), value_kind => boolean, {vbeam, type} => 'ValueInfo'}),
            Actual_value_info_pointer3 = 'ValueInfo].last'(maps:get(values_info, Checker)),
            'Decoder.check_boolean'(Checker)
        end;
        todo -> begin
            'ValueInfo].push'(maps:get(values_info, Checker), #{position => maps:get(checker_idx, Checker), value_kind => null, {vbeam, type} => 'ValueInfo'}),
            Actual_value_info_pointer4 = 'ValueInfo].last'(maps:get(values_info, Checker)),
            'Decoder.check_null'(Checker)
        end;
        todo -> begin
            'ValueInfo].push'(maps:get(values_info, Checker), #{position => maps:get(checker_idx, Checker), value_kind => array, {vbeam, type} => 'ValueInfo'}),
            Actual_value_info_pointer5 = 'ValueInfo].last'(maps:get(values_info, Checker)),
            'Decoder.check_array'(Checker)
        end;
        todo -> begin
            'ValueInfo].push'(maps:get(values_info, Checker), #{position => maps:get(checker_idx, Checker), value_kind => object, {vbeam, type} => 'ValueInfo'}),
            Actual_value_info_pointer6 = 'ValueInfo].last'(maps:get(values_info, Checker)),
            'Decoder.check_object'(Checker)
        end;
        _ -> 'Decoder.checker_error'(Checker, <<"unknown value kind">>)
    end,
    'Decoder.increment'(Checker, <<"">>),
    'Decoder.skip_whitespace'(Checker, <<"">>),
    case (not lists:member(lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)), [todo, todo, todo, todo])) of
        true -> 'Decoder.checker_error'(Checker, <<"invalid value. Unexpected character after ", (maps:get(value_kind, Actual_value_info_pointer6))/binary, " end">>);
        false -> ok
    end,
    ok.

'Decoder.check_string'(Checker) ->
    'Decoder.increment'(Checker, <<"string not closed">>),
    % TODO: unhandled stmt type
    ok    ok.

'Decoder.check_number'(Checker) ->
    case lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) == todo of
        true -> 'Decoder.increment'(Checker, <<"expected digit">>);
        false -> ok
    end,
    case lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) == todo of
        true -> 'Decoder.increment'(Checker, <<"">>);
        false -> case lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) >= todo andalso lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) =< todo of
            true -> begin
                'Decoder.increment'(Checker, <<"">>),
                % TODO: unhandled stmt type
                ok            end;
            false -> 'Decoder.checker_error'(Checker, <<"expected digit got ", ('u8.ascii_str'(lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker))))/binary>>)
        end
    end,
    case lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) == todo of
        true -> begin
            'Decoder.increment'(Checker, <<"expected digit">>),
            case not (lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) >= todo andalso lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) =< todo) of
                true -> 'Decoder.checker_error'(Checker, <<"expected digit got ", ('u8.ascii_str'(lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker))))/binary>>);
                false -> ok
            end,
            % TODO: unhandled stmt type
            ok        end;
        false -> ok
    end,
    case lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) == todo orelse lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) == todo of
        true -> begin
            'Decoder.increment'(Checker, <<"expected digit">>),
            case lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) == todo orelse lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) == todo of
                true -> 'Decoder.increment'(Checker, <<"expected digit">>);
                false -> ok
            end,
            case not (lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) >= todo andalso lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) =< todo) of
                true -> 'Decoder.checker_error'(Checker, <<"expected digit got ", ('u8.ascii_str'(lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker))))/binary>>);
                false -> ok
            end,
            % TODO: unhandled stmt type
            ok        end;
        false -> ok
    end,
    todo,
    ok.

'Decoder.check_boolean'(Checker) ->
    case lists:nth(maps:get(checker_idx, Checker) + 1, maps:get(json, Checker)) of
        todo -> begin
            case length(maps:get(json, Checker)) - maps:get(checker_idx, Checker) =< 3 of
                true -> 'Decoder.checker_error'(Checker, <<"EOF error: expecting `true`">>);
                false -> ok
            end,
            Is_not_ok = todo,
            case Is_not_ok /= 0 of
                true -> 'Decoder.checker_error'(Checker, <<"invalid boolean value. Got `", (lists:nth(todo + 1, maps:get(json, Checker)))/binary, "` instead of `true`">>);
                false -> ok
            end,
        end;
        todo -> begin
            case length(maps:get(json, Checker)) - maps:get(checker_idx, Checker) =< 4 of
                true -> 'Decoder.checker_error'(Checker, <<"EOF error: expecting `false`">>);
                false -> ok
            end,
            Is_not_ok1 = todo,
            case Is_not_ok1 /= 0 of
                true -> 'Decoder.checker_error'(Checker, <<"invalid boolean value. Got `", (lists:nth(todo + 1, maps:get(json, Checker)))/binary, "` instead of `false`">>);
                false -> ok
            end,
        end;
        _ -> 'Decoder.checker_error'(Checker, <<"invalid boolean">>)
    end,
    ok.

'Decoder.check_null'(Checker) ->
    case length(maps:get(json, Checker)) - maps:get(checker_idx, Checker) =< 3 of
        true -> 'Decoder.checker_error'(Checker, <<"EOF error: expecting `null`">>);
        false -> begin
            Is_not_ok = todo,
            case Is_not_ok /= 0 of
                true -> 'Decoder.checker_error'(Checker, <<"invalid null value. Got `", (lists:nth(todo + 1, maps:get(json, Checker)))/binary, "` instead of `null`">>);
                false -> begin
                    ok
                end
                        end
        end
        end.

'Decoder.check_array'(Checker) ->
    'Decoder.increment'(Checker, <<"expected array end">>),
    'Decoder.skip_whitespace'(Checker, <<"expected array end">>),
    % TODO: unhandled stmt type
    ok    ok.

'Decoder.check_object'(Checker) ->
    'Decoder.increment'(Checker, <<"expected object end">>),
    'Decoder.skip_whitespace'(Checker, <<"expected object end">>),
    % TODO: unhandled stmt type
    ok    ok.

'LinkedList.push'(List, Value) ->
    New_node = #{value => Value, {vbeam, type} => 'Node'},
    case maps:get(head, List) == todo of
        true -> begin
        end;
        false -> begin
        end
    end,
    todo,
    ok.

'LinkedList.last'(List) ->
    maps:get(value, maps:get(tail, List)).

'ValueInfo].str'(List) ->
    Result_buffer = [],
    Current = maps:get(head, List),
    % TODO: unhandled stmt type
    ok    '[]u8.bytestr'(Result_buffer).

'LinkedList.str'(List) ->
    Sb = new_builder(128),
    % TODO: unhandled stmt type
    ok    Current = maps:get(head, List),
    % TODO: unhandled stmt type
    ok    'Builder.str'(Sb).

'LinkedList.free'(List) ->
    Current = maps:get(head, List),
    % TODO: unhandled stmt type
    ok
'JsonDecodeError.msg'(E) ->
    <<"\\n", (integer_to_binary(maps:get(line, E)))/binary, ":", (integer_to_binary(maps:get(character, E)))/binary, ": Invalid json: ", (maps:get(message, E))/binary, "\\n", (maps:get(context, E))/binary>>.

'Decoder.checker_error'(Checker, Message) ->
    Position = maps:get(checker_idx, Checker),
    Line_number = 0,
    Character_number = 0,
    Last_newline = 0,
    % TODO: unhandled stmt type
    ok    Cutoff = Character_number > 50,
    Context_start = case Cutoff of
        true -> Position - 50;
        false -> Last_newline
    end,
    Context_end = int_min(length(maps:get(json, Checker)), Position + 5),
    Context_end_newline = 'string.index_u8'(lists:nth(todo + 1, maps:get(json, Checker)), todo),
    case Context_end_newline /= -1 of
        true -> ok;
        false -> ok
    end,
    Context = <<"">>,
    case Cutoff of
        true -> ok;
        false -> ok
    end,
    Context1 = lists:nth(todo + 1, maps:get(json, Checker)),
    Context2 = <<"\\e[31m", ('u8.ascii_str'(lists:nth(Position + 1, maps:get(json, Checker))))/binary, "\\e[0m">>,
    Context3 = lists:nth(todo + 1, maps:get(json, Checker)),
    Context4 = <<"\\n">>,
    case Cutoff of
        true -> ok;
        false -> ok
    end,
    Context5 = <<"\\e[31m^\\e[0m">>,
    todo.

'Decoder.decode_error'(Decoder, Message) ->
    Error_info = #{{vbeam, type} => 'ValueInfo'},
    case maps:get(current_node, Decoder) /= todo of
        true -> ok;
        false -> ok
    end,
    Start = maps:get(position, Error_info),
    End = Start + int_min(maps:get(length, Error_info), 50),
    Line_number = 0,
    Character_number = 0,
    Last_newline = 0,
    % TODO: unhandled stmt type
    ok    Cutoff = Character_number > 50,
    Context_start = case Cutoff of
        true -> Start - 50;
        false -> Last_newline
    end,
    Context_end = int_min(length(maps:get(json, Decoder)), End + 5),
    Context_end_newline = 'string.index_u8'(lists:nth(todo + 1, maps:get(json, Decoder)), todo),
    case Context_end_newline /= -1 of
        true -> ok;
        false -> ok
    end,
    Context = <<"">>,
    case Cutoff of
        true -> ok;
        false -> ok
    end,
    Context1 = lists:nth(todo + 1, maps:get(json, Decoder)),
    Context2 = <<"\\e[31m", (lists:nth(todo + 1, maps:get(json, Decoder)))/binary, "\\e[0m">>,
    Context3 = lists:nth(todo + 1, maps:get(json, Decoder)),
    Context4 = <<"\\n">>,
    case Cutoff of
        true -> ok;
        false -> ok
    end,
    Context5 = <<"\\e[31m", ('string.repeat'(<<"~">>, maps:get(length, Error_info)))/binary, "\\e[0m">>,
    todo.

decode(Val, Params) ->
    case Val == <<"">> of
        true -> todo;
        false -> begin
            Decoder = #{json => Val, strict => maps:get(strict, Params), {vbeam, type} => 'Decoder'},
            'Decoder.check_json_format'(Decoder),
            Result = #{{vbeam, type} => 'T'},
            'Decoder.decode_value'(Decoder, Result),
            todo,
            Result
        end
        end.

get_dynamic_from_element(_t) ->
    [].

'Decoder.decode_value'(Decoder, Val) ->
    case maps:get(current_node, Decoder) /= todo of
        true -> ok;
        false -> ok
    end,
    ok.

'Decoder.decode_string'(Decoder, Val) ->
    String_info = maps:get(value, maps:get(current_node, Decoder)),
    case maps:get(value_kind, String_info) == string of
        true -> begin
            String_buffer = [],
            Buffer_index = 1,
            String_index = 1,
            % TODO: unhandled stmt type
            ok            todo,
            Val = '[]u8.bytestr'(String_buffer),
        end;
        false -> 'Decoder.decode_error'(Decoder, <<"Expected string, but got ", (maps:get(value_kind, String_info))/binary>>)
    end,
    ok.

'Decoder.decode_array'(Decoder, Val) ->
    Array_info = maps:get(value, maps:get(current_node, Decoder)),
    case maps:get(value_kind, Array_info) == array of
        true -> begin
            Array_position = maps:get(position, Array_info),
            Array_end = Array_position + maps:get(length, Array_info),
            % TODO: unhandled stmt type
            ok        end;
        false -> 'Decoder.decode_error'(Decoder, <<"Expected array, but got ", (maps:get(value_kind, Array_info))/binary>>)
    end,
    ok.

'Decoder.decode_map'(Decoder, Val) ->
    Map_info = maps:get(value, maps:get(current_node, Decoder)),
    case maps:get(value_kind, Map_info) == object of
        true -> begin
            Map_position = maps:get(position, Map_info),
            Map_end = Map_position + maps:get(length, Map_info),
            % TODO: unhandled stmt type
            ok        end;
        false -> 'Decoder.decode_error'(Decoder, <<"Expected object, but got ", (maps:get(value_kind, Map_info))/binary>>)
    end,
    ok.

create_value_from_optional(_val) ->
    #{{vbeam, type} => 'T'}.

'Decoder.decode_enum'(Decoder, Val) ->
    Enum_info = maps:get(value, maps:get(current_node, Decoder)),
    case maps:get(value_kind, Enum_info) == number of
        true -> begin
            Result = 0,
            todo,
            'unknown.decode_error'(Decoder, <<"Number value: `", "` does not match any field in enum: ">>)
        end;
        false -> case maps:get(value_kind, Enum_info) == string of
            true -> begin
                Result1 = <<"">>,
                'unknown.decode_string'(Decoder, Result1),
                'unknown.decode_error'(Decoder, <<"String value: `", "` does not match any field in enum: ">>)
            end;
            false -> ok
        end
    end,
    'unknown.decode_error'(Decoder, <<"Expected number or string value for enum, got: ">>),
    ok.

'Decoder.decode_number'(Decoder, Val) ->
    Number_info = maps:get(value, maps:get(current_node, Decoder)),
    Str = lists:nth(todo + 1, maps:get(json, Decoder)),
    case maps:get(unaliased_typ, T) of
        todo -> ok;
        todo -> ok;
        todo -> ok;
        todo -> ok;
        todo -> ok;
        todo -> ok;
        todo -> ok;
        todo -> ok;
        todo -> ok;
        todo -> ok;
        todo -> ok;
        todo -> ok;
        todo -> ok;
        _ -> error(<<"`decode_number` can not decode ", " type">>)
    end,
    ok.

'Decoder.decode_number_from_string'(Decoder) ->
    String_info = maps:get(value, maps:get(current_node, Decoder)),
    case maps:get(length, String_info) < 2 of
        true -> error(<<"invalid string for number conversion">>);
        false -> begin
            Str = lists:nth(todo + 1, maps:get(json, Decoder)),
        end
        end.

copy_type(_t) ->
    #{{vbeam, type} => 'T'}.

'Decoder.get_decoded_sumtype_workaround'(Decoder, Initialized_sumtype) ->
    'Decoder.decode_error'(Decoder, <<"could not decode resolved sumtype (should not happen)">>),
    Initialized_sumtype.

'Decoder.check_element_type_valid'(Decoder, Element, Current_node) ->
    case Current_node == todo of
        true -> true;
        false -> 
            case Element is todo of
                true -> true;
                false -> begin
                    case maps:get(value_kind, maps:get(value, Current_node)) of
                        string -> ;
                        number -> ;
                        boolean -> ;
                        null -> ;
                        array -> ;
                        object -> 
                    end,
                    false
                end
                        end
                end.

get_array_element_type(_arr) ->
    #{{vbeam, type} => 'T'}.

'Decoder.check_array_type_valid'(Decoder, Arr, Current_node) ->
    'Decoder.check_element_type_valid'(Decoder, get_array_element_type(Arr), Current_node).

'Decoder.get_array_type_workaround'(Decoder, Initialized_sumtype) ->
    false.

get_map_element_type(_m) ->
    #{{vbeam, type} => 'V'}.

'Decoder.check_map_type_valid'(Decoder, M, Current_node) ->
    Element = get_map_element_type(M),
    'Decoder.check_element_type_valid'(Decoder, Element, Current_node).

'Decoder.check_map_empty_valid'(Decoder, M) ->
    Element = get_map_element_type(M),
    'unknown.check_element_type_valid'(Decoder, Element, Current_node).

'Decoder.get_map_type_workaround'(Decoder, Initialized_sumtype) ->
    false.

'Decoder.check_struct_type_valid'(Decoder, S, Current_node) ->
    Type_field_node = maps:get(next, maps:get(current_node, Decoder)),
    Map_position = maps:get(position, maps:get(value, Current_node)),
    Map_end = Map_position + maps:get(length, maps:get(value, Current_node)),
    Type_field = <<"\"_type\"">>,
    % TODO: unhandled stmt type
    ok    case Type_field_node == todo of
        true -> false;
        false -> begin
            Variant_name = maps:get(name, todo),
            case maps:get(length, maps:get(value, Type_field_node)) - 2 == length(Variant_name) of
                true -> begin
                    % TODO: unhandled stmt type
                    ok                    case todo of
                        true -> true;
                        false -> ok
                    end
                end;
                false -> ok
            end,
            false
        end
        end.

'Decoder.get_struct_type_workaround'(Decoder, Initialized_sumtype) ->
    false.

'Decoder.init_sumtype_by_value_kind'(Decoder, Val, Value_info) ->
    Failed_struct = false,
    case maps:get(value_kind, Value_info) of
        string -> ok;
        number -> ok;
        boolean -> ok;
        null -> ok;
        array -> ok;
        object -> ok
    end,
    case Failed_struct of
        true -> 'Decoder.decode_error'(Decoder, <<"could not resolve sumtype `", (maps:get(name, T))/binary, "`, missing \"_type\" field?">>);
        false -> ok
    end,
    'Decoder.decode_error'(Decoder, <<"could not resolve sumtype `", (maps:get(name, T))/binary, "`, got ", (maps:get(value_kind, Value_info))/binary, ".">>),
    ok.

'Decoder.decode_sumtype'(Decoder, Val) ->
    ok.

raw_decode(Src) ->
    decode(Src, #{{vbeam, type} => 'DecoderOptions'}).

encode(Val, Config) ->
    Encoder = #{EncoderOptions => Config, {vbeam, type} => 'Encoder'},
    'Encoder.encode_value'(Encoder, Val),
    '[]u8.bytestr'(maps:get(output, Encoder)).

'Encoder.encode_value'(Encoder, Val) ->

'Encoder.encode_string'(Encoder, Val) ->
    maps:get(output, Encoder) bsl todo,
    Buffer_start = 0,
    Buffer_end = 0,
    % TODO: unhandled stmt type
    ok    todo,
    maps:get(output, Encoder) bsl todo,
    ok.

'Encoder.encode_boolean'(Encoder, Val) ->
    case Val of
        true -> todo;
        false -> todo
    end.

'Encoder.encode_number'(Encoder, Val) ->
    Integer_val = 'T.str'(Val),
    todo,
    ok.

'Encoder.encode_null'(Encoder) ->
    todo,
    ok.

'Encoder.encode_array'(Encoder, Val) ->
    maps:get(output, Encoder) bsl todo,
    case maps:get(prettify, Encoder) of
        true -> begin
            'Encoder.increment_level'(Encoder),
            'Encoder.add_indent'(Encoder)
        end;
        false -> ok
    end,
    lists:foreach(fun(Item) ->
        'Encoder.encode_value'(Encoder, Item),
        case I < length(Val) - 1 of
            true -> begin
                maps:get(output, Encoder) bsl todo,
                case maps:get(prettify, Encoder) of
                    true -> 'Encoder.add_indent'(Encoder);
                    false -> ok
                end
            end;
            false -> case maps:get(prettify, Encoder) of
                true -> begin
                    'Encoder.decrement_level'(Encoder),
                    'Encoder.add_indent'(Encoder)
                end;
                false -> ok
            end
        end,
        ok
    end, Val),
    maps:get(output, Encoder) bsl todo,
    ok.

'Encoder.encode_map'(Encoder, Val) ->
    maps:get(output, Encoder) bsl todo,
    case maps:get(prettify, Encoder) of
        true -> begin
            'Encoder.increment_level'(Encoder),
            'Encoder.add_indent'(Encoder)
        end;
        false -> ok
    end,
    I = 0,
    lists:foreach(fun(Value) ->
        'Encoder.encode_string'(Encoder, Key),
        maps:get(output, Encoder) bsl todo,
        case maps:get(prettify, Encoder) of
            true -> maps:get(output, Encoder) bsl todo;
            false -> ok
        end,
        'Encoder.encode_value'(Encoder, Value),
        case I < maps:size(Val) - 1 of
            true -> begin
                maps:get(output, Encoder) bsl todo,
                case maps:get(prettify, Encoder) of
                    true -> 'Encoder.add_indent'(Encoder);
                    false -> ok
                end
            end;
            false -> case maps:get(prettify, Encoder) of
                true -> begin
                    'Encoder.decrement_level'(Encoder),
                    'Encoder.add_indent'(Encoder)
                end;
                false -> ok
            end
        end,
        todo,
        ok
    end, Val),
    maps:get(output, Encoder) bsl todo,
    ok.

'Encoder.encode_enum'(Encoder, Val) ->
    case maps:get(enum_as_int, Encoder) of
        true -> 'unknown.encode_number'(Encoder, todo);
        false -> begin
            Enum_val = 'unknown.str'(Val),
            ,
            Attr_value = <<"">>,
            case Attr_value /= <<"">> of
                true -> ok;
                false -> ok
            end,
            maps:get(output, Encoder) bsl todo,
            todo,
            maps:get(output, Encoder) bsl todo
        end
    end.

'Encoder.encode_sumtype'(Encoder, Val) ->

check_not_empty(Val) ->
    true.

'Encoder.cached_field_infos'(Encoder) ->
    Field_infos = todo,
    case Field_infos == todo of
        true -> begin
            Field_infos1 = [],
        end;
        false -> ok
    end,
    *Field_infos1.

'Encoder.encode_struct'(Encoder, Val) ->
    maps:get(output, Encoder) bsl todo,
    Is_first = 'Encoder.encode_struct_fields'(Encoder, Val, true, [], <<"">>),
    case maps:get(prettify, Encoder) andalso not Is_first of
        true -> begin
            'Encoder.decrement_level'(Encoder),
            'Encoder.add_indent'(Encoder)
        end;
        false -> ok
    end,
    maps:get(output, Encoder) bsl todo,
    ok.

'Encoder.encode_struct_fields'(Encoder, Val, Was_first, Old_used_keys, Prefix) ->
    Field_infos = 'Encoder.cached_field_infos'(Encoder),
    I = 0,
    Is_first = Was_first,
    Used_keys = Old_used_keys,
    Is_first.

'Encoder.encode_custom'(Encoder, Val) ->
    Integer_val = 'T.to_json'(Val),
    todo,
    ok.

'Encoder.encode_custom2'(Encoder, Val) ->
    Integer_val = 'unknown.json_str'(Val),
    todo,
    ok.

'Encoder.increment_level'(Encoder) ->
    todo,

'Encoder.decrement_level'(Encoder) ->
    todo,

'Encoder.add_indent'(Encoder) ->
    todo,
    ok.

encode_pretty(Typed_data) ->
    encode(Typed_data, #{prettify => true, {vbeam, type} => 'void'}).

'Any.str'(F) ->
    'Any.json_str'(todo).

'Any.str'(F) ->
    'Any.json_str'(todo).

'Any.str'(F) ->
    case F is todo of
        true -> F;
        false -> 'Any.json_str'(F)
    end.

'Any.json_str'(F) ->
    encode(F, #{{vbeam, type} => 'EncoderOptions'}).

'Any.prettify_json_str'(F) ->
    encode(F, #{prettify => true, {vbeam, type} => 'EncoderOptions'}).

'Any.i8'(F) ->
    case F of
        todo -> F;
        todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> todo;
        todo -> 'string.i8'(F);
        _ -> 0
    end.

'Any.i16'(F) ->
    case F of
        todo -> F;
        todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> todo;
        todo -> 'string.i16'(F);
        _ -> 0
    end.

'Any.int'(F) ->
    case F of
        todo -> F;
        todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> todo;
        todo -> 'string.int'(F);
        _ -> 0
    end.

'Any.i32'(F) ->
    case F of
        todo -> F;
        todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> todo;
        todo -> 'string.i32'(F);
        _ -> 0
    end.

'Any.i64'(F) ->
    case F of
        todo -> F;
        todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> todo;
        todo -> 'string.i64'(F);
        _ -> 0
    end.

'Any.u8'(F) ->
    case F of
        todo -> F;
        todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> todo;
        todo -> 'string.u8'(F);
        _ -> 0
    end.

'Any.u16'(F) ->
    case F of
        todo -> F;
        todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> todo;
        todo -> 'string.u16'(F);
        _ -> 0
    end.

'Any.u32'(F) ->
    case F of
        todo -> F;
        todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> todo;
        todo -> 'string.u32'(F);
        _ -> 0
    end.

'Any.u64'(F) ->
    case F of
        todo -> F;
        todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> todo;
        todo -> 'string.u64'(F);
        _ -> 0
    end.

'Any.f32'(F) ->
    case F of
        todo -> F;
        todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> todo;
        todo -> 'string.f32'(F);
        _ -> 0.0
    end.

'Any.f64'(F) ->
    case F of
        todo -> F;
        todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> todo;
        todo -> 'string.f64'(F);
        _ -> 0.0
    end.

'Any.bool'(F) ->
    case F of
        todo -> F;
        todo -> begin
            case F == <<"false">> of
                true -> false;
                false -> ok
            end,
            case F == <<"true">> of
                true -> true;
                false -> ok
            end,
            case length(F) > 0 of
                true -> F /= <<"0">> andalso F /= <<"0.0">>;
                false -> false
            end
        end;
        todo; todo; todo; todo; todo -> todo /= 0;
        todo; todo; todo; todo -> todo /= 0;
        todo; todo -> todo /= todo;
        _ -> false
    end.

'Any.arr'(F) ->
    'Any.as_array'(F).

'Any.as_array'(F) ->
    case F is todo of
        true -> F;
        false -> case F is todo of
            true -> begin
                Arr = [],
                lists:foreach(fun(V) ->
                    Arr bsl V,
                    ok
                end, F),
                Arr
            end;
            false -> ok
        end
    end,
    [F].

'Any.as_map'(F) ->
    case F is todo of
        true -> F;
        false -> case F is todo of
            true -> begin
                Mp = #{},
                lists:foreach(fun(Fi) ->
                    ok
                end, F),
                Mp
            end;
            false -> ok
        end
    end,
    #{<<"0">> => F}.

'Any.as_map_of_strings'(F) ->
    case F is todo of
        true -> Ms;
        false -> 
            case F is todo of
                true -> Ms;
                false -> #{<<"0">> => 'Any.str'(F)}
                        end
                end.

'Any.to_time'(F) ->
    case F of
        todo -> F;
        todo -> unix(F);
        todo -> begin
            Is_iso8601 = lists:nth(5, F) == todo andalso lists:nth(8, F) == todo,
            case Is_iso8601 of
                true -> parse_iso8601(F);
                false -> ok
            end,
            Is_rfc3339 = length(F) == 24 andalso lists:nth(24, F) == todo andalso lists:nth(11, F) == todo,
            case Is_rfc3339 of
                true -> parse_rfc3339(F);
                false -> ok
            end,
            Is_unix_timestamp = true,
            lists:foreach(fun(C) ->
                case C == todo orelse (C >= todo andalso C =< todo) of
                    true -> ok;
                    false -> ok
                end.
                Is_unix_timestamp1 = false,
                % TODO: unhandled stmt type
                ok                ok
            end, F),
            case Is_unix_timestamp1 of
                true -> unix('string.i64'(F));
                false -> ok
            end,
            parse(F)
        end;
        _ -> error(<<"not a time value: ", (F)/binary, " of type: ", ('Any.type_name'(F))/binary>>)
    end.

map_from(T) ->
    M = #{},
    M.

flatten_array(T) ->

'Token.full_col'(T) ->
    maps:get(col, T) + length(maps:get(lit, T)).

'Scanner.move'(S) ->
    'Scanner.move_pos'(S, true, true),
    ok.

'Scanner.move_pos_with_newlines'(S) ->
    'Scanner.move_pos'(S, false, true),
    ok.

'Scanner.move_pos'(S, Include_space, Include_newlines) ->
    todo,
    case maps:get(pos, S) < length(maps:get(text, S)) of
        true -> case Include_newlines andalso lists:member(lists:nth(maps:get(pos, S) + 1, maps:get(text, S)), [todo, todo, todo]) of
            true -> begin
                todo,
                case lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) == todo andalso maps:get(pos, S) + 1 < length(maps:get(text, S)) andalso lists:nth(maps:get(pos, S) + 1 + 1, maps:get(text, S)) == todo of
                    true -> todo;
                    false -> ok
                end,
                % TODO: unhandled stmt type
                ok            end;
            false -> case Include_space andalso lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) == todo of
                true -> begin
                    todo,
                    todo,
                    % TODO: unhandled stmt type
                    ok                end;
                false -> ok
            end
        end;
        false -> todo
    end.

'Scanner.error'(S, Description) ->
    'Scanner.tokenize'(S, 'string.bytes'(Description), error).

'Scanner.tokenize'(S, Lit, Kind) ->
    #{lit => Lit, kind => Kind, col => maps:get(col, S), line => maps:get(line, S), {vbeam, type} => 'Token'}.

'Scanner.text_scan'(S) ->
    Has_closed = false,
    Chrs = [],
    % TODO: unhandled stmt type
    ok    Tok = 'Scanner.tokenize'(S, Chrs, str),
    'Scanner.move'(S),
    case not Has_closed of
        true -> 'Scanner.error'(S, <<"missing double quotes in string closing">>);
        false -> Tok
        end.

'Scanner.num_scan'(S) ->
    Is_fl = false,
    Dot_index = -1,
    Digits = [],
    case lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) == todo of
        true -> begin
            Digits bsl todo,
            case not 'u8.is_digit'(lists:nth(maps:get(pos, S) + 1 + 1, maps:get(text, S))) of
                true -> 'Scanner.invalid_token'(S);
                false -> ok
            end,
            'Scanner.move_pos_with_newlines'(S)
        end;
        false -> ok
    end,
    case lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) == todo andalso (maps:get(pos, S) + 1 < length(maps:get(text, S)) andalso 'u8.is_digit'(lists:nth(maps:get(pos, S) + 1 + 1, maps:get(text, S)))) of
        true -> 'Scanner.error'(S, <<"leading zeroes in a number are not allowed">>);
        false -> begin
            % TODO: unhandled stmt type
            ok            case Dot_index + 1 < length(maps:get(text, S)) andalso length(lists:nth(todo + 1, Digits)) == 0 of
                true -> 'Scanner.error'(S, <<"invalid float">>);
                false -> begin
                    case maps:get(pos, S) < length(maps:get(text, S)) andalso (lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) == todo orelse lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) == todo) of
                        true -> begin
                            Digits bsl lists:nth(maps:get(pos, S) + 1, maps:get(text, S)),
                            'Scanner.move_pos_with_newlines'(S),
                            case maps:get(pos, S) < length(maps:get(text, S)) andalso lists:member(lists:nth(maps:get(pos, S) + 1, maps:get(text, S)), [todo, todo]) of
                                true -> begin
                                    Digits bsl lists:nth(maps:get(pos, S) + 1, maps:get(text, S)),
                                    'Scanner.move_pos_with_newlines'(S)
                                end;
                                false -> ok
                            end,
                            Exp_digits_count = 0,
                            % TODO: unhandled stmt type
                            ok                            case Exp_digits_count == 0 of
                                true -> 'Scanner.error'(S, <<"invalid exponent">>);
                                false -> ok
                            end
                        end;
                        false -> ok
                    end,
                    Kind = case Is_fl of
                        true -> float;
                        false -> int
                    end,
                    'Scanner.tokenize'(S, Digits, Kind)
                end
                        end
        end
        end.

'Scanner.invalid_token'(S) ->
    case lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) >= 32 andalso lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) =< 126 of
        true -> begin
            X = 'u8.ascii_str'(lists:nth(maps:get(pos, S) + 1, maps:get(text, S))),
            'Scanner.error'(S, <<"invalid token `", (X)/binary, "`">>)
        end;
        false -> begin
            X1 = 'u8.str_escaped'(lists:nth(maps:get(pos, S) + 1, maps:get(text, S))),
            'Scanner.error'(S, <<"invalid token `", (X1)/binary, "`">>)
        end
    end.

'Scanner.scan'(S) ->
    case maps:get(pos, S) < length(maps:get(text, S)) andalso (lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) == todo orelse lists:member(lists:nth(maps:get(pos, S) + 1, maps:get(text, S)), [todo, todo, todo])) of
        true -> 'Scanner.move'(S);
        false -> ok
    end,
    case maps:get(pos, S) >= length(maps:get(text, S)) of
        true -> 'Scanner.tokenize'(S, [], eof);
        false -> case maps:get(pos, S) + 3 < length(maps:get(text, S)) andalso (lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) == todo orelse lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) == todo) of
            true -> begin
                Ident = '[]u8.bytestr'(lists:nth(todo + 1, maps:get(text, S))),
                case Ident == <<"true">> orelse Ident == <<"null">> of
                    true -> begin
                        Kind = null,
                        case Ident == <<"true">> of
                            true -> ok;
                            false -> ok
                        end,
                        todo,
                        Val = lists:nth(todo + 1, maps:get(text, S)),
                        Tok = 'Scanner.tokenize'(S, Val, Kind),
                        'Scanner.move'(S),
                        'Scanner.move'(S),
                        'Scanner.move'(S),
                        'Scanner.move'(S),
                        Tok
                    end;
                    false -> ok
                end,
                todo,
                'Scanner.invalid_token'(S)
            end;
            false -> case maps:get(pos, S) + 4 < length(maps:get(text, S)) andalso lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) == todo of
                true -> begin
                    Ident1 = '[]u8.bytestr'(lists:nth(todo + 1, maps:get(text, S))),
                    case Ident1 == <<"false">> of
                        true -> begin
                            todo,
                            Val1 = lists:nth(todo + 1, maps:get(text, S)),
                            Tok1 = 'Scanner.tokenize'(S, Val1, bool),
                            'Scanner.move'(S),
                            'Scanner.move'(S),
                            'Scanner.move'(S),
                            'Scanner.move'(S),
                            'Scanner.move'(S),
                            Tok1
                        end;
                        false -> ok
                    end,
                    todo,
                    'Scanner.invalid_token'(S)
                end;
                false -> case lists:member(lists:nth(maps:get(pos, S) + 1, maps:get(text, S)), [todo, todo, todo, todo, todo, todo]) of
                    true -> begin
                        Chr = lists:nth(maps:get(pos, S) + 1, maps:get(text, S)),
                        Tok2 = 'Scanner.tokenize'(S, [], todo),
                        'Scanner.move'(S),
                        Tok2
                    end;
                    false -> case lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) == todo of
                        true -> 'Scanner.text_scan'(S);
                        false -> case 'u8.is_digit'(lists:nth(maps:get(pos, S) + 1, maps:get(text, S))) orelse lists:nth(maps:get(pos, S) + 1, maps:get(text, S)) == todo of
                            true -> 'Scanner.num_scan'(S);
                            false -> 'Scanner.invalid_token'(S)
                        end
                    end
                end
            end
        end
    end.

'Null.from_json_null'(N) ->
    ok.

'Null.to_json'(N) ->
    <<"null">>.

'TokenKind__static__from'(Input) ->
    error(<<"invalid value">>).

'ValueKind__static__from'(Input) ->
    error(<<"invalid value">>).
