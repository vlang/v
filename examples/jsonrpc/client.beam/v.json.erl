-module('v.json').
-export([decode/1, decode_with_type/2, encode/1, encode_pretty/1, raw_decode/1, fast_raw_decode/1, decode_int/1, decode_i8/1, decode_i16/1, decode_i64/1, decode_u8/1, decode_u16/1, decode_u32/1, decode_u64/1, decode_f32/1, decode_f64/1, decode_string/1, decode_bool/1, 'Any.str'/1, 'Any.int'/1, 'Any.i64'/1, 'Any.f64'/1, 'Any.bool'/1, 'Any.as_array'/1, 'Any.as_map'/1, 'Null.str'/1]).
% TODO: [unhandled stmt str type: v.ast.TypeDecl ]
% TODO: const null = json.Null{....};

decode(S) ->
    error(<<"json.decode: not implemented at runtime (compiler builtin)">>).

decode_with_type(Typ, S) ->
    error(<<"json.decode: not implemented at runtime (compiler builtin)">>).

encode(Val) ->
    <<"">>.

encode_pretty(Val) ->
    <<"">>.

raw_decode(S) ->
    error(<<"json.raw_decode: not implemented at runtime">>).

fast_raw_decode(S) ->
    raw_decode(S).

decode_int(Root) ->
    case Root is todo of
        true -> Root;
        false -> ok
    end,
    case Root is todo of
        true -> todo;
        false -> ok
    end,
    case Root is todo of
        true -> todo;
        false -> ok
    end,
    case Root is todo of
        true -> 'string.int'(Root);
        false -> ok
    end,
    0.

decode_i8(Root) ->
    todo.

decode_i16(Root) ->
    todo.

decode_i64(Root) ->
    case Root is todo of
        true -> Root;
        false -> ok
    end,
    case Root is todo of
        true -> todo;
        false -> ok
    end,
    case Root is todo of
        true -> todo;
        false -> ok
    end,
    case Root is todo of
        true -> 'string.i64'(Root);
        false -> ok
    end,
    0.

decode_u8(Root) ->
    todo.

decode_u16(Root) ->
    todo.

decode_u32(Root) ->
    todo.

decode_u64(Root) ->
    case Root is todo of
        true -> todo;
        false -> ok
    end,
    case Root is todo of
        true -> todo;
        false -> ok
    end,
    case Root is todo of
        true -> todo;
        false -> ok
    end,
    0.

decode_f32(Root) ->
    case Root is todo of
        true -> todo;
        false -> ok
    end,
    case Root is todo of
        true -> todo;
        false -> ok
    end,
    case Root is todo of
        true -> todo;
        false -> ok
    end,
    case Root is todo of
        true -> 'string.f32'(Root);
        false -> ok
    end,
    0.0.

decode_f64(Root) ->
    case Root is todo of
        true -> Root;
        false -> ok
    end,
    case Root is todo of
        true -> todo;
        false -> ok
    end,
    case Root is todo of
        true -> todo;
        false -> ok
    end,
    case Root is todo of
        true -> 'string.f64'(Root);
        false -> ok
    end,
    0.0.

decode_string(Root) ->
    case Root is todo of
        true -> Root;
        false -> ok
    end,
    <<"">>.

decode_bool(Root) ->
    case Root is todo of
        true -> Root;
        false -> ok
    end,
    case Root is todo of
        true -> Root == <<"true">>;
        false -> ok
    end,
    false.

'Any.str'(A) ->
    case A of
        todo -> <<"null">>;
        todo -> case A of
            true -> <<"true">>;
            false -> <<"false">>
        end;
        todo -> begin
            Val = A,
            case Val == 0 of
                true -> <<"0">>;
                false -> ok
            end,
            S = <<"">>,
            Neg = false,
            case Val < 0 of
                true -> begin
                    Neg1 = true,
                    Val1 = -Val,
                end;
                false -> ok
            end,
            % TODO: for val > 0 {
            case Neg1 of
                true -> <<"-">> + S;
                false -> S
            end
        end;
        todo -> begin
            Val2 = A,
            case Val2 == 0 of
                true -> <<"0">>;
                false -> ok
            end,
            S1 = <<"">>,
            Neg2 = false,
            case Val2 < 0 of
                true -> begin
                    Neg3 = true,
                    Val3 = -Val2,
                end;
                false -> ok
            end,
            % TODO: for val > 0 {
            case Neg3 of
                true -> <<"-">> + S1;
                false -> S1
            end
        end;
        todo -> <<"<f64>">>;
        todo -> A;
        todo -> <<"[]">>;
        todo -> <<"{}">>
    end.

'Any.int'(A) ->
    decode_int(A).

'Any.i64'(A) ->
    decode_i64(A).

'Any.f64'(A) ->
    decode_f64(A).

'Any.bool'(A) ->
    decode_bool(A).

'Any.as_array'(A) ->
    case A is todo of
        true -> A;
        false -> ok
    end,
    [].

'Any.as_map'(A) ->
    case A is todo of
        true -> A;
        false -> ok
    end,
    #{}.

'Null.str'(N) ->
    <<"null">>.
