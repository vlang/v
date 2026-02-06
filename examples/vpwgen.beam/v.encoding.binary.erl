-module('v.encoding.binary').
-export([big_endian_u16/1, big_endian_u16_at/2, big_endian_u16_end/1, big_endian_put_u16/2, big_endian_put_u16_at/3, big_endian_put_u16_end/2, big_endian_get_u16/1, big_endian_u32/1, big_endian_u32_at/2, big_endian_u32_end/1, big_endian_put_u32/2, big_endian_put_u32_at/3, big_endian_put_u32_end/2, big_endian_get_u32/1, big_endian_u64/1, big_endian_u64_at/2, big_endian_u64_end/1, big_endian_put_u64/2, big_endian_put_u64_at/3, big_endian_put_u64_end/2, big_endian_get_u64/1, big_endian_u16_fixed/1, big_endian_put_u16_fixed/2, big_endian_u32_fixed/1, big_endian_put_u32_fixed/2, big_endian_u64_fixed/1, big_endian_put_u64_fixed/2, little_endian_u16/1, little_endian_u16_at/2, little_endian_u16_end/1, little_endian_put_u16/2, little_endian_put_u16_at/3, little_endian_put_u16_end/2, little_endian_get_u16/1, little_endian_u32/1, little_endian_u32_at/2, little_endian_u32_end/1, little_endian_put_u32/2, little_endian_put_u32_at/3, little_endian_put_u32_end/2, little_endian_get_u32/1, little_endian_u64/1, little_endian_u64_at/2, little_endian_u64_end/1, little_endian_put_u64/2, little_endian_put_u64_at/3, little_endian_put_u64_end/2, little_endian_f32_at/2, little_endian_get_u64/1, little_endian_u16_fixed/1, little_endian_put_u16_fixed/2, little_endian_u32_fixed/1, little_endian_put_u32_fixed/2, little_endian_u64_fixed/1, little_endian_put_u64_fixed/2, encode_binary/2, encode_struct/2, encode_primitive/2, encode_array/2, encode_string/2, encode_map/2, decode_binary/2, decode_struct/2, decode_primitive/2, decode_array/2, decode_string/1, decode_map/2, 'DecodeState.get_u64'/1, 'DecodeState.get_u32'/1, 'DecodeState.get_u16'/1, 'DecodeState.get_u8'/1, 'EncodeState.put_u64'/2, 'EncodeState.put_u32'/2, 'EncodeState.put_u16'/2, 'EncodeState.put_u8'/2]).

big_endian_u16(B) ->
    _ = lists:nth(2, B),
    % TODO: unhandled stmt type
    ok
big_endian_u16_at(B, O) ->
    _ = lists:nth(O + 1, B),
    _1 = lists:nth(O + 1 + 1, B),
    % TODO: unhandled stmt type
    ok
big_endian_u16_end(B) ->
    big_endian_u16_at(B, length(B) - 2).

big_endian_put_u16(B, V) ->
    _ = lists:nth(2, B),
    % TODO: unhandled stmt type
    ok
big_endian_put_u16_at(B, V, O) ->
    _ = lists:nth(O + 1, B),
    _1 = lists:nth(O + 1 + 1, B),
    % TODO: unhandled stmt type
    ok
big_endian_put_u16_end(B, V) ->
    big_endian_put_u16_at(B, V, length(B) - 2),
    ok.

big_endian_get_u16(V) ->
    % TODO: unhandled stmt type
    ok
big_endian_u32(B) ->
    _ = lists:nth(4, B),
    % TODO: unhandled stmt type
    ok
big_endian_u32_at(B, O) ->
    _ = lists:nth(O + 1, B),
    _1 = lists:nth(O + 3 + 1, B),
    % TODO: unhandled stmt type
    ok
big_endian_u32_end(B) ->
    big_endian_u32_at(B, length(B) - 4).

big_endian_put_u32(B, V) ->
    _ = lists:nth(4, B),
    % TODO: unhandled stmt type
    ok
big_endian_put_u32_at(B, V, O) ->
    _ = lists:nth(O + 1, B),
    _1 = lists:nth(O + 3 + 1, B),
    % TODO: unhandled stmt type
    ok
big_endian_put_u32_end(B, V) ->
    big_endian_put_u32_at(B, V, length(B) - 4),
    ok.

big_endian_get_u32(V) ->
    % TODO: unhandled stmt type
    ok
big_endian_u64(B) ->
    _ = lists:nth(8, B),
    % TODO: unhandled stmt type
    ok
big_endian_u64_at(B, O) ->
    _ = lists:nth(O + 1, B),
    _1 = lists:nth(O + 7 + 1, B),
    % TODO: unhandled stmt type
    ok
big_endian_u64_end(B) ->
    big_endian_u64_at(B, length(B) - 8).

big_endian_put_u64(B, V) ->
    _ = lists:nth(8, B),
    % TODO: unhandled stmt type
    ok
big_endian_put_u64_at(B, V, O) ->
    _ = lists:nth(O + 1, B),
    _1 = lists:nth(O + 7 + 1, B),
    % TODO: unhandled stmt type
    ok
big_endian_put_u64_end(B, V) ->
    big_endian_put_u64_at(B, V, length(B) - 8),
    ok.

big_endian_get_u64(V) ->
    % TODO: unhandled stmt type
    ok
big_endian_u16_fixed(B) ->
    % TODO: unhandled stmt type
    ok
big_endian_put_u16_fixed(B, V) ->
    % TODO: unhandled stmt type
    ok
big_endian_u32_fixed(B) ->
    % TODO: unhandled stmt type
    ok
big_endian_put_u32_fixed(B, V) ->
    % TODO: unhandled stmt type
    ok
big_endian_u64_fixed(B) ->
    % TODO: unhandled stmt type
    ok
big_endian_put_u64_fixed(B, V) ->
    % TODO: unhandled stmt type
    ok
little_endian_u16(B) ->
    _ = lists:nth(2, B),
    % TODO: unhandled stmt type
    ok
little_endian_u16_at(B, O) ->
    _ = lists:nth(O + 1, B),
    _1 = lists:nth(O + 1 + 1, B),
    % TODO: unhandled stmt type
    ok
little_endian_u16_end(B) ->
    little_endian_u16_at(B, length(B) - 2).

little_endian_put_u16(B, V) ->
    _ = lists:nth(2, B),
    % TODO: unhandled stmt type
    ok
little_endian_put_u16_at(B, V, O) ->
    _ = lists:nth(O + 1, B),
    _1 = lists:nth(O + 1 + 1, B),
    % TODO: unhandled stmt type
    ok
little_endian_put_u16_end(B, V) ->
    little_endian_put_u16_at(B, V, length(B) - 2),
    ok.

little_endian_get_u16(V) ->
    % TODO: unhandled stmt type
    ok
little_endian_u32(B) ->
    _ = lists:nth(4, B),
    % TODO: unhandled stmt type
    ok
little_endian_u32_at(B, O) ->
    _ = lists:nth(O + 1, B),
    _1 = lists:nth(O + 3 + 1, B),
    % TODO: unhandled stmt type
    ok
little_endian_u32_end(B) ->
    little_endian_u32_at(B, length(B) - 4).

little_endian_put_u32(B, V) ->
    _ = lists:nth(4, B),
    % TODO: unhandled stmt type
    ok
little_endian_put_u32_at(B, V, O) ->
    _ = lists:nth(O + 1, B),
    _1 = lists:nth(O + 3 + 1, B),
    % TODO: unhandled stmt type
    ok
little_endian_put_u32_end(B, V) ->
    little_endian_put_u32_at(B, V, length(B) - 4),
    ok.

little_endian_get_u32(V) ->
    % TODO: unhandled stmt type
    ok
little_endian_u64(B) ->
    _ = lists:nth(8, B),
    % TODO: unhandled stmt type
    ok
little_endian_u64_at(B, O) ->
    _ = lists:nth(O + 1, B),
    _1 = lists:nth(O + 7 + 1, B),
    % TODO: unhandled stmt type
    ok
little_endian_u64_end(B) ->
    little_endian_u64_at(B, length(B) - 8).

little_endian_put_u64(B, V) ->
    _ = lists:nth(8, B),
    % TODO: unhandled stmt type
    ok
little_endian_put_u64_at(B, V, O) ->
    _ = lists:nth(O + 1, B),
    _1 = lists:nth(O + 7 + 1, B),
    % TODO: unhandled stmt type
    ok
little_endian_put_u64_end(B, V) ->
    little_endian_put_u64_at(B, V, length(B) - 8),
    ok.

little_endian_f32_at(B, O) ->
    _ = lists:nth(O + 1, B),
    _1 = lists:nth(O + 3 + 1, B),
    % TODO: unhandled stmt type
    ok
little_endian_get_u64(V) ->
    % TODO: unhandled stmt type
    ok
little_endian_u16_fixed(B) ->
    % TODO: unhandled stmt type
    ok
little_endian_put_u16_fixed(B, V) ->
    % TODO: unhandled stmt type
    ok
little_endian_u32_fixed(B) ->
    % TODO: unhandled stmt type
    ok
little_endian_put_u32_fixed(B, V) ->
    % TODO: unhandled stmt type
    ok
little_endian_u64_fixed(B) ->
    % TODO: unhandled stmt type
    ok
little_endian_put_u64_fixed(B, V) ->
    % TODO: unhandled stmt type
    ok
encode_binary(Obj, Config) ->
    S = #{b => [], big_endian => maps:get(big_endian, Config), {vbeam, type} => 'EncodeState'},
    maps:get(b, S).

encode_struct(S, Obj) ->

encode_primitive(S, Value) ->

encode_array(S, Arr) ->
    'unknown.put_u64'(S, todo),

encode_string(S, Str) ->
    'EncodeState.put_u64'(S, todo),
    maps:get(b, S) bsl 'string.bytes'(Str),
    ok.

encode_map(S, M) ->
    'unknown.put_u64'(S, todo),
    lists:foreach(fun(V) ->
        ok
    end, M),

decode_binary(B, Config) ->
    S = #{b => B, big_endian => maps:get(big_endian, Config), {vbeam, type} => 'DecodeState'},

decode_struct(S, _) ->
    Obj = #{{vbeam, type} => 'T'},
    Obj.

decode_primitive(S, Value) ->
    error(<<"(): impossible error">>).

decode_array(S, _) ->
    Len = todo,
    case Len =< 0 orelse maps:get(offset, S) + Len > length(maps:get(b, S)) of
        true -> error(<<"(): invalid array length decode from stream">>);
        false -> begin
            Arr = [],
            Arr
        end
        end.

decode_string(S) ->
    Len = todo,
    case Len =< 0 orelse maps:get(offset, S) + Len > length(maps:get(b, S)) of
        true -> error(<<(todo)/binary, "(): invalid string length decode from stream">>);
        false -> begin
            Str = todo,
            Str
        end
        end.

decode_map(S, _) ->
    Len = todo,
    case Len =< 0 orelse maps:get(offset, S) + Len > length(maps:get(b, S)) of
        true -> error(<<"(): invalid map length decode from stream">>);
        false -> begin
            M = #{},
            K = lists:foldl(fun(_, KAcc) ->
                KOut = todo,
                KOut
            end, K, lists:seq(0, Len - 1)),
            M
        end
        end.

'DecodeState.get_u64'(S) ->
    case maps:get(offset, S) + 8 > length(maps:get(b, S)) of
        true -> error(<<(todo)/binary, "(): bytes length is not enough for u64">>);
        false -> begin
            % TODO: unhandled stmt type
            ok            case maps:get(big_endian, S) of
                true -> big_endian_u64_at(maps:get(b, S), maps:get(offset, S));
                false -> little_endian_u64_at(maps:get(b, S), maps:get(offset, S))
            end
        end
        end.

'DecodeState.get_u32'(S) ->
    case maps:get(offset, S) + 4 > length(maps:get(b, S)) of
        true -> error(<<(todo)/binary, "(): bytes length is not enough for u32">>);
        false -> begin
            % TODO: unhandled stmt type
            ok            case maps:get(big_endian, S) of
                true -> big_endian_u32_at(maps:get(b, S), maps:get(offset, S));
                false -> little_endian_u32_at(maps:get(b, S), maps:get(offset, S))
            end
        end
        end.

'DecodeState.get_u16'(S) ->
    case maps:get(offset, S) + 2 > length(maps:get(b, S)) of
        true -> error(<<(todo)/binary, "(): bytes length is not enough for u16">>);
        false -> begin
            % TODO: unhandled stmt type
            ok            case maps:get(big_endian, S) of
                true -> big_endian_u16_at(maps:get(b, S), maps:get(offset, S));
                false -> little_endian_u16_at(maps:get(b, S), maps:get(offset, S))
            end
        end
        end.

'DecodeState.get_u8'(S) ->
    case maps:get(offset, S) + 1 > length(maps:get(b, S)) of
        true -> error(<<(todo)/binary, "(): bytes length is not enough for u8">>);
        false -> begin
            % TODO: unhandled stmt type
            ok            lists:nth(maps:get(offset, S) + 1, maps:get(b, S))
        end
        end.

'EncodeState.put_u64'(S, Value) ->
    case maps:get(big_endian, S) of
        true -> big_endian_put_u64(maps:get(b8, S), Value);
        false -> little_endian_put_u64(maps:get(b8, S), Value)
    end,
    maps:get(b, S) bsl maps:get(b8, S),
    ok.

'EncodeState.put_u32'(S, Value) ->
    case maps:get(big_endian, S) of
        true -> big_endian_put_u32(maps:get(b4, S), Value);
        false -> little_endian_put_u32(maps:get(b4, S), Value)
    end,
    maps:get(b, S) bsl maps:get(b4, S),
    ok.

'EncodeState.put_u16'(S, Value) ->
    case maps:get(big_endian, S) of
        true -> big_endian_put_u16(maps:get(b2, S), Value);
        false -> little_endian_put_u16(maps:get(b2, S), Value)
    end,
    maps:get(b, S) bsl maps:get(b2, S),
    ok.

'EncodeState.put_u8'(S, Value) ->
    maps:get(b, S) bsl Value,
    ok.
