-module('v.toml.decoder').
-export(['Decoder.decode'/2, 'Decoder.modify'/2, 'Decoder.excerpt'/2, 'Decoder.decode_quoted'/2, 'Decoder.decode_number'/2, decode_quoted_escapes/1, decode_unicode_escape/1, 'Decoder.decode_date_time'/2]).

'Decoder.decode'(D, N) ->
    walk_and_modify(D, N),
    ok.

'Decoder.modify'(D, Value) ->
    case Value of
        todo -> begin
            V = (todo),
            'Decoder.decode_quoted'(D, V)
        end;
        todo -> begin
            V1 = (todo),
            'Decoder.decode_number'(D, V1)
        end;
        todo -> begin
            V2 = (todo),
            'Decoder.decode_date_time'(D, V2)
        end;
        _ -> ok
    end,
    ok.

'Decoder.excerpt'(D, Tp) ->
    'Scanner.excerpt'(maps:get(scanner, D), maps:get(pos, Tp), 10).

'Decoder.decode_quoted'(D, Q) ->
    decode_quoted_escapes(Q),
    ok.

'Decoder.decode_number'(D, N) ->
    case maps:get(text, N) == <<"-nan">> orelse maps:get(text, N) == <<"+nan">> of
        true -> ok;
        false -> ok
    end,
    ok.

decode_quoted_escapes(Q) ->
    Eat_whitespace = false,
    Decoded_s = <<"">>,
    Is_basic = maps:get(quote, Q) == todo,
    case not Is_basic of
        true -> ok;
        false -> begin
            S = new_simple_text(maps:get(text, Q)),
            % TODO: unhandled stmt type
            ok            ok
        end
        end.

decode_unicode_escape(Esc_unicode) ->
    Is_long_esc_type = 'string.starts_with'(Esc_unicode, <<"U">>),
    Sequence = lists:nth(todo + 1, Esc_unicode),
    Hex_digits_len = case Is_long_esc_type of
        true -> 8;
        false -> 4
    end,
    Sequence_len = Hex_digits_len,
    Sequence1 = lists:nth(todo + 1, Sequence),
    Unicode_point = Sequence1,
    case length(Unicode_point) < 8 of
        true -> ok;
        false -> ok
    end,
    I64_val = parse_int(Unicode_point, 16, 0),
    Rn = todo,
    Rn.

'Decoder.decode_date_time'(D, Dt) ->
    case 'string.contains'(maps:get(text, Dt), <<".">>) of
        true -> begin
            Yymmddhhmmss = 'string.all_before'(maps:get(text, Dt), <<".">>),
            Rest = 'string.all_after'(maps:get(text, Dt), <<".">>),
            Z = case 'string.contains'(Rest, <<"Z">>) of
                true -> <<"Z">>;
                false -> <<"">>
            end,
            Ms = Rest,
            Offset = <<"">>,
            case 'string.contains'(Rest, <<"+">>) of
                true -> begin
                    Offset1 = <<(<<"+">>)/binary, ('string.all_after'(Rest, <<"+">>))/binary>>,
                    Ms1 = 'string.all_before'(Rest, <<"+">>),
                end;
                false -> case 'string.contains'(Rest, <<"-">>) of
                    true -> begin
                        Offset2 = <<(<<"-">>)/binary, ('string.all_after'(Rest, <<"-">>))/binary>>,
                        Ms2 = 'string.all_before'(Rest, <<"-">>),
                    end;
                    false -> ok
                end
            end,
            case Z /= <<"">> of
                true -> ok;
                false -> ok
            end,
            case length(Ms2) > 1 of
                true -> ok;
                false -> ok
            end,
            Ms3 = <<(<<(Ms2)/binary, ('string.repeat'(<<"0">>, 4 - length(Ms2)))/binary>>)/binary, (Z)/binary>>,
        end;
        false -> ok
    end,
    ok.
