-module('v.toml.checker').
-export([toml_parse_time/1, 'Checker.check'/2, 'Checker.visit'/2, 'Checker.excerpt'/2, is_hex_bin_oct_prefixed/1, has_repeating/2, 'Checker.check_number'/2, 'Checker.is_valid_binary_literal'/2, 'Checker.is_valid_octal_literal'/2, 'Checker.is_valid_hex_literal'/2, 'Checker.check_boolean'/2, 'Checker.check_date_time'/2, 'Checker.check_date'/2, 'Checker.check_time'/2, 'Checker.check_quoted'/2, 'Checker.check_quoted_escapes'/2, 'Checker.check_utf8_validity'/2, validate_utf8_codepoint_string/1, 'Checker.check_unicode_escape'/2, 'Checker.check_comment'/2]).
% TODO: const allowed_basic_escape_chars = [`u`, `U`, `b`, `t`, `n`, `f`, `r`, `"`, `\\`];
% TODO: const utf8_max = 0x10FFFF;

toml_parse_time(S) ->
    case length(S) > 3 && lists:nth(3, S) == todo of
        true -> parse_rfc3339(<<"0001-01-01T">> + S);
        false -> ok
    end,
    case length(S) == 10 of
        true -> parse_rfc3339(S + <<"T00:00:00Z">>);
        false -> ok
    end,
    parse_rfc3339(S).

'Checker.check'(C, N) ->
    walk(C, N),
    ok.

'Checker.visit'(C, Value) ->
    case Value of
        todo -> 'Checker.check_boolean'(C, Value);
        todo -> 'Checker.check_number'(C, Value);
        todo -> 'Checker.check_quoted'(C, Value);
        todo -> 'Checker.check_date_time'(C, Value);
        todo -> 'Checker.check_date'(C, Value);
        todo -> 'Checker.check_time'(C, Value);
        _ -> ok
    end,
    ok.

'Checker.excerpt'(C, Tp) ->
    'Scanner.excerpt'(maps:get(scanner, C), maps:get(pos, Tp), 10).

is_hex_bin_oct_prefixed(Hbo) ->
    length(Hbo) > 2 && ('string.starts_with'(Hbo, <<"0x">>) || 'string.starts_with'(Hbo, <<"0o">>) || 'string.starts_with'(Hbo, <<"0b">>)).

has_repeating(Str, Repeats) ->
    lists:foreach(fun(R) ->
        case R in Repeats && I + 1 < length(Str) of
            true -> case R == lists:nth(I + 1 + 1, Str) of
                true -> true;
                false -> ok
            end;
            false -> ok
        end,
        ok
    end, Str),
    false.

'Checker.check_number'(C, Num) ->
    Lit = maps:get(text, Num),
    Lit_lower_case = 'string.to_lower'(Lit),
    case Lit in [<<"0">>, <<"0.0">>, <<"+0">>, <<"-0">>, <<"+0.0">>, <<"-0.0">>, <<"0e0">>, <<"+0e0">>, <<"-0e0">>, <<"0e00">>] of
        true -> ok;
        false -> ok
    end,
    case 'string.contains'(Lit, <<"_">>) of
        true -> begin
            case 'string.starts_with'(Lit, <<"_">>) || 'string.ends_with'(Lit, <<"_">>) of
                true -> error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" can not start or end with `_` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                false -> ok
            end,
            case 'string.contains'(Lit, <<"__">>) of
                true -> error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" can not have more than one underscore (`_`) in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                false -> ok
            end
        end;
        false -> ok
    end,
    Hex_bin_oct = is_hex_bin_oct_prefixed(Lit),
    Is_bin = false,
    Is_oct = false,
    Is_hex = false,
    Is_float = 'string.contains'('string.all_before'(Lit_lower_case, <<"e">>), <<".">>),
    Has_exponent_notation = 'string.contains'(Lit_lower_case, <<"e">>),
    Float_decimal_index = 'string.index_'(Lit, <<".">>),
    Ascii = 'u8.ascii_str'(todo),
    Is_sign_prefixed = lists:nth(1, Lit) in [todo, todo],
    Lit_sans_sign = Lit,
    case Is_sign_prefixed of
        true -> begin
            Lit_sans_sign1 = lists:nth(todo + 1, Lit),
            Hex_bin_oct1 = is_hex_bin_oct_prefixed(Lit_sans_sign1),
            case Hex_bin_oct1 of
                true -> begin
                    Ascii1 = 'u8.ascii_str'(todo),
                    error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" (hex, octal and binary) can not start with `", (Ascii1)/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)
                end;
                false -> ok
            end,
            case length(Lit) > 1 && 'string.starts_with'(Lit_sans_sign1, <<"0">>) && !'string.starts_with'(Lit_sans_sign1, <<"0.">>) of
                true -> begin
                    Ascii2 = 'u8.ascii_str'(todo),
                    error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" can not start with `", (Ascii2)/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)
                end;
                false -> ok
            end
        end;
        false -> case !Hex_bin_oct1 of
            true -> begin
                case !Is_float && lists:nth(1, Lit) == todo of
                    true -> begin
                        case lists:nth(2, Lit) in [todo, todo, todo] of
                            true -> error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" only lowercase notation in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                            false -> ok
                        end,
                        error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" can not start with a zero in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)
                    end;
                    false -> ok
                end,
                case Is_float && lists:nth(1, Lit) == todo && Float_decimal_index > 1 of
                    true -> error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" can not start with a zero in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                    false -> ok
                end
            end;
            false -> ok
        end
    end,
    case has_repeating(Lit, [todo, todo, todo, todo, todo]) of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" can not have ", ([todo, todo, todo, todo, todo, todo, todo])/binary, " as repeating characters in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
        false -> ok
    end,
    case Hex_bin_oct1 of
        true -> begin
            Is_bin1 = 'string.starts_with'(Lit_sans_sign1, <<"0b">>),
            Is_oct1 = 'string.starts_with'(Lit_sans_sign1, <<"0o">>),
            Is_hex1 = 'string.starts_with'(Lit_sans_sign1, <<"0x">>),
            Lit_sans_sign_and_type_prefix = lists:nth(todo + 1, Lit_sans_sign1),
            case 'string.starts_with'(Lit_sans_sign_and_type_prefix, <<"_">>) || 'string.ends_with'(Lit_sans_sign_and_type_prefix, <<"_">>) of
                true -> error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" can not start or end with `_` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                false -> ok
            end,
            case Is_bin1 of
                true -> case !'Checker.is_valid_binary_literal'(C, Lit_sans_sign_and_type_prefix) of
                    true -> error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (Lit)/binary, "\" is not a valid binary number in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                    false -> ok
                end;
                false -> case Is_oct1 of
                    true -> case !'Checker.is_valid_octal_literal'(C, Lit_sans_sign_and_type_prefix) of
                        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (Lit)/binary, "\" is not a valid octal number in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                        false -> ok
                    end;
                    false -> case !'Checker.is_valid_hex_literal'(C, Lit_sans_sign_and_type_prefix) of
                        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (Lit)/binary, "\" is not a valid hexadecimal number in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                        false -> ok
                    end
                end
            end
        end;
        false -> ok
    end,
    case Has_exponent_notation of
        true -> begin
            case 'string.starts_with'('string.all_after'(Lit_lower_case, <<"e">>), <<"_">>) || 'string.ends_with'('string.all_before'(Lit_lower_case, <<"e">>), <<"_">>) of
                true -> error(todo + <<".">> + todo + <<".">> + todo + <<" the exponent in \"", (Lit)/binary, "\" can not start nor end with an underscore in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                false -> ok
            end,
            case 'string.contains'('string.all_after'(Lit_lower_case, <<"e">>), <<".">>) of
                true -> error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" (with exponent) can not have a decimal point in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                false -> ok
            end,
            case !Is_hex1 && 'string.count'(Lit_lower_case, <<"e">>) > 1 of
                true -> error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" (with exponent) can only have one exponent in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                false -> ok
            end
        end;
        false -> ok
    end,
    case Is_float of
        true -> begin
            case 'string.count'(Lit, <<".">>) > 1 of
                true -> error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" (float) can only have one decimal point in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                false -> ok
            end,
            Last = lists:nth(length(Lit) - 1 + 1, Lit),
            case Last in [todo, todo, todo, todo, todo, todo, todo] of
                true -> begin
                    Ascii3 = 'u8.ascii_str'(todo),
                    error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" (float) can not end with `", (Ascii3)/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)
                end;
                false -> ok
            end,
            case 'string.contains'(Lit, <<"_.">>) || 'string.contains'(Lit, <<"._">>) of
                true -> error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" (float) can not have underscores before or after the decimal point in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                false -> ok
            end,
            case 'string.contains'(Lit_lower_case, <<"e.">>) || 'string.contains'(Lit, <<".e">>) of
                true -> error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" (float) can not have decimal points on either side of the exponent notation in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                false -> ok
            end,
            lists:foreach(fun(R) ->
                case R !in [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo] of
                    true -> error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" (float) can not contain `", ('u8.ascii_str'(todo))/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>);
                    false -> ok
                end,
                ok
            end, Lit),
        end;
        false -> case length(Lit) > 1 && 'string.starts_with'(Lit, <<"0">>) && lists:nth(2, Lit) !in [todo, todo, todo] of
            true -> begin
                Ascii4 = 'u8.ascii_str'(todo),
                error(todo + <<".">> + todo + <<".">> + todo + <<" numbers like \"", (Lit)/binary, "\" can not start with `", (Ascii4)/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)
            end;
            false -> ok
        end
    end,
    ok.

'Checker.is_valid_binary_literal'(C, Num) ->
    lists:foreach(fun(Ch) ->
        case Ch == todo of
            true -> ok;
            false -> ok
        end,
        case !(Ch >= todo && Ch <= todo) of
            true -> false;
            false -> ok
        end,
        ok
    end, Num),
    true.

'Checker.is_valid_octal_literal'(C, Num) ->
    lists:foreach(fun(Ch) ->
        case Ch == todo of
            true -> ok;
            false -> ok
        end,
        case !(Ch >= todo && Ch <= todo) of
            true -> false;
            false -> ok
        end,
        ok
    end, Num),
    true.

'Checker.is_valid_hex_literal'(C, Num) ->
    lists:foreach(fun(Ch) ->
        case Ch == todo of
            true -> ok;
            false -> ok
        end,
        case !'u8.is_hex_digit'(Ch) of
            true -> false;
            false -> ok
        end,
        ok
    end, Num),
    true.

'Checker.check_boolean'(C, B) ->
    Lit = maps:get(text, B),
    case Lit in [<<"true">>, <<"false">>] of
        true -> ok;
        false -> ok
    end,
    error(todo + <<".">> + todo + <<".">> + todo + <<" boolean values like \"", (Lit)/binary, "\" can only be `true` or `false` literals, not `", (Lit)/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, B)))/binary, "...">>).

'Checker.check_date_time'(C, Dt) ->
    Lit = maps:get(text, Dt),
    Split = [],
    case 'string.contains_any'('string.to_lower'(Lit), <<" _t">>) of
        true -> begin
            case 'string.contains'(Lit, <<" ">>) of
                true -> ok;
                false -> case 'string.contains'(Lit, <<"_">>) of
                    true -> ok;
                    false -> case 'string.contains'(Lit, <<"T">>) of
                        true -> ok;
                        false -> case 'string.contains'(Lit, <<"t">>) of
                            true -> ok;
                            false -> ok
                        end
                    end
                end
            end,
            case length(Split) != 2 of
                true -> error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (Lit)/binary, "\" contains too many date/time separators in ...", ('Checker.excerpt'(C, maps:get(pos, Dt)))/binary, "...">>);
                false -> ok
            end,
            'Checker.check_date'(C, #{text => lists:nth(1, Split), pos => #{len => length(lists:nth(1, Split)), line_nr => maps:get(line_nr, maps:get(pos, Dt)), pos => maps:get(pos, maps:get(pos, Dt)), col => maps:get(col, maps:get(pos, Dt)), {vbeam, type} => 'Pos'}, {vbeam, type} => 'Date'}),
            'Checker.check_time'(C, #{text => lists:nth(2, Split), pos => #{len => length(lists:nth(2, Split)), line_nr => maps:get(line_nr, maps:get(pos, Dt)), pos => maps:get(pos, maps:get(pos, Dt)) + length(lists:nth(1, Split)), col => maps:get(col, maps:get(pos, Dt)) + length(lists:nth(1, Split)), {vbeam, type} => 'Pos'}, {vbeam, type} => 'Time'}),
            Has_time_offset = false,
            lists:foreach(fun(Ch) ->
                case Ch in [todo, todo, todo] of
                    true -> begin
                        Has_time_offset1 = true,
                        % TODO: break
                    end;
                    false -> ok
                end,
                ok
            end, lists:nth(todo + 1, Lit)),
            Lit_with_offset = Lit,
            case !Has_time_offset1 of
                true -> ok;
                false -> ok
            end,
            toml_parse_time(Lit_with_offset)
        end;
        false -> error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (Lit)/binary, "\" is not a valid RFC 3339 Date-Time format string in ...", ('Checker.excerpt'(C, maps:get(pos, Dt)))/binary, "...">>)
    end,
    ok.

'Checker.check_date'(C, Date) ->
    Lit = maps:get(text, Date),
    Parts = 'string.split'(Lit, <<"-">>),
    case length(Parts) != 3 of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (Lit)/binary, "\" is not a valid RFC 3339 Date format string in ...", ('Checker.excerpt'(C, maps:get(pos, Date)))/binary, "...">>);
        false -> ok
    end,
    Yyyy = lists:nth(1, Parts),
    case length(Yyyy) != 4 of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (Lit)/binary, "\" does not have a valid RFC 3339 year indication in ...", ('Checker.excerpt'(C, maps:get(pos, Date)))/binary, "...">>);
        false -> ok
    end,
    Mm = lists:nth(2, Parts),
    case length(Mm) != 2 of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (Lit)/binary, "\" does not have a valid RFC 3339 month indication in ...", ('Checker.excerpt'(C, maps:get(pos, Date)))/binary, "...">>);
        false -> ok
    end,
    Dd = lists:nth(3, Parts),
    case length(Dd) != 2 of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (Lit)/binary, "\" does not have a valid RFC 3339 day indication in ...", ('Checker.excerpt'(C, maps:get(pos, Date)))/binary, "...">>);
        false -> ok
    end,
    case 'string.int'(Mm) == 2 of
        true -> begin
            Ddi = 'string.int'(Dd),
            case Ddi > 28 of
                true -> case Ddi == 29 of
                    true -> begin
                        Yyyyi = 'string.int'(Yyyy),
                        case !(Yyyyi % 4 == 0 && (Yyyyi % 100 != 0 || Yyyyi % 400 == 0)) of
                            true -> error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (Lit)/binary, "\" is not a valid RFC 3339 date: ", (Yyyy)/binary, " is not a leap year so February can not have 29 days in it ...", ('Checker.excerpt'(C, maps:get(pos, Date)))/binary, "...">>);
                            false -> ok
                        end
                    end;
                    false -> error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (Lit)/binary, "\" is not a valid RFC 3339 date: February can not have more that 28 or 29 days in it ...", ('Checker.excerpt'(C, maps:get(pos, Date)))/binary, "...">>)
                end;
                false -> ok
            end
        end;
        false -> ok
    end,
    toml_parse_time(Lit),
    ok.

'Checker.check_time'(C, T) ->
    Lit = maps:get(text, T),
    Offset_splitter = case 'string.contains'(Lit, <<"+">>) of
        true -> <<"+">>;
        false -> <<"-">>
    end,
    Parts = 'string.split'(Lit, Offset_splitter),
    Hhmmss = 'string.all_before'(lists:nth(1, Parts), <<".">>),
    Check_length = 8,
    case 'string.ends_with'('string.to_upper'(Hhmmss), <<"Z">>) of
        true -> todo;
        false -> ok
    end,
    case length(Hhmmss) != Check_length of
        true -> begin
            Starts_with_zero = 'string.starts_with'(Hhmmss, <<"0">>),
            case !Starts_with_zero of
                true -> error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (Lit)/binary, "\" must be zero prefixed in ...", ('Checker.excerpt'(C, maps:get(pos, T)))/binary, "...">>);
                false -> ok
            end,
            error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (Lit)/binary, "\" is not a valid RFC 3339 Time format string in ...", ('Checker.excerpt'(C, maps:get(pos, T)))/binary, "...">>)
        end;
        false -> ok
    end,
    case length(Parts) > 1 of
        true -> begin
            Offset_parts = 'string.split'(lists:nth(2, Parts), <<":">>),
            case length(Offset_parts) != 2 of
                true -> error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (lists:nth(2, Parts))/binary, "\" is not a valid RFC 3339 time offset specifier in ...", ('Checker.excerpt'(C, maps:get(pos, T)))/binary, "...">>);
                false -> ok
            end,
            Hh = 'string.int'(lists:nth(1, Offset_parts)),
            case Hh < 0 || Hh > 24 of
                true -> begin
                    Pos = #{pos => maps:get(pos, maps:get(pos, T)) + Check_length, {vbeam, type} => 'Pos'},
                    error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (integer_to_binary(Hh))/binary, "\" hour specifier in \"", (lists:nth(2, Parts))/binary, "\" should be between 00 and 24 in ...", ('Checker.excerpt'(C, Pos))/binary, "...">>)
                end;
                false -> ok
            end,
            Mm = 'string.int'(lists:nth(2, Offset_parts)),
            case Mm < 0 || Mm > 59 of
                true -> begin
                    Pos1 = #{pos => maps:get(pos, maps:get(pos, T)) + Check_length, {vbeam, type} => 'Pos'},
                    error(todo + <<".">> + todo + <<".">> + todo + <<" \"", (integer_to_binary(Mm))/binary, "\" second specifier in \"", (lists:nth(2, Parts))/binary, "\" should be between 00 and 59 in ...", ('Checker.excerpt'(C, Pos1))/binary, "...">>)
                end;
                false -> ok
            end
        end;
        false -> ok
    end,
    Has_time_offset = false,
    lists:foreach(fun(Ch) ->
        case Ch in [todo, todo, todo] of
            true -> begin
                Has_time_offset1 = true,
                % TODO: break
            end;
            false -> ok
        end,
        ok
    end, lists:nth(todo + 1, lists:nth(1, Parts))),
    Part_with_offset = lists:nth(1, Parts),
    case !Has_time_offset1 of
        true -> ok;
        false -> ok
    end,
    toml_parse_time(Part_with_offset),
    ok.

'Checker.check_quoted'(C, Q) ->
    Lit = maps:get(text, Q),
    Quote = 'u8.ascii_str'(maps:get(quote, Q)),
    Triple_quote = Quote + Quote + Quote,
    case maps:get(is_multiline, Q) && 'string.ends_with'(Lit, Triple_quote) && !'string.ends_with'(Lit, <<"\\\\">> + Triple_quote) of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" string values like \"", (Lit)/binary, "\" has unbalanced quote literals `", (Quote)/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, Q)))/binary, "...">>);
        false -> ok
    end,
    'Checker.check_quoted_escapes'(C, Q),
    'Checker.check_utf8_validity'(C, Q),
    ok.

'Checker.check_quoted_escapes'(C, Q) ->
    S = new_simple_text(maps:get(text, Q)),
    Is_basic = maps:get(quote, Q) == todo,
    Contains_newlines = 'string.contains'(maps:get(text, Q), <<"\\n">>),
    % TODO: for {
    ok.

'Checker.check_utf8_validity'(C, Q) ->
    Lit = maps:get(text, Q),
    case !validate_str(Lit) of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" the string value \"", (Lit)/binary, "\" is not valid UTF-8 in ...", ('Checker.excerpt'(C, maps:get(pos, Q)))/binary, "...">>);
        false -> ok
    end,
    ok.

validate_utf8_codepoint_string(Str) ->
    Int_val = parse_int(Str, 16, 64),
    case Int_val > 0x10FFFF || Int_val < 0 of
        true -> error(<<"Unicode code point `", (Str)/binary, "` is outside the valid Unicode scalar value ranges.">>);
        false -> ok
    end,
    case !((Int_val >= 0x0000 && Int_val <= 0xD7FF) || (Int_val >= 0xE000 && Int_val <= 0x10FFFF)) of
        true -> error(<<"Unicode code point `", (Str)/binary, "` is not a valid Unicode scalar value.">>);
        false -> ok
    end,
    Bytes = 'string.bytes'(Str),
    case !validate(maps:get(data, Bytes), length(Bytes)) of
        true -> error(<<"Unicode code point `", (Str)/binary, "` is not a valid UTF-8 code point.">>);
        false -> ok
    end,
    ok.

'Checker.check_unicode_escape'(C, Esc_unicode) ->
    case length(Esc_unicode) < 5 || !'string.starts_with'('string.to_lower'(Esc_unicode), <<"u">>) of
        true -> error(<<"`", (Esc_unicode)/binary, "` is not a valid escaped Unicode sequence.">>);
        false -> ok
    end,
    Is_long_esc_type = 'string.starts_with'(Esc_unicode, <<"U">>),
    Sequence = lists:nth(todo + 1, Esc_unicode),
    Hex_digits_len = case Is_long_esc_type of
        true -> 8;
        false -> 4
    end,
    case length(Sequence) < Hex_digits_len of
        true -> error(<<"Unicode escape sequence `", (Esc_unicode)/binary, "` should be at least ", (integer_to_binary(Hex_digits_len))/binary, " in length.">>);
        false -> ok
    end,
    Sequence1 = lists:nth(todo + 1, Sequence),
    validate_utf8_codepoint_string('string.to_upper'(Sequence1)),
    case Is_long_esc_type of
        true -> ok;
        false -> ok
    end,
    ok.

'Checker.check_comment'(C, Comment) ->
    Lit = maps:get(text, Comment),
    S = new_simple_text(Lit),
    % TODO: for {
    case !validate_str(Lit) of
        true -> error(todo + <<".">> + todo + <<".">> + todo + <<" comment \"", (Lit)/binary, "\" is not valid UTF-8 in ...", ('Checker.excerpt'(C, maps:get(pos, Comment)))/binary, "...">>);
        false -> ok
    end,
    ok.
