-module('v.toml.checker').
-export([toml_parse_time/1, 'Checker.check'/2, 'Checker.visit'/2, 'Checker.excerpt'/2, is_hex_bin_oct_prefixed/1, has_repeating/2, 'Checker.check_number'/2, 'Checker.is_valid_binary_literal'/2, 'Checker.is_valid_octal_literal'/2, 'Checker.is_valid_hex_literal'/2, 'Checker.check_boolean'/2, 'Checker.check_date_time'/2, 'Checker.check_date'/2, 'Checker.check_time'/2, 'Checker.check_quoted'/2, 'Checker.check_quoted_escapes'/2, 'Checker.check_utf8_validity'/2, validate_utf8_codepoint_string/1, 'Checker.check_unicode_escape'/2, 'Checker.check_comment'/2]).

toml_parse_time(S) ->
    case length(S) > 3 andalso lists:nth(3, S) == todo of
        true -> parse_rfc3339(<<(<<"0001-01-01T">>)/binary, (S)/binary>>);
        false -> 
            case length(S) == 10 of
                true -> parse_rfc3339(<<(S)/binary, (<<"T00:00:00Z">>)/binary>>);
                false -> parse_rfc3339(S)
                        end
                end.

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
    length(Hbo) > 2 andalso ('string.starts_with'(Hbo, <<"0x">>) orelse 'string.starts_with'(Hbo, <<"0o">>) orelse 'string.starts_with'(Hbo, <<"0b">>)).

has_repeating(Str, Repeats) ->
    lists:foreach(fun(R) ->
        case lists:member(R, Repeats) andalso I + 1 < length(Str) of
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
    case lists:member(Lit, [<<"0">>, <<"0.0">>, <<"+0">>, <<"-0">>, <<"+0.0">>, <<"-0.0">>, <<"0e0">>, <<"+0e0">>, <<"-0e0">>, <<"0e00">>]) of
        true -> ok;
        false -> begin
            case 'string.contains'(Lit, <<"_">>) of
                true -> begin
                    case 'string.starts_with'(Lit, <<"_">>) orelse 'string.ends_with'(Lit, <<"_">>) of
                        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" can not start or end with `_` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                        false -> ok
                    end,
                    case 'string.contains'(Lit, <<"__">>) of
                        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" can not have more than one underscore (`_`) in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
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
            Is_sign_prefixed = lists:member(lists:nth(1, Lit), [todo, todo]),
            Lit_sans_sign = Lit,
            case Is_sign_prefixed of
                true -> begin
                    Lit_sans_sign1 = lists:nth(todo + 1, Lit),
                    Hex_bin_oct1 = is_hex_bin_oct_prefixed(Lit_sans_sign1),
                    case Hex_bin_oct1 of
                        true -> begin
                            Ascii1 = 'u8.ascii_str'(todo),
                            error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" (hex, octal and binary) can not start with `", (Ascii1)/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>)
                        end;
                        false -> ok
                    end,
                    case length(Lit) > 1 andalso 'string.starts_with'(Lit_sans_sign1, <<"0">>) andalso not 'string.starts_with'(Lit_sans_sign1, <<"0.">>) of
                        true -> begin
                            Ascii2 = 'u8.ascii_str'(todo),
                            error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" can not start with `", (Ascii2)/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>)
                        end;
                        false -> ok
                    end
                end;
                false -> case not Hex_bin_oct1 of
                    true -> begin
                        case not Is_float andalso lists:nth(1, Lit) == todo of
                            true -> begin
                                case lists:member(lists:nth(2, Lit), [todo, todo, todo]) of
                                    true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" only lowercase notation in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                                    false -> ok
                                end,
                                error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" can not start with a zero in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>)
                            end;
                            false -> ok
                        end,
                        case Is_float andalso lists:nth(1, Lit) == todo andalso Float_decimal_index > 1 of
                            true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" can not start with a zero in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                            false -> ok
                        end
                    end;
                    false -> ok
                end
            end,
            case has_repeating(Lit, [todo, todo, todo, todo, todo]) of
                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" can not have ", ([todo, todo, todo, todo, todo, todo, todo])/binary, " as repeating characters in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                false -> begin
                    case Hex_bin_oct1 of
                        true -> begin
                            Is_bin1 = 'string.starts_with'(Lit_sans_sign1, <<"0b">>),
                            Is_oct1 = 'string.starts_with'(Lit_sans_sign1, <<"0o">>),
                            Is_hex1 = 'string.starts_with'(Lit_sans_sign1, <<"0x">>),
                            Lit_sans_sign_and_type_prefix = lists:nth(todo + 1, Lit_sans_sign1),
                            case 'string.starts_with'(Lit_sans_sign_and_type_prefix, <<"_">>) orelse 'string.ends_with'(Lit_sans_sign_and_type_prefix, <<"_">>) of
                                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" can not start or end with `_` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                                false -> ok
                            end,
                            case Is_bin1 of
                                true -> case not 'Checker.is_valid_binary_literal'(C, Lit_sans_sign_and_type_prefix) of
                                    true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (Lit)/binary, "\" is not a valid binary number in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                                    false -> ok
                                end;
                                false -> case Is_oct1 of
                                    true -> case not 'Checker.is_valid_octal_literal'(C, Lit_sans_sign_and_type_prefix) of
                                        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (Lit)/binary, "\" is not a valid octal number in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                                        false -> ok
                                    end;
                                    false -> case not 'Checker.is_valid_hex_literal'(C, Lit_sans_sign_and_type_prefix) of
                                        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (Lit)/binary, "\" is not a valid hexadecimal number in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                                        false -> ok
                                    end
                                end
                            end
                        end;
                        false -> ok
                    end,
                    case Has_exponent_notation of
                        true -> begin
                            case 'string.starts_with'('string.all_after'(Lit_lower_case, <<"e">>), <<"_">>) orelse 'string.ends_with'('string.all_before'(Lit_lower_case, <<"e">>), <<"_">>) of
                                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" the exponent in \"", (Lit)/binary, "\" can not start nor end with an underscore in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                                false -> ok
                            end,
                            case 'string.contains'('string.all_after'(Lit_lower_case, <<"e">>), <<".">>) of
                                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" (with exponent) can not have a decimal point in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                                false -> ok
                            end,
                            case not Is_hex1 andalso 'string.count'(Lit_lower_case, <<"e">>) > 1 of
                                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" (with exponent) can only have one exponent in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                                false -> ok
                            end
                        end;
                        false -> ok
                    end,
                    case Is_float of
                        true -> begin
                            case 'string.count'(Lit, <<".">>) > 1 of
                                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" (float) can only have one decimal point in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                                false -> ok
                            end,
                            Last = lists:nth(length(Lit) - 1 + 1, Lit),
                            case lists:member(Last, [todo, todo, todo, todo, todo, todo, todo]) of
                                true -> begin
                                    Ascii3 = 'u8.ascii_str'(todo),
                                    error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" (float) can not end with `", (Ascii3)/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>)
                                end;
                                false -> ok
                            end,
                            case 'string.contains'(Lit, <<"_.">>) orelse 'string.contains'(Lit, <<"._">>) of
                                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" (float) can not have underscores before or after the decimal point in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                                false -> ok
                            end,
                            case 'string.contains'(Lit_lower_case, <<"e.">>) orelse 'string.contains'(Lit, <<".e">>) of
                                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" (float) can not have decimal points on either side of the exponent notation in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                                false -> ok
                            end,
                            lists:foreach(fun(R) ->
                                case (not lists:member(R, [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo])) of
                                    true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" (float) can not contain `", ('u8.ascii_str'(todo))/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>);
                                    false -> ok
                                end,
                                ok
                            end, Lit),
                        end;
                        false -> case length(Lit) > 1 andalso 'string.starts_with'(Lit, <<"0">>) andalso (not lists:member(lists:nth(2, Lit), [todo, todo, todo])) of
                            true -> begin
                                Ascii4 = 'u8.ascii_str'(todo),
                                error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" numbers like \"", (Lit)/binary, "\" can not start with `", (Ascii4)/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, Num)))/binary, "...">>)/binary>>)
                            end;
                            false -> ok
                        end
                    end,
                    ok
                end
                        end
        end
        end.

'Checker.is_valid_binary_literal'(C, Num) ->
    lists:foreach(fun(Ch) ->
        case Ch == todo of
            true -> ok;
            false -> ok
        end,
        case not (Ch >= todo andalso Ch =< todo) of
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
        case not (Ch >= todo andalso Ch =< todo) of
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
        case not 'u8.is_hex_digit'(Ch) of
            true -> false;
            false -> ok
        end,
        ok
    end, Num),
    true.

'Checker.check_boolean'(C, B) ->
    Lit = maps:get(text, B),
    case lists:member(Lit, [<<"true">>, <<"false">>]) of
        true -> ok;
        false -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" boolean values like \"", (Lit)/binary, "\" can only be `true` or `false` literals, not `", (Lit)/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, B)))/binary, "...">>)/binary>>)
        end.

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
            case length(Split) /= 2 of
                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (Lit)/binary, "\" contains too many date/time separators in ...", ('Checker.excerpt'(C, maps:get(pos, Dt)))/binary, "...">>)/binary>>);
                false -> ok
            end,
            'Checker.check_date'(C, #{text => lists:nth(1, Split), pos => #{len => length(lists:nth(1, Split)), line_nr => maps:get(line_nr, maps:get(pos, Dt)), pos => maps:get(pos, maps:get(pos, Dt)), col => maps:get(col, maps:get(pos, Dt)), {vbeam, type} => 'Pos'}, {vbeam, type} => 'Date'}),
            'Checker.check_time'(C, #{text => lists:nth(2, Split), pos => #{len => length(lists:nth(2, Split)), line_nr => maps:get(line_nr, maps:get(pos, Dt)), pos => maps:get(pos, maps:get(pos, Dt)) + length(lists:nth(1, Split)), col => maps:get(col, maps:get(pos, Dt)) + length(lists:nth(1, Split)), {vbeam, type} => 'Pos'}, {vbeam, type} => 'Time'}),
            Has_time_offset = false,
            lists:foreach(fun(Ch) ->
                case lists:member(Ch, [todo, todo, todo]) of
                    true -> begin
                        Has_time_offset1 = true,
                        % TODO: unhandled stmt type
                        ok                    end;
                    false -> ok
                end,
                ok
            end, lists:nth(todo + 1, Lit)),
            Lit_with_offset = Lit,
            case not Has_time_offset1 of
                true -> ok;
                false -> ok
            end,
            toml_parse_time(Lit_with_offset)
        end;
        false -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (Lit)/binary, "\" is not a valid RFC 3339 Date-Time format string in ...", ('Checker.excerpt'(C, maps:get(pos, Dt)))/binary, "...">>)/binary>>)
    end,
    ok.

'Checker.check_date'(C, Date) ->
    Lit = maps:get(text, Date),
    Parts = 'string.split'(Lit, <<"-">>),
    case length(Parts) /= 3 of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (Lit)/binary, "\" is not a valid RFC 3339 Date format string in ...", ('Checker.excerpt'(C, maps:get(pos, Date)))/binary, "...">>)/binary>>);
        false -> begin
            Yyyy = lists:nth(1, Parts),
            case length(Yyyy) /= 4 of
                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (Lit)/binary, "\" does not have a valid RFC 3339 year indication in ...", ('Checker.excerpt'(C, maps:get(pos, Date)))/binary, "...">>)/binary>>);
                false -> begin
                    Mm = lists:nth(2, Parts),
                    case length(Mm) /= 2 of
                        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (Lit)/binary, "\" does not have a valid RFC 3339 month indication in ...", ('Checker.excerpt'(C, maps:get(pos, Date)))/binary, "...">>)/binary>>);
                        false -> begin
                            Dd = lists:nth(3, Parts),
                            case length(Dd) /= 2 of
                                true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (Lit)/binary, "\" does not have a valid RFC 3339 day indication in ...", ('Checker.excerpt'(C, maps:get(pos, Date)))/binary, "...">>)/binary>>);
                                false -> begin
                                    case 'string.int'(Mm) == 2 of
                                        true -> begin
                                            Ddi = 'string.int'(Dd),
                                            case Ddi > 28 of
                                                true -> case Ddi == 29 of
                                                    true -> begin
                                                        Yyyyi = 'string.int'(Yyyy),
                                                        case not (Yyyyi rem 4 == 0 andalso (Yyyyi rem 100 /= 0 orelse Yyyyi rem 400 == 0)) of
                                                            true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (Lit)/binary, "\" is not a valid RFC 3339 date: ", (Yyyy)/binary, " is not a leap year so February can not have 29 days in it ...", ('Checker.excerpt'(C, maps:get(pos, Date)))/binary, "...">>)/binary>>);
                                                            false -> ok
                                                        end
                                                    end;
                                                    false -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (Lit)/binary, "\" is not a valid RFC 3339 date: February can not have more that 28 or 29 days in it ...", ('Checker.excerpt'(C, maps:get(pos, Date)))/binary, "...">>)/binary>>)
                                                end;
                                                false -> ok
                                            end
                                        end;
                                        false -> ok
                                    end,
                                    toml_parse_time(Lit),
                                    ok
                                end
                                                        end
                        end
                                        end
                end
                        end
        end
        end.

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
    case length(Hhmmss) /= Check_length of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (Lit)/binary, "\" is not a valid RFC 3339 Time format string in ...", ('Checker.excerpt'(C, maps:get(pos, T)))/binary, "...">>)/binary>>);
        false -> begin
            case length(Parts) > 1 of
                true -> begin
                    Offset_parts = 'string.split'(lists:nth(2, Parts), <<":">>),
                    case length(Offset_parts) /= 2 of
                        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (lists:nth(2, Parts))/binary, "\" is not a valid RFC 3339 time offset specifier in ...", ('Checker.excerpt'(C, maps:get(pos, T)))/binary, "...">>)/binary>>);
                        false -> ok
                    end,
                    Hh = 'string.int'(lists:nth(1, Offset_parts)),
                    case Hh < 0 orelse Hh > 24 of
                        true -> begin
                            Pos = #{pos => maps:get(pos, maps:get(pos, T)) + Check_length, {vbeam, type} => 'Pos'},
                            error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (integer_to_binary(Hh))/binary, "\" hour specifier in \"", (lists:nth(2, Parts))/binary, "\" should be between 00 and 24 in ...", ('Checker.excerpt'(C, Pos))/binary, "...">>)/binary>>)
                        end;
                        false -> ok
                    end,
                    Mm = 'string.int'(lists:nth(2, Offset_parts)),
                    case Mm < 0 orelse Mm > 59 of
                        true -> begin
                            Pos1 = #{pos => maps:get(pos, maps:get(pos, T)) + Check_length, {vbeam, type} => 'Pos'},
                            error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" \"", (integer_to_binary(Mm))/binary, "\" second specifier in \"", (lists:nth(2, Parts))/binary, "\" should be between 00 and 59 in ...", ('Checker.excerpt'(C, Pos1))/binary, "...">>)/binary>>)
                        end;
                        false -> ok
                    end
                end;
                false -> ok
            end,
            Has_time_offset = false,
            lists:foreach(fun(Ch) ->
                case lists:member(Ch, [todo, todo, todo]) of
                    true -> begin
                        Has_time_offset1 = true,
                        % TODO: unhandled stmt type
                        ok                    end;
                    false -> ok
                end,
                ok
            end, lists:nth(todo + 1, lists:nth(1, Parts))),
            Part_with_offset = lists:nth(1, Parts),
            case not Has_time_offset1 of
                true -> ok;
                false -> ok
            end,
            toml_parse_time(Part_with_offset),
            ok
        end
        end.

'Checker.check_quoted'(C, Q) ->
    Lit = maps:get(text, Q),
    Quote = 'u8.ascii_str'(maps:get(quote, Q)),
    Triple_quote = <<(<<(Quote)/binary, (Quote)/binary>>)/binary, (Quote)/binary>>,
    case maps:get(is_multiline, Q) andalso 'string.ends_with'(Lit, Triple_quote) andalso not 'string.ends_with'(Lit, <<(<<"\\\\">>)/binary, (Triple_quote)/binary>>) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" string values like \"", (Lit)/binary, "\" has unbalanced quote literals `", (Quote)/binary, "` in ...", ('Checker.excerpt'(C, maps:get(pos, Q)))/binary, "...">>)/binary>>);
        false -> begin
            'Checker.check_quoted_escapes'(C, Q),
            'Checker.check_utf8_validity'(C, Q),
            ok
        end
        end.

'Checker.check_quoted_escapes'(C, Q) ->
    S = new_simple_text(maps:get(text, Q)),
    Is_basic = maps:get(quote, Q) == todo,
    Contains_newlines = 'string.contains'(maps:get(text, Q), <<"\\n">>),
    % TODO: unhandled stmt type
    ok    ok.

'Checker.check_utf8_validity'(C, Q) ->
    Lit = maps:get(text, Q),
    case not validate_str(Lit) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" the string value \"", (Lit)/binary, "\" is not valid UTF-8 in ...", ('Checker.excerpt'(C, maps:get(pos, Q)))/binary, "...">>)/binary>>);
        false -> ok
        end.

validate_utf8_codepoint_string(Str) ->
    Int_val = parse_int(Str, 16, 64),
    case Int_val > 16#10FFFF orelse Int_val < 0 of
        true -> error(<<"Unicode code point `", (Str)/binary, "` is outside the valid Unicode scalar value ranges.">>);
        false -> 
            case not ((Int_val >= 16#0000 andalso Int_val =< 16#D7FF) orelse (Int_val >= 16#E000 andalso Int_val =< 16#10FFFF)) of
                true -> error(<<"Unicode code point `", (Str)/binary, "` is not a valid Unicode scalar value.">>);
                false -> begin
                    Bytes = 'string.bytes'(Str),
                    case not validate(maps:get(data, Bytes), length(Bytes)) of
                        true -> error(<<"Unicode code point `", (Str)/binary, "` is not a valid UTF-8 code point.">>);
                        false -> ok
                                        end
                end
                        end
                end.

'Checker.check_unicode_escape'(C, Esc_unicode) ->
    case length(Esc_unicode) < 5 orelse not 'string.starts_with'('string.to_lower'(Esc_unicode), <<"u">>) of
        true -> error(<<"`", (Esc_unicode)/binary, "` is not a valid escaped Unicode sequence.">>);
        false -> begin
            Is_long_esc_type = 'string.starts_with'(Esc_unicode, <<"U">>),
            Sequence = lists:nth(todo + 1, Esc_unicode),
            Hex_digits_len = case Is_long_esc_type of
                true -> 8;
                false -> 4
            end,
            case length(Sequence) < Hex_digits_len of
                true -> error(<<"Unicode escape sequence `", (Esc_unicode)/binary, "` should be at least ", (integer_to_binary(Hex_digits_len))/binary, " in length.">>);
                false -> begin
                    Sequence1 = lists:nth(todo + 1, Sequence),
                    validate_utf8_codepoint_string('string.to_upper'(Sequence1)),
                    case Is_long_esc_type of
                        true -> ok;
                        false -> ok
                    end,
                    ok
                end
                        end
        end
        end.

'Checker.check_comment'(C, Comment) ->
    Lit = maps:get(text, Comment),
    S = new_simple_text(Lit),
    % TODO: unhandled stmt type
    ok    case not validate_str(Lit) of
        true -> error(<<(<<(<<(<<(<<(todo)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<".">>)/binary>>)/binary, (todo)/binary>>)/binary, (<<" comment \"", (Lit)/binary, "\" is not valid UTF-8 in ...", ('Checker.excerpt'(C, maps:get(pos, Comment)))/binary, "...">>)/binary>>);
        false -> ok
        end.
