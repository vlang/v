-module('v.time').
-export([days_from_unix_epoch/3, 'Time.days_from_unix_epoch'/1, date_from_days_after_unix_epoch/1, new_date_time_parser/2, 'DateTimeParser.next'/2, 'DateTimeParser.peek'/2, 'DateTimeParser.must_be_int'/2, 'DateTimeParser.must_be_int_with_minimum_length'/4, 'DateTimeParser.must_be_string'/2, 'DateTimeParser.must_be_string_one_of'/2, 'DateTimeParser.must_be_valid_month'/1, 'DateTimeParser.must_be_valid_three_letter_month'/1, 'DateTimeParser.must_be_valid_week_day'/1, 'DateTimeParser.must_be_valid_two_letter_week_day'/1, 'DateTimeParser.must_be_valid_three_letter_week_day'/1, extract_tokens/1, 'DateTimeParser.parse'/1, 'Duration.nanoseconds'/1, 'Duration.microseconds'/1, 'Duration.milliseconds'/1, 'Duration.seconds'/1, 'Duration.minutes'/1, 'Duration.hours'/1, 'Duration.days'/1, 'Duration.str'/1, 'Duration.debug'/1, 'Duration.times'/2, iclamp/3, int_to_byte_array_no_pad/3, int_to_ptr_byte_array_no_pad/3, 'Time.format'/1, 'Time.format_ss'/1, 'Time.format_ss_milli'/1, 'Time.format_ss_micro'/1, 'Time.format_ss_nano'/1, 'Time.format_rfc3339'/1, 'Time.format_rfc3339_micro'/1, 'Time.format_rfc3339_nano'/1, 'Time.hhmm'/1, 'Time.hhmmss'/1, 'Time.hhmm12'/1, 'Time.ymmdd'/1, 'Time.ddmmy'/1, 'Time.md'/1, ordinal_suffix/1, 'Time.custom_format'/2, 'Time.clean'/1, 'Time.clean12'/1, 'Time.get_fmt_time_str'/2, 'Time.get_fmt_date_str'/3, 'Time.get_fmt_str'/4, 'Time.utc_string'/1, 'Time.http_header_string'/1, 'Time.push_to_http_header'/2, mceil/1, 'Time.=='/2, 'Time.<'/2, 'Time.-'/2, 'TimeParseError.msg'/1, error_invalid_time/2, new_stopwatch/1, 'StopWatch.start'/1, 'StopWatch.restart'/1, 'StopWatch.stop'/1, 'StopWatch.pause'/1, 'StopWatch.elapsed'/1, now/0, utc/0, 'Time.local'/1, sleep/1, ticks/0, sys_mono_now/0, time_with_unix/1, calculate_unix_time/6, 'Time.strftime'/2, parse_iso8601/1, parse_rfc3339/1, parse/1, parse_format/2, 'Time__static__new'/1, new/1, 'Time.smonth'/1, 'Time.unix'/1, 'Time.local_unix'/1, 'Time.unix_milli'/1, 'Time.unix_micro'/1, 'Time.unix_nano'/1, 'Time.add'/2, 'Time.add_seconds'/2, 'Time.add_days'/2, since/1, 'Time.relative'/1, 'Time.relative_short'/1, day_of_week/3, 'Time.day_of_week'/1, 'Time.week_of_year'/1, 'Time.year_day'/1, 'Time.weekday_str'/1, 'Time.long_weekday_str'/1, is_leap_year/1, days_in_month/2, 'Time.debug'/1, offset/0, 'Time.local_to_utc'/1, 'Time.utc_to_local'/1, 'Time.as_local'/1, 'Time.as_utc'/1, 'Time.is_utc'/1, unix/1, unix_milli/1, unix_micro/1, unix_nano/1, ts_to_time_impl/3, unix_microsecond/2, unix_nanosecond/2, calculate_date_from_day_offset/1, calculate_time_from_second_offset/1, 'FormatTime__static__from'/1, 'FormatDate__static__from'/1, 'FormatDelimiter__static__from'/1]).

days_from_unix_epoch(Year, Month, Day) ->
    Y = case Month =< 2 of
        true -> Year - 1;
        false -> Year
    end,
    Era = Y div 400,
    Year_of_the_era = Y - Era * 400,
    Day_of_year = (153 * (Month + (case Month > 2 of
        true -> -3;
        false -> 9
    end)) + 2) div 5 + Day - 1,
    Day_of_the_era = Year_of_the_era * 365 + Year_of_the_era div 4 - Year_of_the_era div 100 + Day_of_year,
    Era * 146097 + Day_of_the_era - 719468.

'Time.days_from_unix_epoch'(T) ->
    days_from_unix_epoch(maps:get(year, T), maps:get(month, T), maps:get(day, T)).

date_from_days_after_unix_epoch(Days) ->
    Year = element(1, calculate_date_from_day_offset(todo)),
    Month = element(2, calculate_date_from_day_offset(todo)),
    Day = element(3, calculate_date_from_day_offset(todo)),
    #{year => Year, month => Month, day => Day, {vbeam, type} => 'Time'}.

new_date_time_parser(Datetime, Format) ->
    #{datetime => Datetime, format => Format, {vbeam, type} => 'DateTimeParser'}.

'DateTimeParser.next'(P, Length) ->
    case maps:get(current_pos_datetime, P) + Length > length(maps:get(datetime, P)) of
        true -> error(<<"end of string">>);
        false -> begin
            Val = lists:nth(todo + 1, maps:get(datetime, P)),
            Val
        end
        end.

'DateTimeParser.peek'(P, Length) ->
    case maps:get(current_pos_datetime, P) + Length > length(maps:get(datetime, P)) of
        true -> error(<<"end of string">>);
        false -> lists:nth(todo + 1, maps:get(datetime, P))
        end.

'DateTimeParser.must_be_int'(P, Length) ->
    Val = 'DateTimeParser.next'(P, Length),
    case not 'string.contains_only'(Val, <<"0123456789">>) of
        true -> error(<<"expected int, found: ", (Val)/binary>>);
        false -> binary_to_integer(Val)
        end.

'DateTimeParser.must_be_int_with_minimum_length'(P, Min, Max, Allow_leading_zero) ->
    Length = Max + 1 - Min,
    Val = <<"">>,
    {Tok, Val1} = lists:foldl(fun(_, {TokAcc, ValAcc}) ->
        TokOut = 'DateTimeParser.peek'(P, 1),
        case not 'string.contains_only'(Tok, <<"0123456789">>) of
            true -> ok;
            false -> ok
        end,
        'DateTimeParser.next'(P, 1),
        ValOut = TokAcc,
        {TokOut, ValOut}
    end, {Tok, Val}, lists:seq(0, Length - 1)),
    case length(Val1) < Min of
        true -> error(<<"expected int with a minimum length of ", (integer_to_binary(Min))/binary, ", found: ", (integer_to_binary(length(Val1)))/binary>>);
        false -> 
            case not Allow_leading_zero andalso case string:prefix(Val1, <<"0">>) of nomatch -> false; _ -> true end of
                true -> error(<<"0 is not allowed for this format">>);
                false -> binary_to_integer(Val1)
                        end
                end.

'DateTimeParser.must_be_string'(P, Must) ->
    Val = 'DateTimeParser.next'(P, length(Must)),
    case Val /= Must of
        true -> error(<<"invalid string: \"", (Val)/binary, "\"!=\"", (Must)/binary, "\" at: ", (integer_to_binary(maps:get(current_pos_datetime, P)))/binary>>);
        false -> ok
        end.

'DateTimeParser.must_be_string_one_of'(P, Oneof) ->
    lists:foreach(fun(Must) ->
        Val = 'DateTimeParser.peek'(P, length(Must)),
        case Val == Must of
            true -> Must;
            false -> ok
        end,
        ok
    end, Oneof),
    error(<<"invalid string: must be one of ", (Oneof)/binary, ", at: ", (integer_to_binary(maps:get(current_pos_datetime, P)))/binary>>).

'DateTimeParser.must_be_valid_month'(P) ->
    lists:foreach(fun(V) ->
        case maps:get(current_pos_datetime, P) + length(V) < length(maps:get(datetime, P)) of
            true -> begin
                Month_name = lists:nth(todo + 1, maps:get(datetime, P)),
                case V == Month_name of
                    true -> begin
                        '[]string.index'([<<"January">>, <<"February">>, <<"March">>, <<"April">>, <<"May">>, <<"June">>, <<"July">>, <<"August">>, <<"September">>, <<"October">>, <<"November">>, <<"December">>], Month_name) + 1
                    end;
                    false -> ok
                end
            end;
            false -> ok
        end,
        ok
    end, [<<"January">>, <<"February">>, <<"March">>, <<"April">>, <<"May">>, <<"June">>, <<"July">>, <<"August">>, <<"September">>, <<"October">>, <<"November">>, <<"December">>]),
    error_invalid_time(0, <<"invalid month name, at: ", (integer_to_binary(maps:get(current_pos_datetime, P)))/binary>>).

'DateTimeParser.must_be_valid_three_letter_month'(P) ->
    case maps:get(current_pos_datetime, P) + 3 < length(maps:get(datetime, P)) of
        true -> begin
            Letters = lists:nth(todo + 1, maps:get(datetime, P)),
            % TODO: unhandled stmt type
        end;
        false -> ok
    end,
    error_invalid_time(0, <<"invalid three letter month, at: ", (integer_to_binary(maps:get(current_pos_datetime, P)))/binary>>).

'DateTimeParser.must_be_valid_week_day'(P) ->
    lists:foreach(fun(V) ->
        case maps:get(current_pos_datetime, P) + length(V) < length(maps:get(datetime, P)) of
            true -> begin
                Weekday = lists:nth(todo + 1, maps:get(datetime, P)),
                case V == Weekday of
                    true -> begin
                        Weekday
                    end;
                    false -> ok
                end
            end;
            false -> ok
        end,
        ok
    end, [<<"Monday">>, <<"Tuesday">>, <<"Wednesday">>, <<"Thursday">>, <<"Friday">>, <<"Saturday">>, <<"Sunday">>]),
    error_invalid_time(0, <<"invalid weekday, at: ", (integer_to_binary(maps:get(current_pos_datetime, P)))/binary>>).

'DateTimeParser.must_be_valid_two_letter_week_day'(P) ->
    case maps:get(current_pos_datetime, P) + 2 < length(maps:get(datetime, P)) of
        true -> begin
            Letters = lists:nth(todo + 1, maps:get(datetime, P)),
            % TODO: unhandled stmt type
        end;
        false -> ok
    end,
    error_invalid_time(0, <<"invalid two letter weekday, at: ", (integer_to_binary(maps:get(current_pos_datetime, P)))/binary>>).

'DateTimeParser.must_be_valid_three_letter_week_day'(P) ->
    case maps:get(current_pos_datetime, P) + 3 < length(maps:get(datetime, P)) of
        true -> begin
            Letters = lists:nth(todo + 1, maps:get(datetime, P)),
            % TODO: unhandled stmt type
        end;
        false -> ok
    end,
    error_invalid_time(0, <<"invalid three letter weekday, at: ", (integer_to_binary(maps:get(current_pos_datetime, P)))/binary>>).

extract_tokens(S) ->
    Tokens = [],
    Current = <<"">>,
    lists:foreach(fun(R) ->
        case 'string.contains_only'(Current, 'u8.ascii_str'(R)) orelse Current == <<"">> of
            true -> ok;
            false -> begin
                Tokens bsl Current,
                Current1 = 'u8.ascii_str'(R),
            end
        end,
        ok
    end, S),
    case Current1 /= <<"">> of
        true -> Tokens bsl Current1;
        false -> ok
    end,
    Tokens.

'DateTimeParser.parse'(P) ->
    Year_ = 0,
    Month_ = 0,
    Day_in_month = 0,
    Hour_ = 0,
    Minute_ = 0,
    Second_ = 0,
    Tokens = extract_tokens(maps:get(format, P)),
    lists:foreach(fun(Token) ->
        case Token of
            <<"YYYY">> -> ok;
            <<"YY">> -> ok;
            <<"M">> -> begin
                Month_1 = 'DateTimeParser.must_be_int_with_minimum_length'(P, 1, 2, false),
                case Month_1 < 1 orelse Month_1 > 12 of
                    true -> error_invalid_time(0, <<"month must be  between 1 and 12">>);
                    false -> ok
                end
            end;
            <<"MM">> -> begin
                Month_2 = 'DateTimeParser.must_be_int'(P, 2),
                case Month_2 < 1 orelse Month_2 > 12 of
                    true -> error_invalid_time(0, <<"month must be  between 01 and 12">>);
                    false -> ok
                end
            end;
            <<"MMM">> -> ok;
            <<"MMMM">> -> ok;
            <<"D">> -> begin
                Day_in_month1 = 'DateTimeParser.must_be_int_with_minimum_length'(P, 1, 2, false),
                case Day_in_month1 < 1 orelse Day_in_month1 > 31 of
                    true -> error_invalid_time(0, <<"day must be  between 1 and 31">>);
                    false -> ok
                end
            end;
            <<"DD">> -> begin
                Day_in_month2 = 'DateTimeParser.must_be_int'(P, 2),
                case Day_in_month2 < 1 orelse Day_in_month2 > 31 of
                    true -> error_invalid_time(0, <<"day must be  between 01 and 31">>);
                    false -> ok
                end
            end;
            <<"d">> -> 'DateTimeParser.must_be_int'(P, 1);
            <<"c">> -> 'DateTimeParser.must_be_int'(P, 1);
            <<"dd">> -> 'DateTimeParser.must_be_valid_two_letter_week_day'(P);
            <<"ddd">> -> 'DateTimeParser.must_be_valid_three_letter_week_day'(P);
            <<"dddd">> -> 'DateTimeParser.must_be_valid_week_day'(P);
            <<"H">> -> begin
                Hour_1 = 'DateTimeParser.must_be_int_with_minimum_length'(P, 1, 2, true),
                case Hour_1 < 0 orelse Hour_1 > 23 of
                    true -> error_invalid_time(0, <<"hour must be  between 0 and 23">>);
                    false -> ok
                end
            end;
            <<"HH">> -> begin
                Hour_2 = 'DateTimeParser.must_be_int'(P, 2),
                case Hour_2 < 0 orelse Hour_2 > 23 of
                    true -> error_invalid_time(0, <<"hour must be  between 00 and 23">>);
                    false -> ok
                end
            end;
            <<"h">> -> begin
                Hour_3 = 'DateTimeParser.must_be_int_with_minimum_length'(P, 1, 2, true),
                case Hour_3 < 0 orelse Hour_3 > 23 of
                    true -> error_invalid_time(0, <<"hour must be  between 0 and 23">>);
                    false -> ok
                end
            end;
            <<"hh">> -> begin
                Hour_4 = 'DateTimeParser.must_be_int'(P, 2),
                case Hour_4 < 0 orelse Hour_4 > 23 of
                    true -> error_invalid_time(0, <<"hour must be  between 00 and 23">>);
                    false -> ok
                end
            end;
            <<"k">> -> begin
                Hour_5 = 'DateTimeParser.must_be_int'(P, 1),
                case Hour_5 < 0 orelse Hour_5 > 23 of
                    true -> error_invalid_time(0, <<"hour must be  between 0 and 23">>);
                    false -> ok
                end
            end;
            <<"kk">> -> begin
                Hour_6 = 'DateTimeParser.must_be_int'(P, 2),
                case Hour_6 < 0 orelse Hour_6 > 23 of
                    true -> error_invalid_time(0, <<"hour must be  between 00 and 23">>);
                    false -> ok
                end
            end;
            <<"m">> -> begin
                Minute_1 = 'DateTimeParser.must_be_int'(P, 1),
                case Minute_1 < 0 orelse Minute_1 > 59 of
                    true -> error_invalid_time(0, <<"minute must be between 0 and 59">>);
                    false -> ok
                end
            end;
            <<"mm">> -> begin
                Minute_2 = 'DateTimeParser.must_be_int'(P, 2),
                case Minute_2 < 0 orelse Minute_2 > 59 of
                    true -> error_invalid_time(0, <<"minute must be between 00 and 59">>);
                    false -> ok
                end
            end;
            <<"s">> -> begin
                Second_1 = 'DateTimeParser.must_be_int'(P, 1),
                case Second_1 < 0 orelse Second_1 > 59 of
                    true -> error_invalid_time(0, <<"second must be between 0 and 59">>);
                    false -> ok
                end
            end;
            <<"ss">> -> begin
                Second_2 = 'DateTimeParser.must_be_int'(P, 2),
                case Second_2 < 0 orelse Second_2 > 59 of
                    true -> error_invalid_time(0, <<"second must be between 00 and 59">>);
                    false -> ok
                end
            end;
            _ -> 'DateTimeParser.must_be_string'(P, Token)
        end,
        ok
    end, Tokens),
    case Month_2 == 2 of
        true -> begin
            Feb_days_in_year = case is_leap_year(Year_) of
                true -> 29;
                false -> 28
            end,
            case Day_in_month2 > Feb_days_in_year of
                true -> error_invalid_time(0, <<"February has only 28 days in the given year">>);
                false -> ok
            end
        end;
        false -> case Day_in_month2 == 31 andalso (not lists:member(Month_2, [1, 3, 5, 7, 8, 10, 12])) of
            true -> begin
                Month_name = 'Time.custom_format'(#{month => Month_2, {vbeam, type} => 'Time'}, <<"MMMM">>),
                error_invalid_time(0, <<(Month_name)/binary, " has only 30 days">>)
            end;
            false -> ok
        end
    end,
    new(#{year => Year_, month => Month_2, day => Day_in_month2, hour => Hour_6, minute => Minute_2, second => Second_2, {vbeam, type} => 'Time'}).

'Duration.nanoseconds'(D) ->
    todo.

'Duration.microseconds'(D) ->
    todo / todo.

'Duration.milliseconds'(D) ->
    todo / todo.

'Duration.seconds'(D) ->
    todo / todo.

'Duration.minutes'(D) ->
    todo / todo.

'Duration.hours'(D) ->
    todo / todo.

'Duration.days'(D) ->
    todo / todo.

'Duration.str'(D) ->
    case D == todo of
        true -> <<"inf">>;
        false -> begin
            Sign = <<"">>,
            T = todo,
            case T < 0 of
                true -> begin
                    Sign1 = <<"-">>,
                    T1 = -T,
                end;
                false -> ok
            end,
            Hr = T1 / todo,
            T2 = Hr * todo,
            Min = T2 / todo,
            T3 = Min * todo,
            Sec = T3 / todo,
            T4 = Sec * todo,
            Ms = T4 / todo,
            T5 = Ms * todo,
            Us = T5 / todo,
            T6 = Us * todo,
            Ns = T6,
            if
                Hr > 0 -> <<(Sign1)/binary, (integer_to_binary(Hr))/binary, ":", (integer_to_binary(Min))/binary, ":", (integer_to_binary(Sec))/binary>>;
                Min > 0 -> <<(Sign1)/binary, (integer_to_binary(Min))/binary, ":", (integer_to_binary(Sec))/binary, ".", (integer_to_binary(Ms))/binary>>;
                Sec > 0 -> <<(Sign1)/binary, (integer_to_binary(Sec))/binary, ".", (integer_to_binary(Ms))/binary, "s">>;
                Ms > 0 -> <<(Sign1)/binary, (integer_to_binary(Ms))/binary, ".", (integer_to_binary(Us))/binary, "ms">>;
                Us > 0 -> <<(Sign1)/binary, (integer_to_binary(Us))/binary, ".", (integer_to_binary(Ns))/binary, "us">>;
                true -> <<(Sign1)/binary, (integer_to_binary(Ns))/binary, "ns">>
            end
        end
        end.

'Duration.debug'(D) ->
    Res = [],
    X = todo,
    Sign = <<"">>,
    case X < 0 of
        true -> begin
            Sign1 = <<"- ">>,
            X1 = -X,
        end;
        false -> ok
    end,
    lists:foreach(fun(V) ->
        case X1 > V of
            true -> begin
                Xx = X1 div V,
                X2 = X1 rem V,
                Res bsl <<(integer_to_binary(Xx))/binary, (Label)/binary>>
            end;
            false -> ok
        end,
        ok
    end, #{<<"days">> => 24 * todo, <<"h">> => todo, <<"m">> => todo, <<"s">> => todo, <<"ms">> => todo, <<"us">> => todo}),
    case X2 > 0 of
        true -> Res bsl <<(integer_to_binary(X2))/binary, "ns">>;
        false -> ok
    end,
    <<"Duration: ", (Sign1)/binary, (iolist_to_binary(lists:join(<<", ">>, Res)))/binary>>.

'Duration.times'(D, X) ->
    todo * X.

iclamp(X, A, B) ->
    case X < A of
        true -> A;
        false -> 
            case X > B of
                true -> B;
                false -> X
                        end
                end.

int_to_byte_array_no_pad(Value, Arr, Size) ->
    Num = Value,
    case Size =< 0 orelse Num < 0 of
        true -> ok;
        false -> begin
            I = Size - 1,
            % TODO: unhandled stmt type
                        ok
        end
        end.

int_to_ptr_byte_array_no_pad(Value, Arr_prt, Arr_len) ->
    Num = Value,
    case Arr_len =< 0 orelse Num < 0 of
        true -> ok;
        false -> begin
            I = Arr_len - 1,
            % TODO: unhandled stmt type
                        ok
        end
        end.

'Time.format'(T) ->
    Buf = [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo],
    % TODO: unhandled stmt type
    int_to_byte_array_no_pad(maps:get(year, T), Buf, 4),
    int_to_byte_array_no_pad(maps:get(month, T), Buf, 7),
    int_to_byte_array_no_pad(maps:get(day, T), Buf, 10),
    int_to_byte_array_no_pad(maps:get(hour, T), Buf, 13),
    int_to_byte_array_no_pad(maps:get(minute, T), Buf, 16),
    '[]u8.bytestr'(Buf).

'Time.format_ss'(T) ->
    Buf = [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo],
    % TODO: unhandled stmt type
    int_to_byte_array_no_pad(maps:get(year, T), Buf, 4),
    int_to_byte_array_no_pad(maps:get(month, T), Buf, 7),
    int_to_byte_array_no_pad(maps:get(day, T), Buf, 10),
    int_to_byte_array_no_pad(maps:get(hour, T), Buf, 13),
    int_to_byte_array_no_pad(maps:get(minute, T), Buf, 16),
    int_to_byte_array_no_pad(maps:get(second, T), Buf, 19),
    '[]u8.bytestr'(Buf).

'Time.format_ss_milli'(T) ->
    Buf = [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo],
    % TODO: unhandled stmt type
    int_to_byte_array_no_pad(maps:get(year, T), Buf, 4),
    int_to_byte_array_no_pad(maps:get(month, T), Buf, 7),
    int_to_byte_array_no_pad(maps:get(day, T), Buf, 10),
    int_to_byte_array_no_pad(maps:get(hour, T), Buf, 13),
    int_to_byte_array_no_pad(maps:get(minute, T), Buf, 16),
    int_to_byte_array_no_pad(maps:get(second, T), Buf, 19),
    Millis = maps:get(nanosecond, T) div 1000000,
    int_to_byte_array_no_pad(Millis, Buf, 23),
    '[]u8.bytestr'(Buf).

'Time.format_ss_micro'(T) ->
    Buf = [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo],
    % TODO: unhandled stmt type
    int_to_byte_array_no_pad(maps:get(year, T), Buf, 4),
    int_to_byte_array_no_pad(maps:get(month, T), Buf, 7),
    int_to_byte_array_no_pad(maps:get(day, T), Buf, 10),
    int_to_byte_array_no_pad(maps:get(hour, T), Buf, 13),
    int_to_byte_array_no_pad(maps:get(minute, T), Buf, 16),
    int_to_byte_array_no_pad(maps:get(second, T), Buf, 19),
    Micros = maps:get(nanosecond, T) div 1000,
    int_to_byte_array_no_pad(Micros, Buf, 26),
    '[]u8.bytestr'(Buf).

'Time.format_ss_nano'(T) ->
    Buf = [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo],
    % TODO: unhandled stmt type
    int_to_byte_array_no_pad(maps:get(year, T), Buf, 4),
    int_to_byte_array_no_pad(maps:get(month, T), Buf, 7),
    int_to_byte_array_no_pad(maps:get(day, T), Buf, 10),
    int_to_byte_array_no_pad(maps:get(hour, T), Buf, 13),
    int_to_byte_array_no_pad(maps:get(minute, T), Buf, 16),
    int_to_byte_array_no_pad(maps:get(second, T), Buf, 19),
    int_to_byte_array_no_pad(maps:get(nanosecond, T), Buf, 29),
    '[]u8.bytestr'(Buf).

'Time.format_rfc3339'(T) ->
    Buf = [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo],
    % TODO: unhandled stmt type
    T_ = time_with_unix(T),
    case maps:get(is_local, T_) of
        true -> begin
            Utc_time = 'Time.local_to_utc'(T_),
            int_to_byte_array_no_pad(maps:get(year, Utc_time), Buf, 4),
            int_to_byte_array_no_pad(maps:get(month, Utc_time), Buf, 7),
            int_to_byte_array_no_pad(maps:get(day, Utc_time), Buf, 10),
            int_to_byte_array_no_pad(maps:get(hour, Utc_time), Buf, 13),
            int_to_byte_array_no_pad(maps:get(minute, Utc_time), Buf, 16),
            int_to_byte_array_no_pad(maps:get(second, Utc_time), Buf, 19),
            int_to_byte_array_no_pad(maps:get(nanosecond, Utc_time) div 1000000, Buf, 23)
        end;
        false -> begin
            int_to_byte_array_no_pad(maps:get(year, T_), Buf, 4),
            int_to_byte_array_no_pad(maps:get(month, T_), Buf, 7),
            int_to_byte_array_no_pad(maps:get(day, T_), Buf, 10),
            int_to_byte_array_no_pad(maps:get(hour, T_), Buf, 13),
            int_to_byte_array_no_pad(maps:get(minute, T_), Buf, 16),
            int_to_byte_array_no_pad(maps:get(second, T_), Buf, 19),
            int_to_byte_array_no_pad(maps:get(nanosecond, T_) div 1000000, Buf, 23)
        end
    end,
    '[]u8.bytestr'(Buf).

'Time.format_rfc3339_micro'(T) ->
    Buf = [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo],
    % TODO: unhandled stmt type
    T_ = time_with_unix(T),
    case maps:get(is_local, T_) of
        true -> begin
            Utc_time = 'Time.local_to_utc'(T_),
            int_to_byte_array_no_pad(maps:get(year, Utc_time), Buf, 4),
            int_to_byte_array_no_pad(maps:get(month, Utc_time), Buf, 7),
            int_to_byte_array_no_pad(maps:get(day, Utc_time), Buf, 10),
            int_to_byte_array_no_pad(maps:get(hour, Utc_time), Buf, 13),
            int_to_byte_array_no_pad(maps:get(minute, Utc_time), Buf, 16),
            int_to_byte_array_no_pad(maps:get(second, Utc_time), Buf, 19),
            int_to_byte_array_no_pad(maps:get(nanosecond, Utc_time) div 1000, Buf, 26)
        end;
        false -> begin
            int_to_byte_array_no_pad(maps:get(year, T_), Buf, 4),
            int_to_byte_array_no_pad(maps:get(month, T_), Buf, 7),
            int_to_byte_array_no_pad(maps:get(day, T_), Buf, 10),
            int_to_byte_array_no_pad(maps:get(hour, T_), Buf, 13),
            int_to_byte_array_no_pad(maps:get(minute, T_), Buf, 16),
            int_to_byte_array_no_pad(maps:get(second, T_), Buf, 19),
            int_to_byte_array_no_pad(maps:get(nanosecond, T_) div 1000, Buf, 26)
        end
    end,
    '[]u8.bytestr'(Buf).

'Time.format_rfc3339_nano'(T) ->
    Buf = [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo],
    % TODO: unhandled stmt type
    T_ = time_with_unix(T),
    case maps:get(is_local, T_) of
        true -> begin
            Utc_time = 'Time.local_to_utc'(T_),
            int_to_byte_array_no_pad(maps:get(year, Utc_time), Buf, 4),
            int_to_byte_array_no_pad(maps:get(month, Utc_time), Buf, 7),
            int_to_byte_array_no_pad(maps:get(day, Utc_time), Buf, 10),
            int_to_byte_array_no_pad(maps:get(hour, Utc_time), Buf, 13),
            int_to_byte_array_no_pad(maps:get(minute, Utc_time), Buf, 16),
            int_to_byte_array_no_pad(maps:get(second, Utc_time), Buf, 19),
            int_to_byte_array_no_pad(maps:get(nanosecond, Utc_time), Buf, 29)
        end;
        false -> begin
            int_to_byte_array_no_pad(maps:get(year, T_), Buf, 4),
            int_to_byte_array_no_pad(maps:get(month, T_), Buf, 7),
            int_to_byte_array_no_pad(maps:get(day, T_), Buf, 10),
            int_to_byte_array_no_pad(maps:get(hour, T_), Buf, 13),
            int_to_byte_array_no_pad(maps:get(minute, T_), Buf, 16),
            int_to_byte_array_no_pad(maps:get(second, T_), Buf, 19),
            int_to_byte_array_no_pad(maps:get(nanosecond, T_), Buf, 29)
        end
    end,
    '[]u8.bytestr'(Buf).

'Time.hhmm'(T) ->
    Buf = [todo, todo, todo, todo, todo],
    % TODO: unhandled stmt type
    int_to_byte_array_no_pad(maps:get(hour, T), Buf, 2),
    int_to_byte_array_no_pad(maps:get(minute, T), Buf, 5),
    '[]u8.bytestr'(Buf).

'Time.hhmmss'(T) ->
    Buf = [todo, todo, todo, todo, todo, todo, todo, todo],
    % TODO: unhandled stmt type
    int_to_byte_array_no_pad(maps:get(hour, T), Buf, 2),
    int_to_byte_array_no_pad(maps:get(minute, T), Buf, 5),
    int_to_byte_array_no_pad(maps:get(second, T), Buf, 8),
    '[]u8.bytestr'(Buf).

'Time.hhmm12'(T) ->
    'Time.get_fmt_time_str'(T, hhmm12).

'Time.ymmdd'(T) ->
    'Time.get_fmt_date_str'(T, hyphen, yyyymmdd).

'Time.ddmmy'(T) ->
    'Time.get_fmt_date_str'(T, dot, ddmmyyyy).

'Time.md'(T) ->
    'Time.get_fmt_date_str'(T, space, mmmd).

ordinal_suffix(N) ->
    case N > 3 andalso N < 21 of
        true -> <<(integer_to_binary(N))/binary, "th">>;
        false -> case N rem 10 of
            1 -> <<(integer_to_binary(N))/binary, "st">>;
            2 -> <<(integer_to_binary(N))/binary, "nd">>;
            3 -> <<(integer_to_binary(N))/binary, "rd">>;
            _ -> <<(integer_to_binary(N))/binary, "th">>
        end
        end.

'Time.custom_format'(T, S) ->
    Tokens = [],
    % TODO: unhandled stmt type
    Sb = new_builder(128),
    lists:foreach(fun(Token) ->
        case Token of
            <<"M">> -> 'Builder.write_string'(Sb, integer_to_binary(maps:get(month, T)));
            <<"MM">> -> 'Builder.write_string'(Sb, integer_to_binary(maps:get(month, T)));
            <<"Mo">> -> 'Builder.write_string'(Sb, ordinal_suffix(maps:get(month, T)));
            <<"MMM">> -> 'Builder.write_string'(Sb, lists:nth(todo + 1, lists:nth(iclamp(0, maps:get(month, T) - 1, 11) + 1, [<<"January">>, <<"February">>, <<"March">>, <<"April">>, <<"May">>, <<"June">>, <<"July">>, <<"August">>, <<"September">>, <<"October">>, <<"November">>, <<"December">>])));
            <<"MMMM">> -> 'Builder.write_string'(Sb, lists:nth(iclamp(0, maps:get(month, T) - 1, 11) + 1, [<<"January">>, <<"February">>, <<"March">>, <<"April">>, <<"May">>, <<"June">>, <<"July">>, <<"August">>, <<"September">>, <<"October">>, <<"November">>, <<"December">>]));
            <<"D">> -> 'Builder.write_string'(Sb, integer_to_binary(maps:get(day, T)));
            <<"DD">> -> 'Builder.write_string'(Sb, integer_to_binary(maps:get(day, T)));
            <<"Do">> -> 'Builder.write_string'(Sb, ordinal_suffix(maps:get(day, T)));
            <<"DDD">> -> 'Builder.write_string'(Sb, integer_to_binary(('Time.year_day'(T))));
            <<"DDDD">> -> 'Builder.write_string'(Sb, integer_to_binary('Time.year_day'(T)));
            <<"DDDo">> -> 'Builder.write_string'(Sb, ordinal_suffix('Time.year_day'(T)));
            <<"d">> -> 'Builder.write_string'(Sb, integer_to_binary('Time.day_of_week'(T) rem 7));
            <<"dd">> -> 'Builder.write_string'(Sb, lists:nth(todo + 1, lists:nth(iclamp(0, 'Time.day_of_week'(T) - 1, 6) + 1, [<<"Monday">>, <<"Tuesday">>, <<"Wednesday">>, <<"Thursday">>, <<"Friday">>, <<"Saturday">>, <<"Sunday">>])));
            <<"ddd">> -> 'Builder.write_string'(Sb, lists:nth(todo + 1, lists:nth(iclamp(0, 'Time.day_of_week'(T) - 1, 6) + 1, [<<"Monday">>, <<"Tuesday">>, <<"Wednesday">>, <<"Thursday">>, <<"Friday">>, <<"Saturday">>, <<"Sunday">>])));
            <<"dddd">> -> 'Builder.write_string'(Sb, lists:nth(iclamp(0, 'Time.day_of_week'(T) - 1, 6) + 1, [<<"Monday">>, <<"Tuesday">>, <<"Wednesday">>, <<"Thursday">>, <<"Friday">>, <<"Saturday">>, <<"Sunday">>]));
            <<"YY">> -> 'Builder.write_string'(Sb, lists:nth(todo + 1, integer_to_binary(maps:get(year, T))));
            <<"YYYY">> -> 'Builder.write_string'(Sb, integer_to_binary(maps:get(year, T)));
            <<"H">> -> 'Builder.write_string'(Sb, integer_to_binary(maps:get(hour, T)));
            <<"HH">> -> 'Builder.write_string'(Sb, integer_to_binary(maps:get(hour, T)));
            <<"h">> -> begin
                H = (maps:get(hour, T) + 11) rem 12 + 1,
                'Builder.write_string'(Sb, integer_to_binary(H))
            end;
            <<"hh">> -> begin
                H1 = (maps:get(hour, T) + 11) rem 12 + 1,
                'Builder.write_string'(Sb, integer_to_binary(H1))
            end;
            <<"i">> -> begin
                H2 = case maps:get(hour, T) > 12 of
                    true -> maps:get(hour, T) - 12;
                    false -> maps:get(hour, T)
                end,
                'Builder.write_string'(Sb, integer_to_binary(H2))
            end;
            <<"ii">> -> begin
                H3 = case maps:get(hour, T) > 12 of
                    true -> maps:get(hour, T) - 12;
                    false -> maps:get(hour, T)
                end,
                'Builder.write_string'(Sb, integer_to_binary(H3))
            end;
            <<"m">> -> 'Builder.write_string'(Sb, integer_to_binary(maps:get(minute, T)));
            <<"mm">> -> 'Builder.write_string'(Sb, integer_to_binary(maps:get(minute, T)));
            <<"s">> -> 'Builder.write_string'(Sb, integer_to_binary(maps:get(second, T)));
            <<"ss">> -> 'Builder.write_string'(Sb, integer_to_binary(maps:get(second, T)));
            <<"k">> -> 'Builder.write_string'(Sb, integer_to_binary((maps:get(hour, T) + 1)));
            <<"kk">> -> 'Builder.write_string'(Sb, integer_to_binary((maps:get(hour, T) + 1)));
            <<"w">> -> 'Builder.write_string'(Sb, integer_to_binary('Time.week_of_year'(T)));
            <<"ww">> -> 'Builder.write_string'(Sb, integer_to_binary('Time.week_of_year'(T)));
            <<"wo">> -> 'Builder.write_string'(Sb, ordinal_suffix('Time.week_of_year'(T)));
            <<"Q">> -> 'Builder.write_string'(Sb, integer_to_binary((maps:get(month, T) - 1) div 3 + 1));
            <<"QQ">> -> 'Builder.write_string'(Sb, integer_to_binary((maps:get(month, T) - 1) div 3 + 1));
            <<"Qo">> -> 'Builder.write_string'(Sb, ordinal_suffix((maps:get(month, T) - 1) div 3 + 1));
            <<"c">> -> 'Builder.write_string'(Sb, integer_to_binary('Time.day_of_week'(T)));
            <<"N">> -> 'Builder.write_string'(Sb, <<"AD">>);
            <<"NN">> -> 'Builder.write_string'(Sb, <<"Anno Domini">>);
            <<"Z">> -> begin
                Hours = offset() div 60 * 60,
                case Hours >= 0 of
                    true -> 'Builder.write_string'(Sb, <<"+", (integer_to_binary(Hours))/binary>>);
                    false -> begin
                        Hours1 = -Hours,
                        'Builder.write_string'(Sb, <<"-", (integer_to_binary(Hours1))/binary>>)
                    end
                end
            end;
            <<"ZZ">> -> begin
                Hours2 = offset() div 60 * 60,
                case Hours2 >= 0 of
                    true -> 'Builder.write_string'(Sb, <<"+", (integer_to_binary(Hours2))/binary, "00">>);
                    false -> begin
                        Hours3 = -Hours2,
                        'Builder.write_string'(Sb, <<"-", (integer_to_binary(Hours3))/binary, "00">>)
                    end
                end
            end;
            <<"ZZZ">> -> begin
                Hours4 = offset() div 60 * 60,
                case Hours4 >= 0 of
                    true -> 'Builder.write_string'(Sb, <<"+", (integer_to_binary(Hours4))/binary, ":00">>);
                    false -> begin
                        Hours5 = -Hours4,
                        'Builder.write_string'(Sb, <<"-", (integer_to_binary(Hours5))/binary, ":00">>)
                    end
                end
            end;
            <<"a">> -> case maps:get(hour, T) < 12 of
                true -> 'Builder.write_string'(Sb, <<"am">>);
                false -> 'Builder.write_string'(Sb, <<"pm">>)
            end;
            <<"A">> -> case maps:get(hour, T) < 12 of
                true -> 'Builder.write_string'(Sb, <<"AM">>);
                false -> 'Builder.write_string'(Sb, <<"PM">>)
            end;
            _ -> 'Builder.write_string'(Sb, Token)
        end,
        ok
    end, Tokens),
    'Builder.str'(Sb).

'Time.clean'(T) ->
    Znow = now(),
    case maps:get(month, T) == maps:get(month, Znow) andalso maps:get(year, T) == maps:get(year, Znow) andalso maps:get(day, T) == maps:get(day, Znow) of
        true -> 'Time.get_fmt_time_str'(T, hhmm24);
        false -> 
            case maps:get(year, T) == maps:get(year, Znow) of
                true -> 'Time.get_fmt_str'(T, space, hhmm24, mmmd);
                false -> 'Time.format'(T)
                        end
                end.

'Time.clean12'(T) ->
    Znow = now(),
    case maps:get(month, T) == maps:get(month, Znow) andalso maps:get(year, T) == maps:get(year, Znow) andalso maps:get(day, T) == maps:get(day, Znow) of
        true -> 'Time.get_fmt_time_str'(T, hhmm12);
        false -> 
            case maps:get(year, T) == maps:get(year, Znow) of
                true -> 'Time.get_fmt_str'(T, space, hhmm12, mmmd);
                false -> 'Time.format'(T)
                        end
                end.

'Time.get_fmt_time_str'(T, Fmt_time) ->
    case Fmt_time == no_time of
        true -> <<"">>;
        false -> begin
            Tp = case maps:get(hour, T) > 11 of
                true -> <<"p.m.">>;
                false -> <<"a.m.">>
            end,
            Hour_ = case maps:get(hour, T) > 12 of
                true -> maps:get(hour, T) - 12;
                false -> case maps:get(hour, T) == 0 of
                    true -> 12;
                    false -> maps:get(hour, T)
                end
            end,
            case Fmt_time of
                hhmm12 -> <<(integer_to_binary(Hour_))/binary, ":", (integer_to_binary(maps:get(minute, T)))/binary, " ", (Tp)/binary>>;
                hhmm24 -> <<(integer_to_binary(maps:get(hour, T)))/binary, ":", (integer_to_binary(maps:get(minute, T)))/binary>>;
                hhmmss12 -> <<(integer_to_binary(Hour_))/binary, ":", (integer_to_binary(maps:get(minute, T)))/binary, ":", (integer_to_binary(maps:get(second, T)))/binary, " ", (Tp)/binary>>;
                hhmmss24 -> <<(integer_to_binary(maps:get(hour, T)))/binary, ":", (integer_to_binary(maps:get(minute, T)))/binary, ":", (integer_to_binary(maps:get(second, T)))/binary>>;
                hhmmss24_milli -> <<(integer_to_binary(maps:get(hour, T)))/binary, ":", (integer_to_binary(maps:get(minute, T)))/binary, ":", (integer_to_binary(maps:get(second, T)))/binary, ".", (integer_to_binary((maps:get(nanosecond, T) div 1000000)))/binary>>;
                hhmmss24_micro -> <<(integer_to_binary(maps:get(hour, T)))/binary, ":", (integer_to_binary(maps:get(minute, T)))/binary, ":", (integer_to_binary(maps:get(second, T)))/binary, ".", (integer_to_binary((maps:get(nanosecond, T) div 1000)))/binary>>;
                hhmmss24_nano -> <<(integer_to_binary(maps:get(hour, T)))/binary, ":", (integer_to_binary(maps:get(minute, T)))/binary, ":", (integer_to_binary(maps:get(second, T)))/binary, ".", (integer_to_binary(maps:get(nanosecond, T)))/binary>>;
                _ -> <<"unknown enumeration ", (Fmt_time)/binary>>
            end
        end
        end.

'Time.get_fmt_date_str'(T, Fmt_dlmtr, Fmt_date) ->
    case Fmt_date == no_date of
        true -> <<"">>;
        false -> begin
            Month = 'Time.smonth'(T),
            Year = integer_to_binary((maps:get(year, T) rem 100)),
            Res = case Fmt_date of
                ddmmyy -> <<(integer_to_binary(maps:get(day, T)))/binary, "|", (integer_to_binary(maps:get(month, T)))/binary, "|", (Year)/binary>>;
                ddmmyyyy -> <<(integer_to_binary(maps:get(day, T)))/binary, "|", (integer_to_binary(maps:get(month, T)))/binary, "|", (integer_to_binary(maps:get(year, T)))/binary>>;
                mmddyy -> <<(integer_to_binary(maps:get(month, T)))/binary, "|", (integer_to_binary(maps:get(day, T)))/binary, "|", (Year)/binary>>;
                mmddyyyy -> <<(integer_to_binary(maps:get(month, T)))/binary, "|", (integer_to_binary(maps:get(day, T)))/binary, "|", (integer_to_binary(maps:get(year, T)))/binary>>;
                mmmd -> <<(Month)/binary, "|", (integer_to_binary(maps:get(day, T)))/binary>>;
                mmmdd -> <<(Month)/binary, "|", (integer_to_binary(maps:get(day, T)))/binary>>;
                mmmddyy -> <<(Month)/binary, "|", (integer_to_binary(maps:get(day, T)))/binary, "|", (Year)/binary>>;
                mmmddyyyy -> <<(Month)/binary, "|", (integer_to_binary(maps:get(day, T)))/binary, "|", (integer_to_binary(maps:get(year, T)))/binary>>;
                yyyymmdd -> <<(integer_to_binary(maps:get(year, T)))/binary, "|", (integer_to_binary(maps:get(month, T)))/binary, "|", (integer_to_binary(maps:get(day, T)))/binary>>;
                yymmdd -> <<(Year)/binary, "|", (integer_to_binary(maps:get(month, T)))/binary, "|", (integer_to_binary(maps:get(day, T)))/binary>>;
                _ -> <<"unknown enumeration ", (Fmt_date)/binary>>
            end,
            Del = case Fmt_dlmtr of
                dot -> <<".">>;
                hyphen -> <<"-">>;
                slash -> <<"/">>;
                space -> <<" ">>;
                no_delimiter -> <<"">>
            end,
            Res1 = binary:replace(Res, <<"|">>, Del, [global]),
            Res1
        end
        end.

'Time.get_fmt_str'(T, Fmt_dlmtr, Fmt_time, Fmt_date) ->
    case Fmt_date == no_date of
        true -> case Fmt_time == no_time of
            true -> <<"">>;
            false -> 'Time.get_fmt_time_str'(T, Fmt_time)
        end;
        false -> case Fmt_time /= no_time of
            true -> begin
                Dstr = 'Time.get_fmt_date_str'(T, Fmt_dlmtr, Fmt_date),
                Tstr = 'Time.get_fmt_time_str'(T, Fmt_time),
                <<(Dstr)/binary, " ", (Tstr)/binary>>
            end;
            false -> 'Time.get_fmt_date_str'(T, Fmt_dlmtr, Fmt_date)
        end
    end.

'Time.utc_string'(T) ->
    Day_str = 'Time.weekday_str'(T),
    Month_str = 'Time.smonth'(T),
    Utc_string = <<(Day_str)/binary, ", ", (integer_to_binary(maps:get(day, T)))/binary, " ", (Month_str)/binary, " ", (integer_to_binary(maps:get(year, T)))/binary, " ", (integer_to_binary(maps:get(hour, T)))/binary, ":", (integer_to_binary(maps:get(minute, T)))/binary, ":", (integer_to_binary(maps:get(second, T)))/binary, " UTC">>,
    Utc_string.

'Time.http_header_string'(T) ->
    Buf = [],
    'Time.push_to_http_header'(T, Buf),
    '[]u8.bytestr'(Buf).

'Time.push_to_http_header'(T, Buffer) ->
    Day_str = 'Time.weekday_str'(T),
    Month_str = 'Time.smonth'(T),
    Buf = [lists:nth(1, Day_str), lists:nth(2, Day_str), lists:nth(3, Day_str), todo, todo, todo, todo, todo, lists:nth(1, Month_str), lists:nth(2, Month_str), lists:nth(3, Month_str), todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo],
    % TODO: unhandled stmt type
    todo,
    ok.

mceil(X) ->
    case X > 0 of
        true -> 1 + todo;
        false -> 
            case X < 0 of
                true -> -todo;
                false -> 0
                        end
                end.

'Time.=='(T1, T2) ->
    maps:get(nanosecond, T1) == maps:get(nanosecond, T2) andalso maps:get(is_local, T1) == maps:get(is_local, T2) andalso 'Time.local_unix'(T1) == 'Time.local_unix'(T2).

'Time.<'(T1, T2) ->
    T1u = 'Time.unix'(T1),
    T2u = 'Time.unix'(T2),
    T1u < T2u orelse (T1u == T2u andalso maps:get(nanosecond, T1) < maps:get(nanosecond, T2)).

'Time.-'(Lhs, Rhs) ->
    Unixs = todo * todo,
    Nanos = maps:get(nanosecond, Lhs) - maps:get(nanosecond, Rhs),
    Unixs + Nanos.

'TimeParseError.msg'(Err) ->
    <<"Invalid time format code: ", (integer_to_binary(maps:get(code, Err)))/binary, ", error: ", (maps:get(message, Err))/binary>>.

error_invalid_time(Code, Message) ->
    #{code => Code, message => Message, {vbeam, type} => 'TimeParseError'}.

new_stopwatch(Opts) ->
    Initial = todo,
    case maps:get(auto_start, Opts) of
        true -> ok;
        false -> ok
    end,
    #{elapsed => 0, start => Initial, end => 0, {vbeam, type} => 'StopWatch'}.

'StopWatch.start'(T) ->

'StopWatch.restart'(T) ->

'StopWatch.stop'(T) ->

'StopWatch.pause'(T) ->
    case maps:get(start, T) > 0 of
        true -> case maps:get(end, T) == 0 of
            true -> ok;
            false -> ok
        end;
        false -> ok
    end,

'StopWatch.elapsed'(T) ->
    case maps:get(start, T) > 0 of
        true -> case maps:get(end, T) == 0 of
            true -> todo;
            false -> todo
        end;
        false -> ok
    end,
    todo.

now() ->
    #{is_local => true, {vbeam, type} => 'Time'}.

utc() ->
    #{{vbeam, type} => 'Time'}.

'Time.local'(T) ->
    case maps:get(is_local, T) of
        true -> T;
        false -> #{is_local => true, {vbeam, type} => 'Time'}
        end.

sleep(Duration) ->
    ok.

ticks() ->
    0.

sys_mono_now() ->
    0.

time_with_unix(T) ->
    case maps:get(unix, T) /= 0 of
        true -> T;
        false -> #{unix => calculate_unix_time(maps:get(year, T), maps:get(month, T), maps:get(day, T), maps:get(hour, T), maps:get(minute, T), maps:get(second, T)), {vbeam, type} => 'Time'}
        end.

calculate_unix_time(Year, Month, Day, Hour, Minute, Second) ->
    Y = Year,
    M = Month,
    case M =< 2 of
        true -> begin
            Y1 = 1,
            M1 = 12,
        end;
        false -> ok
    end,
    Days = 365 * (Y1 - 1970) + (Y1 - 1969) div 4 - (Y1 - 1901) div 100 + (Y1 - 1601) div 400,
    Days_before_month = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334],
    Day_of_year = lists:nth(Month - 1 + 1, Days_before_month) + Day,
    case Month > 2 andalso is_leap_year(Year) of
        true -> ok;
        false -> ok
    end,
    Total_days = Days + Day_of_year - 1,
    todo * 86400 + todo * 3600 + todo * 60 + todo.

'Time.strftime'(T, Fmt) ->
    <<"">>.

parse_iso8601(S) ->
    case S == <<"">> of
        true -> error(<<"datetime string is empty">>);
        false -> #{{vbeam, type} => 'Time'}
        end.

parse_rfc3339(S) ->
    case S == <<"">> of
        true -> error(<<"datetime string is empty">>);
        false -> #{{vbeam, type} => 'Time'}
        end.

parse(S) ->
    case S == <<"">> of
        true -> error_invalid_time(0, <<"datetime string is empty">>);
        false -> begin
            Pos = 'string.index'(S, <<" ">>),
            Symd = lists:nth(todo + 1, S),
            Ymd = binary:split(Symd, <<"-">>, [global]),
            case length(Ymd) /= 3 of
                true -> error_invalid_time(2, <<"date must be in the form of y-m-d">>);
                false -> begin
                    Shms = lists:nth(todo + 1, S),
                    Hms = binary:split(Shms, <<":">>, [global]),
                    case length(Hms) /= 3 of
                        true -> error_invalid_time(9, <<"time must be in the form of H:i:s">>);
                        false -> begin
                            Hour_ = lists:nth(todo + 1, lists:nth(1, Hms)),
                            Minute_ = lists:nth(2, Hms),
                            Second_ = lists:nth(3, Hms),
                            Iyear = atoi(lists:nth(1, Ymd)),
                            Imonth = atoi(lists:nth(2, Ymd)),
                            Iday = atoi(lists:nth(3, Ymd)),
                            Ihour = atoi(Hour_),
                            Iminute = atoi(Minute_),
                            Isecond = atoi(Second_),
                            case Iyear > 9999 orelse Iyear < -9999 of
                                true -> error_invalid_time(3, <<"year must be between -10000 and 10000">>);
                                false -> 
                                    case Imonth > 12 orelse Imonth < 1 of
                                        true -> error_invalid_time(4, <<"month must be between 1 and 12">>);
                                        false -> 
                                            case Iday > 31 orelse Iday < 1 of
                                                true -> error_invalid_time(5, <<"day must be between 1 and 31">>);
                                                false -> 
                                                    case Ihour > 23 orelse Ihour < 0 of
                                                        true -> error_invalid_time(6, <<"hours must be between 0 and 24">>);
                                                        false -> 
                                                            case Iminute > 59 orelse Iminute < 0 of
                                                                true -> error_invalid_time(7, <<"minutes must be between 0 and 60">>);
                                                                false -> 
                                                                    case Isecond > 59 orelse Isecond < 0 of
                                                                        true -> error_invalid_time(8, <<"seconds must be between 0 and 60">>);
                                                                        false -> begin
                                                                            Res = new(#{year => Iyear, month => Imonth, day => Iday, hour => Ihour, minute => Iminute, second => Isecond, {vbeam, type} => 'Time'}),
                                                                            Res
                                                                        end
                                                                                                                                        end
                                                                                                                                                                                        end
                                                                                                                                                                end
                                                                                                                                        end
                                                                                                                end
                                                                                        end
                        end
                                        end
                end
                        end
        end
        end.

parse_format(S, Format) ->
    case S == <<"">> of
        true -> error_invalid_time(0, <<"datetime string is empty">>);
        false -> #{{vbeam, type} => 'Time'}
        end.

'Time__static__new'(T) ->
    time_with_unix(T).

new(T) ->
    time_with_unix(T).

'Time.smonth'(T) ->
    case maps:get(month, T) =< 0 orelse maps:get(month, T) > 12 of
        true -> <<"---">>;
        false -> begin
            I = maps:get(month, T) - 1,
            lists:nth(todo + 1, <<"JanFebMarAprMayJunJulAugSepOctNovDec">>)
        end
        end.

'Time.unix'(T) ->
    maps:get(unix, time_with_unix('Time.local_to_utc'(T))).

'Time.local_unix'(T) ->
    maps:get(unix, time_with_unix(T)).

'Time.unix_milli'(T) ->
    'Time.unix'(T) * 1000 + (todo div 1000000).

'Time.unix_micro'(T) ->
    'Time.unix'(T) * 1000000 + (todo div 1000).

'Time.unix_nano'(T) ->
    'Time.unix'(T) * 1000000000 + todo.

'Time.add'(T, Duration_in_nanosecond) ->
    Increased_time_nanosecond = todo + 'Duration.nanoseconds'(Duration_in_nanosecond),
    Increased_time_second = 'Time.local_unix'(T) + (Increased_time_nanosecond / todo),
    Increased_time_nanosecond1 = Increased_time_nanosecond rem todo,
    case Increased_time_nanosecond1 < 0 of
        true -> begin
            todo,
            Increased_time_nanosecond2 = todo,
        end;
        false -> ok
    end,
    Res = unix_nanosecond(Increased_time_second, todo),
    case maps:get(is_local, T) of
        true -> #{is_local => true, unix => 0, {vbeam, type} => 'Time'};
        false -> Res
        end.

'Time.add_seconds'(T, Seconds) ->
    'Time.add'(time_with_unix(T), todo * todo).

'Time.add_days'(T, Days) ->
    'Time.add'(time_with_unix(T), todo * 24 * todo).

since(T) ->
    now() - T.

'Time.relative'(T) ->
    Znow = now(),
    Secs = 'Time.unix'(Znow) - 'Time.unix'(T),
    Prefix = <<"">>,
    Suffix = <<"">>,
    case Secs < 0 of
        true -> begin
            Secs1 = -1,
            Prefix1 = <<"in ">>,
        end;
        false -> ok
    end,
    case Secs1 < 30 of
        true -> <<"now">>;
        false -> 
            case Secs1 < 3600 of
                true -> <<(Prefix1)/binary, (integer_to_binary(M))/binary, " minutes", (Suffix)/binary>>;
                false -> 
                    case Secs1 < 86400 of
                        true -> <<(Prefix1)/binary, (integer_to_binary(H))/binary, " hours", (Suffix)/binary>>;
                        false -> 
                            case Secs1 < 604800 of
                                true -> <<(Prefix1)/binary, (integer_to_binary(D))/binary, " days", (Suffix)/binary>>;
                                false -> 
                                    case Secs1 < 31536000 of
                                        true -> <<"last ", ('Time.md'(T))/binary>>;
                                        false -> begin
                                            Y = Secs1 div 60 * 60 div 24 div 365,
                                            case Y == 1 of
                                                true -> <<(Prefix1)/binary, "1 year", (Suffix)/binary>>;
                                                false -> <<(Prefix1)/binary, (integer_to_binary(Y))/binary, " years", (Suffix)/binary>>
                                                                                        end
                                        end
                                                                        end
                                                                                        end
                                                                end
                                        end
                end.

'Time.relative_short'(T) ->
    Znow = now(),
    Secs = 'Time.unix'(Znow) - 'Time.unix'(T),
    Prefix = <<"">>,
    Suffix = <<"">>,
    case Secs < 0 of
        true -> begin
            Secs1 = -1,
            Prefix1 = <<"in ">>,
        end;
        false -> ok
    end,
    case Secs1 < 30 of
        true -> <<"now">>;
        false -> 
            case Secs1 < 3600 of
                true -> <<(Prefix1)/binary, (integer_to_binary(M))/binary, "m", (Suffix)/binary>>;
                false -> 
                    case Secs1 < 86400 of
                        true -> <<(Prefix1)/binary, (integer_to_binary(H))/binary, "h", (Suffix)/binary>>;
                        false -> 
                            case Secs1 < 31536000 of
                                true -> <<(Prefix1)/binary, (integer_to_binary(D))/binary, "d", (Suffix)/binary>>;
                                false -> begin
                                    Y = Secs1 div 60 * 60 div 24 div 365,
                                    case Y == 1 of
                                        true -> <<(Prefix1)/binary, "1y", (Suffix)/binary>>;
                                        false -> <<(Prefix1)/binary, (integer_to_binary(Y))/binary, "y", (Suffix)/binary>>
                                                                        end
                                end
                                                        end
                                                                end
                                        end
                end.

day_of_week(Y, M, D) ->
    T = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4],
    Sy = Y,
    case M < 3 of
        true -> ok;
        false -> ok
    end,
    (Sy + Sy div 4 - Sy div 100 + Sy div 400 + lists:nth(iclamp(0, M - 1, 11) + 1, T) + D - 1) rem 7 + 1.

'Time.day_of_week'(T) ->
    day_of_week(maps:get(year, T), maps:get(month, T), maps:get(day, T)).

'Time.week_of_year'(T) ->
    Day_of_week = 'Time.day_of_week'(T),
    Days_to_thursday = 4 - Day_of_week,
    Thursday_date = 'Time.add_days'(T, Days_to_thursday),
    Thursday_day_of_year = 'Time.year_day'(Thursday_date),
    Week_number = (Thursday_day_of_year - 1) div 7 + 1,
    Week_number.

'Time.year_day'(T) ->
    Yday = maps:get(day, T) + lists:nth(iclamp(0, maps:get(month, T) - 1, 12) + 1, [0, 31, 31 + 28, 31 + 28 + 31, 31 + 28 + 31 + 30, 31 + 28 + 31 + 30 + 31, 31 + 28 + 31 + 30 + 31 + 30, 31 + 28 + 31 + 30 + 31 + 30 + 31, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31]),
    case is_leap_year(maps:get(year, T)) andalso maps:get(month, T) > 2 of
        true -> Yday + 1;
        false -> Yday
        end.

'Time.weekday_str'(T) ->
    I = 'Time.day_of_week'(T) - 1,
    lists:nth(todo + 1, lists:nth(iclamp(0, I, 6) + 1, [<<"Monday">>, <<"Tuesday">>, <<"Wednesday">>, <<"Thursday">>, <<"Friday">>, <<"Saturday">>, <<"Sunday">>])).

'Time.long_weekday_str'(T) ->
    I = 'Time.day_of_week'(T) - 1,
    lists:nth(iclamp(0, I, 6) + 1, [<<"Monday">>, <<"Tuesday">>, <<"Wednesday">>, <<"Thursday">>, <<"Friday">>, <<"Saturday">>, <<"Sunday">>]).

is_leap_year(Year) ->
    Year rem 4 == 0 andalso (Year rem 100 /= 0 orelse Year rem 400 == 0).

days_in_month(Month, Year) ->
    case Month > 12 orelse Month < 1 of
        true -> error(<<"Invalid month: ", (integer_to_binary(Month))/binary>>);
        false -> begin
            Extra = case Month == 2 andalso is_leap_year(Year) of
                true -> 1;
                false -> 0
            end,
            Res = lists:nth(Month - 1 + 1, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + Extra,
            Res
        end
        end.

'Time.debug'(T) ->
    <<"Time{ year: ", (integer_to_binary(maps:get(year, T)))/binary, " month: ", (integer_to_binary(maps:get(month, T)))/binary, " day: ", (integer_to_binary(maps:get(day, T)))/binary, " hour: ", (integer_to_binary(maps:get(hour, T)))/binary, " minute: ", (integer_to_binary(maps:get(minute, T)))/binary, " second: ", (integer_to_binary(maps:get(second, T)))/binary, " nanosecond: ", (integer_to_binary(maps:get(nanosecond, T)))/binary, " unix: ", (integer_to_binary(maps:get(unix, T)))/binary, " is_local: ", (atom_to_binary(maps:get(is_local, T)))/binary, " }">>.

offset() ->
    T = utc(),
    Local = 'Time.local'(T),
    todo.

'Time.local_to_utc'(T) ->
    case not maps:get(is_local, T) of
        true -> T;
        false -> #{is_local => false, {vbeam, type} => 'Time'}
        end.

'Time.utc_to_local'(U) ->
    case maps:get(is_local, U) of
        true -> U;
        false -> #{is_local => true, {vbeam, type} => 'Time'}
        end.

'Time.as_local'(T) ->
    #{is_local => true, {vbeam, type} => 'Time'}.

'Time.as_utc'(T) ->
    #{is_local => false, {vbeam, type} => 'Time'}.

'Time.is_utc'(T) ->
    not maps:get(is_local, T).

unix(Epoch) ->
    unix_nanosecond(Epoch, 0).

unix_milli(Ms) ->
    ts_to_time_impl(Ms, 1000, 1000000).

unix_micro(Us) ->
    ts_to_time_impl(Us, 1000000, 1000).

unix_nano(Ns) ->
    ts_to_time_impl(Ns, 1000000000, 1).

ts_to_time_impl(Value, Down, Up) ->
    Epoch = Value div Down,
    Remainder = (Value rem Down) * Up,
    unix_nanosecond(Epoch, todo).

unix_microsecond(Epoch, Microsecond) ->
    unix_nanosecond(Epoch, Microsecond * 1000).

unix_nanosecond(Abs_unix_timestamp, Nanosecond) ->
    Day_offset = Abs_unix_timestamp div 24 * 3600,
    case Abs_unix_timestamp rem 86400 < 0 of
        true -> todo;
        false -> ok
    end,
    Year = element(1, calculate_date_from_day_offset(Day_offset)),
    Month = element(2, calculate_date_from_day_offset(Day_offset)),
    Day = element(3, calculate_date_from_day_offset(Day_offset)),
    Hour_ = element(1, calculate_time_from_second_offset(Abs_unix_timestamp rem 24 * 3600)),
    Minute_ = element(2, calculate_time_from_second_offset(Abs_unix_timestamp rem 24 * 3600)),
    Second_ = element(3, calculate_time_from_second_offset(Abs_unix_timestamp rem 24 * 3600)),
    #{year => Year, month => Month, day => Day, hour => Hour_, minute => Minute_, second => Second_, nanosecond => Nanosecond, unix => Abs_unix_timestamp, {vbeam, type} => 'Time'}.

calculate_date_from_day_offset(Day_offset_) ->
    Day_offset = Day_offset_,
    Day_offset1 = 719468,
    Era = 0,
    case Day_offset1 >= 0 of
        true -> ok;
        false -> ok
    end,
    Day_of_era = Day_offset1 - Era * 365 * 400 + 97,
    Year_of_era = (Day_of_era - Day_of_era div (365 * 4 + 1 - 1) + Day_of_era div 365 * 100 + 24 - Day_of_era div (365 * 400 + 97 - 1)) div 365,
    Year = todo,
    Day_of_year = Day_of_era - (365 * Year_of_era + Year_of_era div 4 - Year_of_era div 100),
    Month_position = (5 * Day_of_year + 2) div 153,
    Day = todo,
    Month = todo,
    case Month_position < 10 of
        true -> ok;
        false -> ok
    end,
    case Month =< 2 of
        true -> ok;
        false -> ok
    end,
    Year.

calculate_time_from_second_offset(Second_offset_) ->
    Second_offset = Second_offset_,
    case Second_offset < 0 of
        true -> ok;
        false -> ok
    end,
    Hour_ = Second_offset div 60 * 60,
    Second_offset1 = 60 * 60,
    Minute_ = Second_offset1 div 60,
    Second_offset2 = 60,
    todo.

'FormatTime__static__from'(Input) ->
    error(<<"invalid value">>).

'FormatDate__static__from'(Input) ->
    error(<<"invalid value">>).

'FormatDelimiter__static__from'(Input) ->
    error(<<"invalid value">>).
