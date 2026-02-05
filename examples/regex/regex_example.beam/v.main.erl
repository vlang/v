-module('v.main').
-export([convert_html_rgb/1, convert_html_rgb_n/1, main/0]).

convert_html_rgb(In_col) ->
    N_digit = case length(In_col) == 4 of
        true -> 1;
        false -> 2
    end,
    Col_mul = case length(In_col) == 4 of
        true -> 4;
        false -> 0
    end,
    Query = <<"#([a-fA-F0-9]{", (integer_to_binary(N_digit))/binary, "})([a-fA-F0-9]{", (integer_to_binary(N_digit))/binary, "})([a-fA-F0-9]{", (integer_to_binary(N_digit))/binary, "})">>,
    Re = regex_opt(Query),
    Start = element(1, 'RE.match_string'(Re, In_col)),
    End = element(2, 'RE.match_string'(Re, In_col)),
    vbeam_io:println(<<"start: ", (integer_to_binary(Start))/binary, ", end: ", (integer_to_binary(End))/binary>>),
    Res = todo,
    case Start >= 0 of
        true -> begin
            Group_list = 'RE.get_group_list'(Re),
            R = 'string.u32'((<<"0x">> + lists:nth(todo + 1, In_col))) << Col_mul,
            G = 'string.u32'((<<"0x">> + lists:nth(todo + 1, In_col))) << Col_mul,
            B = 'string.u32'((<<"0x">> + lists:nth(todo + 1, In_col))) << Col_mul,
            vbeam_io:println(<<"r: ", (integer_to_binary(R))/binary, " g: ", (integer_to_binary(G))/binary, " b: ", (integer_to_binary(B))/binary>>),
            Res1 = R << 16 | G << 8 | B,
        end;
        false -> ok
    end,
    Res1.

convert_html_rgb_n(In_col) ->
    N_digit = case length(In_col) == 4 of
        true -> 1;
        false -> 2
    end,
    Col_mul = case length(In_col) == 4 of
        true -> 4;
        false -> 0
    end,
    Query = <<"#(?P<red>[a-fA-F0-9]{", (integer_to_binary(N_digit))/binary, "})(?P<green>[a-fA-F0-9]{", (integer_to_binary(N_digit))/binary, "})(?P<blue>[a-fA-F0-9]{", (integer_to_binary(N_digit))/binary, "})">>,
    Re = regex_opt(Query),
    Start = element(1, 'RE.match_string'(Re, In_col)),
    End = element(2, 'RE.match_string'(Re, In_col)),
    vbeam_io:println(<<"start: ", (integer_to_binary(Start))/binary, ", end: ", (integer_to_binary(End))/binary>>),
    Res = todo,
    case Start >= 0 of
        true -> begin
            Red_s = element(1, 'RE.get_group_bounds_by_name'(Re, <<"red">>)),
            Red_e = element(2, 'RE.get_group_bounds_by_name'(Re, <<"red">>)),
            R = 'string.u32'((<<"0x">> + lists:nth(todo + 1, In_col))) << Col_mul,
            Green_s = element(1, 'RE.get_group_bounds_by_name'(Re, <<"green">>)),
            Green_e = element(2, 'RE.get_group_bounds_by_name'(Re, <<"green">>)),
            G = 'string.u32'((<<"0x">> + lists:nth(todo + 1, In_col))) << Col_mul,
            Blue_s = element(1, 'RE.get_group_bounds_by_name'(Re, <<"blue">>)),
            Blue_e = element(2, 'RE.get_group_bounds_by_name'(Re, <<"blue">>)),
            B = 'string.u32'((<<"0x">> + lists:nth(todo + 1, In_col))) << Col_mul,
            vbeam_io:println(<<"r: ", (integer_to_binary(R))/binary, " g: ", (integer_to_binary(G))/binary, " b: ", (integer_to_binary(B))/binary>>),
            Res1 = R << 16 | G << 8 | B,
        end;
        false -> ok
    end,
    Res1.

main() ->
    vbeam_io:println('u32.hex'(convert_html_rgb(<<"#A0b0Cc">>))),
    vbeam_io:println('u32.hex'(convert_html_rgb(<<"#ABC">>))),
    vbeam_io:println('u32.hex'(convert_html_rgb_n(<<"#A0B0CC">>))),
    vbeam_io:println('u32.hex'(convert_html_rgb_n(<<"#ABC">>))),
    ok.
