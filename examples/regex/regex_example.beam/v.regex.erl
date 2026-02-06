-module('v.regex').
-export([utf8util_char_len/1, 'RE.get_char'/3, 'RE.get_charb'/3, is_alnum/1, is_not_alnum/1, is_space/1, is_not_space/1, is_digit/1, is_not_digit/1, is_lower/1, is_upper/1, 'RE.get_parse_error_string'/2, utf8_str/1, simple_log/1, 'Token.reset'/1, 'RE.reset'/1, 'RE.reset_src'/1, 'RE.parse_bsls'/3, 'RE.get_char_class'/2, 'RE.check_char_class'/3, 'RE.parse_char_class'/3, 'RE.parse_quantifier'/3, 'RE.parse_groups'/3, 'RE.impl_compile'/2, 'RE.get_code'/1, 'RE.get_query'/1, 'RE.group_continuous_save'/2, state_str/1, 'RE.match_base'/3, 'RE.compile_opt'/2, new/0, regex_opt/1, regex_base/1, 'RE.get_group_bounds_by_name'/2, 'RE.get_group_by_name'/3, 'RE.get_group_by_id'/3, 'RE.get_group_bounds_by_id'/2, 'RE.get_group_list'/1, 'RE.match_string'/2, 'RE.matches_string'/2, 'RE.find'/2, 'RE.find_from'/3, 'RE.find_all'/2, 'RE.split'/2, 'RE.find_all_str'/2, 'RE.replace_simple'/3, 'RE.replace_by_fn'/3, 'RE.parsed_replace_string'/3, 'RE.replace'/3, 'RE.replace_n'/4, 'BSLS_parse_state__static__from'/1, 'CharClass_parse_state__static__from'/1, 'Quant_parse_state__static__from'/1, 'Group_parse_state__static__from'/1, 'Match_state__static__from'/1]).

utf8util_char_len(B) ->
    ((16#e5000000 bsr ((B bsr 3) band 16#1e)) band 3) + 1.

'RE.get_char'(Re, In_txt, I) ->
    Ini = todo,
    case (maps:get(flag, Re) band 16#00000200) /= 0 orelse Ini band 16#80 == 0 of
        true -> todo;
        false -> begin
            Char_len = utf8util_char_len(Ini),
            Tmp = 0,
            Ch = todo,
            % TODO: unhandled stmt type
            Ch
        end
        end.

'RE.get_charb'(Re, In_txt, I) ->
    case (maps:get(flag, Re) band 16#00000200) /= 0 orelse todo band 16#80 == 0 of
        true -> todo;
        false -> begin
            Char_len = utf8util_char_len(todo),
            Tmp = 0,
            Ch = todo,
            % TODO: unhandled stmt type
            Ch
        end
        end.

is_alnum(In_char) ->
    Tmp = In_char - todo,
    case Tmp =< 25 of
        true -> true;
        false -> begin
            Tmp1 = In_char - todo,
            case Tmp1 =< 25 of
                true -> true;
                false -> begin
                    Tmp2 = In_char - todo,
                    case Tmp2 =< 9 of
                        true -> true;
                        false -> 
                            case In_char == todo of
                                true -> true;
                                false -> false
                                                        end
                                                                end
                end
                        end
        end
        end.

is_not_alnum(In_char) ->
    not is_alnum(In_char).

is_space(In_char) ->
    lists:member(In_char, [todo, todo, todo, todo, todo, todo]).

is_not_space(In_char) ->
    not is_space(In_char).

is_digit(In_char) ->
    Tmp = In_char - todo,
    Tmp =< 16#09.

is_not_digit(In_char) ->
    not is_digit(In_char).

is_lower(In_char) ->
    Tmp = In_char - todo,
    Tmp =< 25.

is_upper(In_char) ->
    Tmp = In_char - todo,
    Tmp =< 25.

'RE.get_parse_error_string'(Re, Err) ->
    case Err of
        Regex.compile_ok -> <<"compile_ok">>;
        Regex.no_match_found -> <<"no_match_found">>;
        Regex.err_char_unknown -> <<"err_char_unknown">>;
        Regex.err_undefined -> <<"err_undefined">>;
        Regex.err_internal_error -> <<"err_internal_error">>;
        Regex.err_cc_alloc_overflow -> <<"err_cc_alloc_overflow">>;
        Regex.err_syntax_error -> <<"err_syntax_error">>;
        Regex.err_groups_overflow -> <<"err_groups_overflow">>;
        Regex.err_groups_max_nested -> <<"err_groups_max_nested">>;
        Regex.err_group_not_balanced -> <<"err_group_not_balanced">>;
        Regex.err_group_qm_notation -> <<"err_group_qm_notation">>;
        Regex.err_invalid_or_with_cc -> <<"err_invalid_or_with_cc">>;
        Regex.err_neg_group_quantifier -> <<"err_neg_group_quantifier">>;
        Regex.err_consecutive_dots -> <<"err_consecutive_dots">>;
        _ -> <<"err_unknown">>
    end.

utf8_str(Ch) ->
    I = 4,
    Res = <<"">>,
    % TODO: unhandled stmt type
    Res.

simple_log(Txt) ->
    io:format("~s", [Txt]),
    ok.

'Token.reset'(Tok) ->

'RE.reset'(Re) ->
    I = 0,
    % TODO: unhandled stmt type
    case maps:get(group_count, Re) > 0 of
        true -> case length(maps:get(groups, Re)) == 0 of
            true -> ok;
            false -> begin
                I1 = 0,
                % TODO: unhandled stmt type
            end
        end;
        false -> ok
    end,
    case maps:get(group_csave_flag, Re) == true of
        true -> '[]int.clear'(maps:get(group_csave, Re));
        false -> ok
    end,
    'StateObj.clear'(maps:get(state_list, Re)),
    lists:foreach(fun(X) ->
        X = -1,
        ok
    end, maps:get(group_stack, Re)),
        ok.

'RE.reset_src'(Re) ->
    I = 0,
    % TODO: unhandled stmt type
        ok.

'RE.parse_bsls'(Re, In_txt, In_i) ->
    Status = start,
    I = In_i,
    Hex_max_len = 2,
    Hex_res = todo,
    Hex_count = 0,
    % TODO: unhandled stmt type
    -6.

'RE.get_char_class'(Re, Pc) ->
    Buf = [],
    Buf_ptr = todo,
    Cc_i = maps:get(cc_index, lists:nth(Pc + 1, maps:get(prog, Re))),
    I = 0,
    Tmp = 0,
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    todo.

'RE.check_char_class'(Re, Pc, Ch) ->
    Cc_i = maps:get(cc_index, lists:nth(Pc + 1, maps:get(prog, Re))),
    % TODO: unhandled stmt type
    false.

'RE.parse_char_class'(Re, In_txt, In_i) ->
    Status = start,
    I = In_i,
    Tmp_index = maps:get(cc_index, Re),
    Res_index = maps:get(cc_index, Re),
    Cc_type = todo,
    % TODO: unhandled stmt type
    -6.

'RE.parse_quantifier'(Re, In_txt, In_i) ->
    Status = start,
    I = In_i,
    Q_min = 0,
    Q_max = 0,
    Ch = todo,
    % TODO: unhandled stmt type
    -6.

'RE.parse_groups'(Re, In_txt, In_i) ->
    Status = start,
    I = In_i,
    Name = <<"">>,
    % TODO: unhandled stmt type
    -2.

'RE.impl_compile'(Re, In_txt) ->
    I = 0,
    Pc = 0,
    Group_count = -1,
    Group_stack = [],
    Group_stack_txt_index = [],
    Group_stack_index = -1,
    I1 = 0,
    % TODO: unhandled stmt type
    case Group_stack_index /= -1 of
        true -> -9;
        false -> 
            case Pc > 0 andalso maps:get(ist, lists:nth(Pc - 1 + 1, maps:get(prog, Re))) == todo of
                true -> -6;
                false -> begin
                    Pc1 = 0,
                    Dot_char_count = 0,
                    Last_dot_char_pc = -1,
                    % TODO: unhandled stmt type
                    case Last_dot_char_pc >= 0 of
                        true -> begin
                            Pc11 = Last_dot_char_pc + 1,
                            Is_last_dot = true,
                            % TODO: unhandled stmt type
                            case Is_last_dot of
                                true -> ok;
                                false -> ok
                            end
                        end;
                        false -> ok
                    end,
                    Pc12 = 0,
                    Bsls_char_count = 0,
                    Last_bsls_char_pc = -1,
                    % TODO: unhandled stmt type
                    case Last_bsls_char_pc >= 0 of
                        true -> begin
                            Pc13 = Last_bsls_char_pc + 1,
                            Is_last_bsls = true,
                            % TODO: unhandled stmt type
                            case Is_last_bsls of
                                true -> ok;
                                false -> ok
                            end
                        end;
                        false -> ok
                    end,
                    Pc14 = 0,
                    Cc_char_count = 0,
                    Last_cc_char_pc = -1,
                    % TODO: unhandled stmt type
                    case Last_cc_char_pc >= 0 of
                        true -> begin
                            Pc15 = Last_cc_char_pc + 1,
                            Is_last_cc = true,
                            % TODO: unhandled stmt type
                            case Is_last_cc of
                                true -> ok;
                                false -> ok
                            end
                        end;
                        false -> ok
                    end,
                    Pc16 = 0,
                    % TODO: unhandled stmt type
                    case maps:get(debug, Re) > 0 of
                        true -> begin
                            Gc = 'RE.get_code'(Re),
                            'RE.log_func'(Re, Gc)
                        end;
                        false -> ok
                    end,
                    0
                end
                        end
                end.

'RE.get_code'(Re) ->
    Pc1 = 0,
    Res = new_builder(length(maps:get(cc, Re)) * 2 * length(maps:get(prog, Re))),
    'Builder.write_string'(Res, <<"========================================\\nv RegEx compiler v ", (<<"1.0 alpha">>)/binary, " output:\\n">>),
    Stop_flag = false,
    % TODO: unhandled stmt type
    'Builder.write_string'(Res, <<"========================================\\n">>),
    'Builder.str'(Res).

'RE.get_query'(Re) ->
    Res = new_builder(length(maps:get(query, Re)) * 2),
    case (maps:get(flag, Re) band 16#00000002) /= 0 of
        true -> 'Builder.write_string'(Res, <<"^">>);
        false -> ok
    end,
    I = 0,
    % TODO: unhandled stmt type
    case (maps:get(flag, Re) band 16#00000004) /= 0 of
        true -> 'Builder.write_string'(Res, <<"$">>);
        false -> ok
    end,
    'Builder.str'(Res).

'RE.group_continuous_save'(Re, G_index) ->
    case maps:get(group_csave_flag, Re) == true of
        true -> begin
            case length(maps:get(group_csave, Re)) == 0 of
                true -> maps:get(group_csave, Re) bsl 0;
                false -> ok
            end,
            Gi = G_index bsr 1,
            Start = lists:nth(G_index + 1, maps:get(groups, Re)),
            End = lists:nth(G_index + 1 + 1, maps:get(groups, Re)),
            case length(maps:get(group_csave, Re)) >= 4 andalso Gi == lists:nth(length(maps:get(group_csave, Re)) - 3 + 1, maps:get(group_csave, Re)) andalso Start == lists:nth(length(maps:get(group_csave, Re)) - 2 + 1, maps:get(group_csave, Re)) of
                true -> begin
                end;
                false -> ok
            end,
            todo,
            maps:get(group_csave, Re) bsl (G_index bsr 1),
            maps:get(group_csave, Re) bsl lists:nth(G_index + 1, maps:get(groups, Re)),
            maps:get(group_csave, Re) bsl lists:nth(G_index + 1 + 1, maps:get(groups, Re))
        end;
        false -> ok
    end.

state_str(S) ->
    case S of
        start -> <<"start">>;
        stop -> <<"stop">>;
        end -> <<"end">>;
        new_line -> <<"new line">>;
        ist_load -> <<"ist_load">>;
        ist_next -> <<"ist_next">>;
        ist_next_ks -> <<"ist_next_ks">>;
        ist_quant_p -> <<"ist_quant_p">>;
        ist_quant_n -> <<"ist_quant_n">>;
        ist_quant_pg -> <<"ist_quant_pg">>;
        ist_quant_ng -> <<"ist_quant_ng">>
    end.

'RE.match_base'(Re, In_txt, In_txt_len) ->
    Result = -1,
    Ch = todo,
    Char_len = 0,
    M_state = start,
    Src_end = false,
    Last_fnd_pc = -1,
    State = #{{vbeam, type} => 'StateObj'},
    Ist = todo,
    L_ist = todo,
    Step_count = 0,
    Dbg_line = 0,
    'RE.reset'(Re),
    case maps:get(debug, Re) > 0 of
        true -> begin
            H_buf = new_builder(32),
            'Builder.write_string'(H_buf, <<"flags: ">>),
            'Builder.write_string'(H_buf, binary:replace(integer_to_binary(maps:get(flag, Re)), <<" ">>, <<"0">>, [global])),
            'Builder.write_string'(H_buf, <<"\\n">>),
            Sss = 'Builder.str'(H_buf),
            'RE.log_func'(Re, Sss)
        end;
        false -> ok
    end,
    % TODO: unhandled stmt type
    case maps:get(match_index, State) >= 0 of
        true -> case maps:get(group_index, State) < 0 of
            true -> begin
                case maps:get(ist, lists:nth(maps:get(pc, State) + 1, maps:get(prog, Re))) == todo of
                    true -> case (maps:get(flag, Re) band 16#00020000) /= 0 of
                        true -> maps:get(first_match, State);
                        false -> 0
                    end;
                    false -> ok
                end,
                case maps:get(ist, lists:nth(maps:get(pc, State) + 1 + 1, maps:get(prog, Re))) == todo orelse maps:get(ist, lists:nth(maps:get(pc, State) + 1, maps:get(prog, Re))) == todo of
                    true -> begin
                        Rep = maps:get(rep, lists:nth(maps:get(pc, State) + 1, maps:get(prog, Re))),
                        case Rep >= maps:get(rep_min, lists:nth(maps:get(pc, State) + 1, maps:get(prog, Re))) andalso Rep =< maps:get(rep_max, lists:nth(maps:get(pc, State) + 1, maps:get(prog, Re))) of
                            true -> maps:get(first_match, State);
                            false -> ok
                        end,
                        -1
                    end;
                    false -> ok
                end,
                case Src_end of
                    true -> maps:get(first_match, State);
                    false -> ok
                end,
                -1
            end;
            false -> maps:get(first_match, State)
        end;
        false -> ok
    end,
    -1.

'RE.compile_opt'(Re, Pattern) ->
    Re_err = element(1, 'RE.impl_compile'(Re, Pattern)),
    Err_pos = element(2, 'RE.impl_compile'(Re, Pattern)),
    case Re_err /= 0 of
        true -> error_with_code('Builder.str'(Err_msg), Re_err);
        false -> ok
        end.

new() ->
    Re = #{{vbeam, type} => 'RE'},
    Re.

regex_opt(Pattern) ->
    Re = #{{vbeam, type} => 'RE'},
    'RE.compile_opt'(Re, Pattern),
    Re.

regex_base(Pattern) ->
    Re = #{{vbeam, type} => 'RE'},
    Re_err = element(1, 'RE.impl_compile'(Re, Pattern)),
    Err_pos = element(2, 'RE.impl_compile'(Re, Pattern)),
    Re.

'RE.get_group_bounds_by_name'(Re, Group_name) ->
    case lists:member(Group_name, maps:get(group_map, Re)) of
        true -> Start;
        false -> -1
        end.

'RE.get_group_by_name'(Re, In_txt, Group_name) ->
    case lists:member(Group_name, maps:get(group_map, Re)) of
        true -> begin
            Tmp_index = maps:get(Group_name, maps:get(group_map, Re)) - 1,
            Start = lists:nth(Tmp_index * 2 + 1, maps:get(groups, Re)),
            End = lists:nth(Tmp_index * 2 + 1 + 1, maps:get(groups, Re)),
            case Start >= 0 andalso End > Start of
                true -> lists:nth(todo + 1, In_txt);
                false -> ok
            end
        end;
        false -> ok
    end,
    <<"">>.

'RE.get_group_by_id'(Re, In_txt, Group_id) ->
    case Group_id < (length(maps:get(groups, Re)) bsr 1) of
        true -> begin
            Index = Group_id * 2,
            Start = lists:nth(Index + 1, maps:get(groups, Re)),
            End = lists:nth(Index + 1 + 1, maps:get(groups, Re)),
            case Start >= 0 andalso End > Start of
                true -> lists:nth(todo + 1, In_txt);
                false -> ok
            end
        end;
        false -> ok
    end,
    <<"">>.

'RE.get_group_bounds_by_id'(Re, Group_id) ->
    case Group_id < maps:get(group_count, Re) of
        true -> lists:nth(Index + 1, maps:get(groups, Re));
        false -> -1
        end.

'RE.get_group_list'(Re) ->
    Res = [],
    Gi = 0,
    % TODO: unhandled stmt type
    Res.

'RE.match_string'(Re, In_txt) ->
    % TODO: unhandled stmt type
        ok.

'RE.matches_string'(Re, In_txt) ->
    Start = element(1, 'RE.match_string'(Re, In_txt)),
    Start /= -1.

'RE.find'(Re, In_txt) ->
    I = 0,
    % TODO: unhandled stmt type
    -1.

'RE.find_from'(Re, In_txt, Start) ->
    Old_flag = maps:get(flag, Re),
    I = Start,
    case I < 0 of
        true -> -1;
        false -> begin
            % TODO: unhandled stmt type
            -1
        end
        end.

'RE.find_all'(Re, In_txt) ->
    I = 0,
    Res = [],
    % TODO: unhandled stmt type
    Res.

'RE.split'(Re, In_txt) ->
    Pos = 'RE.find_all'(Re, In_txt),
    Sections = [],
    case length(Pos) == 0 of
        true -> [In_txt];
        false -> begin
            % TODO: unhandled stmt type
            Sections bsl lists:nth(todo + 1, In_txt),
            Sections
        end
        end.

'RE.find_all_str'(Re, In_txt) ->
    I = 0,
    Res = [],
    % TODO: unhandled stmt type
    Res.

'RE.replace_simple'(Re, In_txt, Repl) ->
    Pos = 'RE.find_all'(Re, In_txt),
    case length(Pos) > 0 of
        true -> Res;
        false -> In_txt
        end.

'RE.replace_by_fn'(Re, In_txt, Repl_fn) ->
    I = 0,
    Res = new_builder(length(In_txt)),
    Last_end = 0,
    % TODO: unhandled stmt type
    case Last_end >= 0 andalso Last_end < length(In_txt) of
        true -> 'Builder.write_string'(Res, lists:nth(todo + 1, In_txt));
        false -> ok
    end,
    'Builder.str'(Res).

'RE.parsed_replace_string'(Re, In_txt, Repl) ->
    Str_lst = binary:split(Repl, <<"\\\\">>, [global]),
    Res = lists:nth(1, Str_lst),
    I = 1,
    % TODO: unhandled stmt type
    Res.

'RE.replace'(Re, In_txt, Repl_str) ->
    I = 0,
    Res = new_builder(length(In_txt)),
    Last_end = 0,
    % TODO: unhandled stmt type
    case Last_end >= 0 andalso Last_end < length(In_txt) of
        true -> 'Builder.write_string'(Res, lists:nth(todo + 1, In_txt));
        false -> ok
    end,
    'Builder.str'(Res).

'RE.replace_n'(Re, In_txt, Repl_str, Count) ->
    I = 0,
    Index = 0,
    I_p = 0,
    Res = new_builder(length(In_txt)),
    Lst = 'RE.find_all'(Re, In_txt),
    case Count < 0 of
        true -> ok;
        false -> case Count > 0 of
            true -> ok;
            false -> case Count == 0 of
                true -> In_txt;
                false -> ok
            end
        end
    end,
    % TODO: unhandled stmt type
    I1 = I_p,
    'Builder.write_string'(Res, lists:nth(todo + 1, In_txt)),
    'Builder.str'(Res).

'BSLS_parse_state__static__from'(Input) ->
    error(<<"invalid value">>).

'CharClass_parse_state__static__from'(Input) ->
    error(<<"invalid value">>).

'Quant_parse_state__static__from'(Input) ->
    error(<<"invalid value">>).

'Group_parse_state__static__from'(Input) ->
    error(<<"invalid value">>).

'Match_state__static__from'(Input) ->
    error(<<"invalid value">>).
