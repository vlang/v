-module('v.main').
-export([regex_match/2, regex_match_core/5, main/0]).

regex_match(Src, Pat) ->
    Src_size = length(Src) + 1,
    Pat_size = length(Pat) + 1,
    Memo = [],
    regex_match_core(Src, Pat, 0, 0, Memo).

regex_match_core(Src, Pat, Src_pos, Pat_pos, Memo) ->
    case lists:nth(Pat_pos + 1, lists:nth(Src_pos + 1, Memo)) /= -1 of
        true -> lists:nth(Pat_pos + 1, lists:nth(Src_pos + 1, Memo)) == 1;
        false -> begin
            Spos = Src_pos,
            Ppos = Pat_pos,
            case Spos >= length(Src) andalso Ppos >= length(Pat) of
                true -> begin
                    true
                end;
                false -> case Spos < length(Src) andalso Ppos >= length(Pat) of
                    true -> begin
                        false
                    end;
                    false -> case Spos >= length(Src) andalso Ppos < length(Pat) of
                        true -> begin
                            case lists:nth(Ppos + 1, Pat) == todo of
                                true -> todo;
                                false -> ok
                            end,
                            Res = Ppos + 1 < length(Pat) andalso lists:member(lists:nth(Ppos + 1 + 1, Pat), [todo, todo]) andalso regex_match_core(Src, Pat, Spos, Ppos + 2, Memo),
                            Res
                        end;
                        false -> begin
                            First_is_bslash = lists:nth(Ppos + 1, Pat) == todo,
                            case First_is_bslash of
                                true -> todo;
                                false -> ok
                            end,
                            First_bslash_and_match = First_is_bslash andalso Ppos < length(Pat) andalso (((lists:nth(Ppos + 1, Pat) == todo andalso 'u8.is_digit'(lists:nth(Spos + 1, Src))) orelse (lists:nth(Ppos + 1, Pat) == todo andalso not 'u8.is_digit'(lists:nth(Spos + 1, Src))) orelse (lists:nth(Ppos + 1, Pat) == todo andalso 'u8.is_space'(lists:nth(Spos + 1, Src))) orelse (lists:nth(Ppos + 1, Pat) == todo andalso not 'u8.is_space'(lists:nth(Spos + 1, Src))) orelse (lists:nth(Ppos + 1, Pat) == todo andalso ('u8.is_digit'(lists:nth(Spos + 1, Src)) orelse 'u8.is_letter'(lists:nth(Spos + 1, Src)) orelse lists:nth(Spos + 1, Src) == todo)) orelse (lists:nth(Ppos + 1, Pat) == todo andalso not ('u8.is_digit'(lists:nth(Spos + 1, Src)) orelse 'u8.is_letter'(lists:nth(Spos + 1, Src)) orelse lists:nth(Spos + 1, Src) == todo))) orelse (lists:member(lists:nth(Ppos + 1, Pat), [todo, todo, todo, todo, todo, todo]) andalso Ppos + 1 < length(Pat) andalso lists:member(lists:nth(Ppos + 1 + 1, Pat), [todo, todo, todo])) orelse ((not lists:member(lists:nth(Ppos + 1, Pat), [todo, todo, todo, todo, todo, todo])) andalso lists:nth(Spos + 1, Src) == lists:nth(Ppos + 1, Pat))),
                            case Ppos + 1 < length(Pat) of
                                true -> case lists:nth(Ppos + 1 + 1, Pat) of
                                    todo -> case First_bslash_and_match of
                                        true -> begin
                                            Res1 = regex_match_core(Src, Pat, Spos + 1, Ppos - 1, Memo) orelse regex_match_core(Src, Pat, Spos, Ppos + 2, Memo),
                                            Res1
                                        end;
                                        false -> case lists:nth(Spos + 1, Src) == lists:nth(Ppos + 1, Pat) orelse lists:nth(Ppos + 1, Pat) == todo of
                                            true -> begin
                                                Res2 = regex_match_core(Src, Pat, Spos + 1, Ppos, Memo) orelse regex_match_core(Src, Pat, Spos, Ppos + 2, Memo),
                                                Res2
                                            end;
                                            false -> begin
                                                Res3 = regex_match_core(Src, Pat, Spos, Ppos + 2, Memo),
                                                Res3
                                            end
                                        end
                                    end;
                                    todo -> case First_bslash_and_match of
                                        true -> begin
                                            Res4 = regex_match_core(Src, Pat, Spos + 1, Ppos - 1, Memo) orelse regex_match_core(Src, Pat, Spos + 1, Ppos + 2, Memo),
                                            Res4
                                        end;
                                        false -> case lists:nth(Spos + 1, Src) == lists:nth(Ppos + 1, Pat) orelse lists:nth(Ppos + 1, Pat) == todo of
                                            true -> begin
                                                Res5 = regex_match_core(Src, Pat, Spos + 1, Ppos, Memo) orelse regex_match_core(Src, Pat, Spos + 1, Ppos + 2, Memo),
                                                Res5
                                            end;
                                            false -> begin
                                                false
                                            end
                                        end
                                    end;
                                    todo -> case First_bslash_and_match orelse lists:nth(Spos + 1, Src) == lists:nth(Ppos + 1, Pat) orelse lists:nth(Ppos + 1, Pat) == todo of
                                        true -> begin
                                            Res6 = regex_match_core(Src, Pat, Spos + 1, Ppos + 2, Memo) orelse regex_match_core(Src, Pat, Spos, Ppos + 2, Memo),
                                            Res6
                                        end;
                                        false -> begin
                                            Res7 = regex_match_core(Src, Pat, Spos, Ppos + 2, Memo),
                                            Res7
                                        end
                                    end;
                                    _ -> ok
                                end;
                                false -> ok
                            end,
                            case First_is_bslash of
                                true -> begin
                                    Res8 = First_bslash_and_match andalso regex_match_core(Src, Pat, Spos + 1, Ppos + 1, Memo),
                                    Res8
                                end;
                                false -> begin
                                    Res9 = (lists:nth(Spos + 1, Src) == lists:nth(Ppos + 1, Pat) orelse lists:nth(Ppos + 1, Pat) == todo) andalso lists:nth(Ppos + 1, Pat) /= todo andalso regex_match_core(Src, Pat, Spos + 1, Ppos + 1, Memo),
                                    Res9
                                end
                            end
                        end
                    end
                end
            end
        end
        end.

main() ->
    Cnt = 0,
    io:format("~s~n", [<<"currently supported patterns: . ? + * \\\\ \\\\d \\\\D \\\\s \\\\S \\\\w \\\\W">>]),
    io:format("~s~n", [<<"example: source `address@domain.net` matches pattern `\\\\w+@domain\\\\.net`">>]),
    io:format("~s~n", [<<"enter `exit` to quit\\n">>]),
    % TODO: unhandled stmt type
        ok.
