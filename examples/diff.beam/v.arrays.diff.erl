-module('v.arrays.diff').
-export([diff/2, 'DiffContext.compare'/5, 'DiffContext.find_middle_snake'/5, 'DiffContext.result'/3, 'DiffContext.merge_changes'/2, 'DiffContext.generate_patch'/2, 'DiffContext.write_context'/5, 'DiffContext.write_change'/4, 'DiffContextFlag.is_empty'/1, 'DiffContextFlag.has'/2, 'DiffContextFlag.all'/2, 'DiffContextFlag.set'/2, 'DiffContextFlag.set_all'/1, 'DiffContextFlag.clear'/2, 'DiffContextFlag.clear_all'/1, 'DiffContextFlag.toggle'/2, 'DiffContextFlag__static__zero'/0, 'DiffContextFlag__static__from'/1]).

diff(A, B) ->
    C = #{a => A, b => B, {vbeam, type} => 'DiffContext'},
    'DiffContext.compare'(C, 0, 0, length(A), length(B)),
    C.

'DiffContext.compare'(C, Mut_aoffset, Mut_boffset, Mut_alimit, Mut_blimit) ->
    Aoffset = Mut_aoffset,
    Boffset = Mut_boffset,
    Alimit = Mut_alimit,
    Blimit = Mut_blimit,
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    case Aoffset == Alimit of
        true -> ok;
        false -> 
            case Boffset == Blimit of
                true -> ok;
                false -> begin
                    X = element(1, 'DiffContext.find_middle_snake'(C, Aoffset, Boffset, Alimit, Blimit)),
                    Y = element(2, 'DiffContext.find_middle_snake'(C, Aoffset, Boffset, Alimit, Blimit)),
                    'DiffContext.compare'(C, Aoffset, Boffset, X, Y),
                    'DiffContext.compare'(C, X, Y, Alimit, Blimit),
                    ok
                end
                        end
                end.

'DiffContext.find_middle_snake'(C, Aoffset, Boffset, Alimit, Blimit) ->
    Fmid = Aoffset - Boffset,
    Rmid = Alimit - Blimit,
    Foff = maps:get(max, C) - Fmid,
    Roff = maps:get(max, C) - Rmid,
    Isodd = (Rmid - Fmid) band 1 /= 0,
    Maxd = (Alimit - Aoffset + Blimit - Boffset + 2) div 2,
    X = 0,
    Y = 0,
    % TODO: unhandled stmt type
    erlang:error({panic, <<"diff.find_middle_snake: should never be reached">>}),
    ok.

'DiffContext.result'(C, N, M) ->
    X = 0,
    Y = 0,
    Res = [],
    % TODO: unhandled stmt type
    Res.

'DiffContext.merge_changes'(C, Context_lines) ->
    case length(maps:get(changes, C)) == 0 of
        true -> ok;
        false -> begin
            Merged = [],
            Current = lists:nth(1, maps:get(changes, C)),
            Next = lists:foldl(fun(I, NextAcc) ->
                NextOut = lists:nth(I + 1, maps:get(changes, C)),
                case maps:get(a, Next) =< maps:get(a, Current) + maps:get(del, Current) + Context_lines of
                    true -> ok;
                    false -> begin
                        Merged bsl Current,
                        Current1 = Next,
                    end
                end,
                NextOut
            end, Next, lists:seq(1, length(maps:get(changes, C)) - 1)),
            Merged bsl Current1,
        end
        end.

'DiffContext.generate_patch'(C, Param) ->
    Sb = new_builder(100),
    % TODO: unhandled stmt type
    Unified = case maps:get(unified, Param) < 0 of
        true -> 0;
        false -> maps:get(unified, Param)
    end,
    'DiffContext.merge_changes'(C, Unified),
    case length(maps:get(changes, C)) == 0 of
        true -> <<"">>;
        false -> begin
            Prev_a_end = 0,
            Prev_b_end = 0,
            lists:foreach(fun(Change) ->
                Ctx_start_a = int_max(Prev_a_end, maps:get(a, Change) - Unified),
                Ctx_end_a = maps:get(a, Change) + maps:get(del, Change) + Unified,
                Ctx_start_b = int_max(Prev_b_end, maps:get(b, Change) - Unified),
                Ctx_end_b = maps:get(b, Change) + maps:get(ins, Change) + Unified,
                case maps:get(block_header, Param) of
                    true -> begin
                        case maps:get(colorful, Param) of
                            true -> 'Builder.write_string'(Sb, <<"\\033[36m">>);
                            false -> ok
                        end,
                        'Builder.writeln'(Sb, <<"@@ -", (integer_to_binary(Ctx_start_a + 1))/binary, ",", (integer_to_binary(Ctx_end_a - Ctx_start_a))/binary, " +", (integer_to_binary(Ctx_start_b + 1))/binary, ",", (integer_to_binary(Ctx_end_b - Ctx_start_b))/binary, " @@">>),
                        case maps:get(colorful, Param) of
                            true -> 'Builder.write_string'(Sb, <<"\\033[0m">>);
                            false -> ok
                        end
                    end;
                    false -> ok
                end,
                'DiffContext.write_context'(C, Sb, Ctx_start_b, maps:get(b, Change), Param),
                'DiffContext.write_change'(C, Sb, Change, Param),
                'DiffContext.write_context'(C, Sb, maps:get(b, Change) + maps:get(ins, Change), Ctx_end_b, Param),
                Prev_a_end1 = Ctx_end_a,
                Prev_b_end1 = Ctx_end_b,
                ok
            end, maps:get(changes, C)),
            'Builder.str'(Sb)
        end
        end.

'DiffContext.write_context'(C, Sb, Start, End, Param) ->
    Line = lists:foldl(fun(I, LineAcc) ->
        case I >= length(maps:get(b, C)) of
            true -> ok;
            false -> ok
        end.
        LineOut = 'T.str'(lists:nth(I + 1, maps:get(b, C))),
        case maps:get(colorful, Param) of
            true -> 'Builder.writeln'(Sb, <<"\\033[37m", (Line)/binary, "\\033[0m">>);
            false -> 'Builder.writeln'(Sb, Line)
        end.
        LineOut
    end, Line, lists:seq(Start, End - 1)),
        ok.

'DiffContext.write_change'(C, Sb, Change, Param) ->
    Line = lists:foldl(fun(I, LineAcc) ->
        LineOut = 'T.str'(lists:nth(I + 1, maps:get(a, C))),
        case maps:get(colorful, Param) of
            true -> 'Builder.writeln'(Sb, <<"\\033[31m-", (Line)/binary, "\\033[0m">>);
            false -> 'Builder.writeln'(Sb, <<"-", (Line)/binary>>)
        end,
        LineOut
    end, Line, lists:seq(maps:get(a, Change), maps:get(a, Change) + maps:get(del, Change) - 1)),
    Line1 = lists:foldl(fun(I, LineAcc) ->
        LineOut = 'T.str'(lists:nth(I + 1, maps:get(b, C))),
        case maps:get(colorful, Param) of
            true -> 'Builder.writeln'(Sb, <<"\\033[32m+", (Line1)/binary, "\\033[0m">>);
            false -> 'Builder.writeln'(Sb, <<"+", (Line1)/binary>>)
        end.
        LineOut
    end, Line, lists:seq(maps:get(b, Change), maps:get(b, Change) + maps:get(ins, Change) - 1)),
        ok.

'DiffContextFlag.is_empty'(E) ->
    todo == 0.

'DiffContextFlag.has'(E, Flag_) ->
    (todo band (todo)) /= 0.

'DiffContextFlag.all'(E, Flag_) ->
    (todo band (todo)) == todo.

'DiffContextFlag.set'(E, Flag_) ->
    % TODO: unhandled stmt type
        ok.

'DiffContextFlag.set_all'(E) ->
    % TODO: unhandled stmt type
        ok.

'DiffContextFlag.clear'(E, Flag_) ->
    % TODO: unhandled stmt type
        ok.

'DiffContextFlag.clear_all'(E) ->
    % TODO: unhandled stmt type
        ok.

'DiffContextFlag.toggle'(E, Flag_) ->
    % TODO: unhandled stmt type
        ok.

'DiffContextFlag__static__zero'() ->
    todo.

'DiffContextFlag__static__from'(Input) ->
    error(<<"invalid value">>).
