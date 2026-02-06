-module('v.strings').
-export([new_builder/1, 'Builder.write_ptr'/3, 'Builder.write_string'/2, 'Builder.write_byte'/2, 'Builder.write_u8'/2, 'Builder.write_rune'/2, 'Builder.writeln'/2, 'Builder.str'/1, 'Builder.clear'/1, 'Builder.go_back'/2, 'Builder.go_back_to'/2, 'Builder.byte_at'/2, 'Builder.last_n'/2, 'Builder.after'/2, 'Builder.free'/1, 'Builder.reuse_as_plain_u8_array'/1, min/3, max2/2, min2/2, abs2/2, levenshtein_distance/2, levenshtein_distance_percentage/2, dice_coefficient/2, hamming_distance/2, hamming_similarity/2, jaro_similarity/2, jaro_winkler_similarity/2, repeat/2, repeat_string/2, find_between_pair_u8/3, find_between_pair_rune/3, find_between_pair_string/3, split_capital/1]).

new_builder(Initial_size) ->
    todo.

'Builder.write_ptr'(B, Ptr, Len) ->
    case Len == 0 of
        true -> ok;
        false -> 
            lists:foreach(fun(I) ->
                B bsl todo,
                ok
                ok
            end, lists:seq(0, Len - 1)),
                end.

'Builder.write_string'(B, S) ->
    lists:foreach(fun(C) ->
        B bsl C,
        ok.
        ok
    end, S),

'Builder.write_byte'(B, Data) ->
    B bsl Data,
    ok.

'Builder.write_u8'(B, Data) ->
    B bsl Data,
    ok.

'Builder.write_rune'(B, R) ->
    case R < 16#80 of
        true -> B bsl todo;
        false -> case R < 16#800 of
            true -> begin
                B bsl todo,
                B bsl todo
            end;
            false -> case R < 16#10000 of
                true -> begin
                    B bsl todo,
                    B bsl todo,
                    B bsl todo
                end;
                false -> begin
                    B bsl todo,
                    B bsl todo,
                    B bsl todo,
                    B bsl todo
                end
            end
        end
    end.

'Builder.writeln'(B, S) ->
    'Builder.write_string'(B, S),
    B bsl todo,
    ok.

'Builder.str'(B) ->
    S = 'Builder.bytestr'((*B)),
    'Builder.clear'(B),
    S.

'Builder.clear'(B) ->
    'Builder.clear'((*B)),
    ok.

'Builder.go_back'(B, N) ->
    case N > length(B) of
        true -> 'Builder.clear'(B);
        false -> 'Builder.trim'(B, length(B) - N)
    end.

'Builder.go_back_to'(B, Pos) ->
    case Pos =< 0 of
        true -> 'Builder.clear'(B);
        false -> case Pos < length(B) of
            true -> 'Builder.trim'(B, Pos);
            false -> ok
        end
    end.

'Builder.byte_at'(B, N) ->
    lists:nth(N + 1, (*B)).

'Builder.last_n'(B, N) ->
    case N > length(B) of
        true -> <<"">>;
        false -> 'Builder.bytestr'(lists:nth(todo + 1, (*B)))
        end.

'Builder.after'(B, N) ->
    case N >= length(B) of
        true -> <<"">>;
        false -> 'Builder.bytestr'(lists:nth(todo + 1, (*B)))
        end.

'Builder.free'(B) ->
    ok.

'Builder.reuse_as_plain_u8_array'(B) ->
    *B.

min(A, B, C) ->
    M = A,
    case B < M of
        true -> ok;
        false -> ok
    end,
    case C < M of
        true -> ok;
        false -> ok
    end,
    M.

max2(A, B) ->
    case A < B of
        true -> B;
        false -> A
        end.

min2(A, B) ->
    case A < B of
        true -> A;
        false -> B
        end.

abs2(A, B) ->
    case A < B of
        true -> B - A;
        false -> A - B
        end.

levenshtein_distance(A, B) ->
    case length(A) == 0 of
        true -> length(B);
        false -> 
            case length(B) == 0 of
                true -> length(A);
                false -> 
                    case A == B of
                        true -> 0;
                        false -> begin
                            Row = [],
                            % TODO: unhandled stmt type
                            ok                            lists:nth(length(A) + 1, Row)
                        end
                                        end
                                        end
                end.

levenshtein_distance_percentage(A, B) ->
    D = levenshtein_distance(A, B),
    L = case length(A) >= length(B) of
        true -> length(A);
        false -> length(B)
    end,
    (todo - todo / todo) * todo.

dice_coefficient(S1, S2) ->
    case length(S1) == 0 orelse length(S2) == 0 of
        true -> 0.0;
        false -> 
            case S1 == S2 of
                true -> 1.0;
                false -> 
                    case length(S1) < 2 orelse length(S2) < 2 of
                        true -> 0.0;
                        false -> begin
                            A = case length(S1) > length(S2) of
                                true -> S1;
                                false -> S2
                            end,
                            B = case A == S1 of
                                true -> S2;
                                false -> S1
                            end,
                            First_bigrams = #{},
                            {Bigram, Q} = lists:foldl(fun(I, {BigramAcc, QAcc}) ->
                                BigramOut = lists:nth(todo + 1, A),
                                QOut = case lists:member(Bigram, First_bigrams) of
                                    true -> maps:get(Bigram, First_bigrams) + 1;
                                    false -> 1
                                end,
                                {BigramOut, QOut}
                            end, {Bigram, Q}, lists:seq(0, length(A) - 1 - 1)),
                            Intersection_size = 0,
                            {Bigram1, Count} = lists:foldl(fun(I, {BigramAcc, CountAcc}) ->
                                BigramOut = lists:nth(todo + 1, B),
                                CountOut = case lists:member(Bigram1, First_bigrams) of
                                    true -> maps:get(Bigram1, First_bigrams);
                                    false -> 0
                                end,
                                case Count > 0 of
                                    true -> begin
                                        todo
                                    end;
                                    false -> ok
                                end,
                                {BigramOut, CountOut}
                            end, {Bigram, Count}, lists:seq(0, length(B) - 1 - 1)),
                            (todo * todo) / (todo + todo - 2)
                        end
                                        end
                                        end
                end.

hamming_distance(A, B) ->
    case length(A) == 0 andalso length(B) == 0 of
        true -> 0;
        false -> begin
            Match_len = min2(length(A), length(B)),
            Diff_count = abs2(length(A), length(B)),
            lists:foreach(fun(I) ->
                case lists:nth(I + 1, A) /= lists:nth(I + 1, B) of
                    true -> todo;
                    false -> ok
                end,
                ok
            end, lists:seq(0, Match_len - 1)),
            Diff_count
        end
        end.

hamming_similarity(A, B) ->
    L = max2(length(A), length(B)),
    case L == 0 of
        true -> 1.0;
        false -> begin
            D = hamming_distance(A, B),
            todo - todo / todo
        end
        end.

jaro_similarity(A, B) ->
    A_len = length(A),
    B_len = length(B),
    case A_len == 0 andalso B_len == 0 of
        true -> 1.0;
        false -> 
            case A_len == 0 orelse B_len == 0 of
                true -> 0;
                false -> begin
                    Match_distance = max2(A_len, B_len) div 2 - 1,
                    A_matches = [],
                    B_matches = [],
                    Matches = 0,
                    Transpositions = 0.0,
                    {Start, End} = lists:foldl(fun(I, {StartAcc, EndAcc}) ->
                        StartOut = max2(0, I - Match_distance),
                        EndOut = min2(B_len, I + Match_distance + 1),
                        lists:foreach(fun(K) ->
                            case lists:nth(K + 1, B_matches) of
                                true -> ok;
                                false -> ok
                            end,
                            case lists:nth(I + 1, A) /= lists:nth(K + 1, B) of
                                true -> ok;
                                false -> ok
                            end,
                            todo,
                            % TODO: unhandled stmt type
                            ok                            ok
                        end, lists:seq(Start, End - 1)),
                        {StartOut, EndOut}
                    end, {Start, End}, lists:seq(0, A_len - 1)),
                    case Matches == 0 of
                        true -> 0;
                        false -> begin
                            K = 0,
                            lists:foreach(fun(I) ->
                                case not lists:nth(I + 1, A_matches) of
                                    true -> ok;
                                    false -> ok
                                end,
                                % TODO: unhandled stmt type
                                ok                                case lists:nth(I + 1, A) /= lists:nth(K + 1, B) of
                                    true -> todo;
                                    false -> ok
                                end,
                                todo,
                                ok
                            end, lists:seq(0, A_len - 1)),
                            Transpositions1 = 2,
                            (Matches / todo + Matches / todo + (Matches - Transpositions1) / Matches) / 3
                        end
                                        end
                end
                        end
                end.

jaro_winkler_similarity(A, B) ->
    Lmax = min2(4, min2(length(A), length(B))),
    L = 0,
    lists:foreach(fun(I) ->
        case lists:nth(I + 1, A) == lists:nth(I + 1, B) of
            true -> todo;
            false -> ok
        end,
        ok
    end, lists:seq(0, Lmax - 1)),
    Js = jaro_similarity(A, B),
    P = 0.1,
    Ws = Js + todo * P * (1 - Js),
    Ws.

repeat(C, N) ->
    case N =< 0 of
        true -> <<"">>;
        false -> begin
            Arr = [],
            lists:foreach(fun(_) ->
                Arr bsl C,
                ok
            end, lists:seq(0, N - 1)),
            '[]u8.bytestr'(Arr)
        end
        end.

repeat_string(S, N) ->
    case N =< 0 orelse length(S) == 0 of
        true -> <<"">>;
        false -> begin
            Result = new_builder(length(S) * N),
            lists:foreach(fun(_) ->
                'Builder.write_string'(Result, S),
                ok
            end, lists:seq(0, N - 1)),
            'Builder.str'(Result)
        end
        end.

find_between_pair_u8(Input, Start, End) ->
    Marks = 0,
    Start_index = -1,
    lists:foreach(fun(B) ->
        case B == Start of
            true -> begin
                case Start_index == -1 of
                    true -> ok;
                    false -> ok
                end,
                todo,
                % TODO: unhandled stmt type
                ok            end;
            false -> ok
        end,
        case Start_index > 0 of
            true -> case B == End of
                true -> begin
                    todo,
                    case Marks == 0 of
                        true -> lists:nth(todo + 1, Input);
                        false -> ok
                    end
                end;
                false -> ok
            end;
            false -> ok
        end,
        ok
    end, Input),
    <<"">>.

find_between_pair_rune(Input, Start, End) ->
    Marks = 0,
    Start_index = -1,
    Runes = 'string.runes'(Input),
    lists:foreach(fun(R) ->
        case R == Start of
            true -> begin
                case Start_index == -1 of
                    true -> ok;
                    false -> ok
                end,
                todo,
                % TODO: unhandled stmt type
                ok            end;
            false -> ok
        end,
        case Start_index > 0 of
            true -> case R == End of
                true -> begin
                    todo,
                    case Marks == 0 of
                        true -> '[]rune.string'(lists:nth(todo + 1, Runes));
                        false -> ok
                    end
                end;
                false -> ok
            end;
            false -> ok
        end,
        ok
    end, Runes),
    <<"">>.

find_between_pair_string(Input, Start, End) ->
    Start_index = -1,
    Marks = 0,
    Start_runes = 'string.runes'(Start),
    End_runes = 'string.runes'(End),
    Runes = 'string.runes'(Input),
    I = 0,
    % TODO: unhandled stmt type
    ok    <<"">>.

split_capital(S) ->
    Res = [],
    Word_start = 0,
    lists:foreach(fun(C) ->
        case 'u8.is_capital'(C) of
            true -> begin
                case Word_start /= Idx of
                    true -> Res bsl lists:nth(todo + 1, S);
                    false -> ok
                end,
                Word_start1 = Idx,
                % TODO: unhandled stmt type
                ok            end;
            false -> ok
        end,
        ok
    end, S),
    case Word_start1 /= length(S) of
        true -> Res bsl lists:nth(todo + 1, S);
        false -> ok
    end,
    Res.
