-module('v.strings').
-export([find_between_pair_u8/3, find_between_pair_rune/3, find_between_pair_string/3, split_capital/1]).

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
                % TODO: continue
            end;
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
                % TODO: continue
            end;
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
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
    <<"">>.

split_capital(S) ->
    Res = [],
    Word_start = 0,
    lists:foreach(fun(C) ->
        case 'u8.is_capital'(C) of
            true -> begin
                case Word_start != Idx of
                    true -> Res << lists:nth(todo + 1, S);
                    false -> ok
                end,
                Word_start1 = Idx,
                % TODO: continue
            end;
            false -> ok
        end,
        ok
    end, S),
    case Word_start1 != length(S) of
        true -> Res << lists:nth(todo + 1, S);
        false -> ok
    end,
    Res.
