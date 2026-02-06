-module('v.main').
-export(['BFState__static__new'/1, 'BFState.show'/2, 'BFState.find_matching_pairs'/1, 'BFState.panic_for_bracket'/3, 'BFState.run'/1, show_usage/0, main/0]).

'BFState__static__new'(Program) ->
    State = #{program => Program, {vbeam, type} => 'BFState'},
    'BFState.find_matching_pairs'(State),
    State.

'BFState.show'(State, Suffix) ->
    Max_non_zero_address = -1,
    % TODO: unhandled stmt type
    ok    vbeam_io:println(<<"PC: ", (integer_to_binary(maps:get(pc, State)))/binary, " | Address: ", (integer_to_binary(maps:get(address, State)))/binary, " | Memory: ", (lists:nth(todo + 1, maps:get(memory, State)))/binary, " | Memory[Address]: ", (lists:nth(todo + 1, maps:get(memory, State)))/binary, " | ", (Suffix)/binary>>),
    ok.

'BFState.find_matching_pairs'(State) ->
    Stack = [],
    Pi = lists:foldl(fun(I, PiAcc) ->
        PiOut = lists:nth(I + 1, maps:get(program, State)),
        case Pi of
            todo -> Stack bsl I;
            todo -> begin
                case length(Stack) == 0 of
                    true -> begin
                        eprintln(<<"> unmatched `]` found in the program, at position: ", (integer_to_binary(I))/binary>>),
                        eprintln(<<"program so far:">>),
                        eprintln(lists:nth(todo + 1, maps:get(program, State))),
                        exit(1)
                    end;
                    false -> ok
                end,
                Pc = '[]int.pop'(Stack),
            end;
            _ -> ok
        end,
        PiOut
    end, Pi, lists:seq(0, length(maps:get(program, State)) - 1)),
    case length(Stack) > 0 of
        true -> begin
            eprintln(<<"> found ", (integer_to_binary(length(Stack)))/binary, " unmatched `[`:">>),
            lists:foreach(fun(I) ->
                eprintln(<<"  `[` at position: ", (integer_to_binary(I))/binary, ", program so far: `", (lists:nth(todo + 1, maps:get(program, State)))/binary, "`">>),
                ok.
                ok
            end, Stack),
            exit(1)
        end;
        false -> ok
    end.

'BFState.panic_for_bracket'(State, B1, B2) ->
    panic(<<"unbalanced `", (B1)/binary, "` found, its target `", (B2)/binary, "` is not known; address: ", (integer_to_binary(maps:get(address, State)))/binary, ", pc: ", (integer_to_binary(maps:get(pc, State)))/binary>>),
    ok.

'BFState.run'(State) ->
    I = 0,
    % TODO: unhandled stmt type
    ok    ok.

show_usage() ->
    eprintln(<<"you need to supply a brainfuck program/expression as a string argument,">>),
    eprintln(<<"or filename.b, if it is located in a file (note the `.b` or `.bf` extension).">>),
    exit(1),
    ok.

main() ->
    case length('v.os':'arguments'()) < 2 of
        true -> show_usage();
        false -> ok
    end,
    Program = lists:nth(2, 'v.os':'arguments'()),
    case 'string.ends_with'(Program, <<".b">>) orelse 'string.ends_with'(Program, <<".bf">>) of
        true -> ok;
        false -> ok
    end,
    State = 'BFState__static__new'(Program),
    'BFState.run'(State),
    case getenv(<<"VERBOSE">>) /= <<"">> of
        true -> 'BFState.show'(State, <<"FINAL">>);
        false -> ok
    end.
