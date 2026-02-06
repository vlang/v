-module('v.main').
-export(['BFState__static__new'/1, 'BFState.show'/2, 'BFState.find_matching_pairs'/1, 'BFState.panic_for_bracket'/3, 'BFState.run'/1, show_usage/0, main/0]).

'BFState__static__new'(Program) ->
    State = #{program => Program, {vbeam, type} => 'BFState'},
    'BFState.find_matching_pairs'(State),
    State.

'BFState.show'(State, Suffix) ->
    Max_non_zero_address = -1,
    % TODO: unhandled stmt type
    vbeam_io:println(<<"PC: ", (integer_to_binary(maps:get(pc, State)))/binary, " | Address: ", (integer_to_binary(maps:get(address, State)))/binary, " | Memory: ", (lists:nth(todo + 1, maps:get(memory, State)))/binary, " | Memory[Address]: ", (lists:nth(todo + 1, maps:get(memory, State)))/binary, " | ", (Suffix)/binary>>),
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
                        io:format(standard_error, "~s~n", [<<"> unmatched `]` found in the program, at position: ", (integer_to_binary(I))/binary>>]),
                        io:format(standard_error, "~s~n", [<<"program so far:">>]),
                        io:format(standard_error, "~s~n", [lists:nth(todo + 1, maps:get(program, State))]),
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
            io:format(standard_error, "~s~n", [<<"> found ", (integer_to_binary(length(Stack)))/binary, " unmatched `[`:">>]),
            lists:foreach(fun(I) ->
                io:format(standard_error, "~s~n", [<<"  `[` at position: ", (integer_to_binary(I))/binary, ", program so far: `", (lists:nth(todo + 1, maps:get(program, State)))/binary, "`">>]),
                ok.
                ok
            end, Stack),
            exit(1)
        end;
        false -> ok
    end.

'BFState.panic_for_bracket'(State, B1, B2) ->
    erlang:error({panic, <<"unbalanced `", (B1)/binary, "` found, its target `", (B2)/binary, "` is not known; address: ", (integer_to_binary(maps:get(address, State)))/binary, ", pc: ", (integer_to_binary(maps:get(pc, State)))/binary>>}),
    ok.

'BFState.run'(State) ->
    I = 0,
    % TODO: unhandled stmt type
    ok.

show_usage() ->
    io:format(standard_error, "~s~n", [<<"you need to supply a brainfuck program/expression as a string argument,">>]),
    io:format(standard_error, "~s~n", [<<"or filename.b, if it is located in a file (note the `.b` or `.bf` extension).">>]),
    exit(1),
    ok.

main() ->
    case length(init:get_plain_arguments()) < 2 of
        true -> show_usage();
        false -> ok
    end,
    Program = lists:nth(2, init:get_plain_arguments()),
    case case binary:longest_common_suffix([Program, <<".b">>]) of 0 -> false; _ -> true end orelse case binary:longest_common_suffix([Program, <<".bf">>]) of 0 -> false; _ -> true end of
        true -> ok;
        false -> ok
    end,
    State = 'BFState__static__new'(Program),
    'BFState.run'(State),
    case getenv(<<"VERBOSE">>) /= <<"">> of
        true -> 'BFState.show'(State, <<"FINAL">>);
        false -> ok
    end.
