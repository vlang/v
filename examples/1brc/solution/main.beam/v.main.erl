-module('v.main').
-export([mmap/6, munmap/2, mmap_file/1, 'MemoryMappedFile.unmap'/1, format_value/1, print_results/2, combine_results/1, process_chunk/3, process_in_parallel/2, main/0, 'ReadState__static__from'/1]).

mmap(Addr, Len, Prot, Flags, Fd, Offset) ->
    ok.

munmap(Addr, Len) ->
    ok.

mmap_file(Path) ->
    Mf = #{file => open_file(Path, <<"r">>, 0), size => file_size(Path), data => C.NULL, {vbeam, type} => 'MemoryMappedFile'},
    Mf.

'MemoryMappedFile.unmap'(Mf) ->
    case munmap(maps:get(data, Mf), maps:get(size, Mf)) /= 0 of
        true -> erlang:error({panic, <<"(", (integer_to_binary(C.errno))/binary, ") munmap() failed">>});
        false -> ok
    end,
    'File.close'(maps:get(file, Mf)),
    ok.

format_value(Value) ->
    <<(integer_to_binary(Value div 10))/binary, ".", (integer_to_binary(abs(Value rem 10)))/binary>>.

print_results(Results, Print_nicely) ->
    Output = [],
    Cities = maps:keys(Results),
    lists:sort(Cities),
    lists:foreach(fun(City) ->
        V = maps:get(City, Results),
        Mean = todo / maps:get(count, V) / 10,
        Output bsl <<(City)/binary, "=", (format_value(maps:get(min, V)))/binary, "/", (float_to_binary(Mean))/binary, "/", (format_value(maps:get(max, V)))/binary>>,
        ok
    end, Cities),
    case Print_nicely of
        true -> vbeam_io:println(iolist_to_binary(lists:join(<<"\\n">>, Output)));
        false -> vbeam_io:println(<<(<<(<<"{">>)/binary, (iolist_to_binary(lists:join(<<", ">>, Output)))/binary>>)/binary, (<<"}">>)/binary>>)
    end.

combine_results(Results) ->
    Combined_result = #{},
    lists:foreach(fun(Result) ->
        lists:foreach(fun(R) ->
            case (not lists:member(City, Combined_result)) of
                true -> ok;
                false -> begin
                    case maps:get(max, R) > maps:get(max, maps:get(City, Combined_result)) of
                        true -> ok;
                        false -> ok
                    end,
                    case maps:get(min, R) < maps:get(min, maps:get(City, Combined_result)) of
                        true -> ok;
                        false -> ok
                    end,
                end
            end,
            ok
        end, Result),
        ok
    end, Results),
    Combined_result.

process_chunk(Addr, From, To) ->
    Results = #{},
    State = city,
    City = <<"">>,
    Temp = todo,
    Mod = todo,
    J = todo,
    C = lists:foldl(fun(I, CAcc) ->
        COut = todo,
        case State of
            city -> case C of
                todo -> begin
                    State1 = temp,
                    City1 = todo,
                end;
                _ -> ok
            end;
            temp -> case C of
                todo -> begin
                    Temp1 = Mod,
                    case (not lists:member(City1, Results)) of
                        true -> ok;
                        false -> begin
                            case Temp1 > maps:get(max, maps:get(City1, Results)) of
                                true -> ok;
                                false -> ok
                            end,
                            case Temp1 < maps:get(min, maps:get(City1, Results)) of
                                true -> ok;
                                false -> ok
                            end,
                        end
                    end,
                    State2 = city,
                    Temp2 = 0,
                    Mod1 = 1,
                    J1 = 0,
                end;
                todo -> ok;
                todo -> ok;
                _ -> ok
            end
        end,
        COut
    end, C, lists:seq(From, To - 1)),
    Results.

process_in_parallel(Mf, Thread_count) ->
    Threads = [],
    Approx_chunk_size = maps:get(size, Mf) div Thread_count,
    From = todo,
    To = Approx_chunk_size,
    {From1, To1} = lists:foldl(fun(_, {FromAcc, ToAcc}) ->
        % TODO: unhandled stmt type
        Threads bsl todo,
        FromOut = ToAcc + 1,
        ToOut = FromAcc + Approx_chunk_size,
        {FromOut, ToOut}
    end, {From, To}, lists:seq(0, Thread_count - 1 - 1)),
    To2 = maps:get(size, Mf),
    Threads bsl todo,
    Res = ok,
    combine_results(Res).

main() ->
    Fp = new_flag_parser(init:get_plain_arguments()),
    'FlagParser.version'(Fp, <<"1brc v1.0.0">>),
    'FlagParser.skip_executable'(Fp),
    'FlagParser.application'(Fp, <<"1 billion rows challenge">>),
    'FlagParser.description'(Fp, <<"The 1 billion rows challenge solved in V.\\nFor details, see https://www.morling.dev/blog/one-billion-row-challenge/">>),
    Thread_count = todo,
    Print_nicely = 'FlagParser.bool'(Fp, <<"human-readable">>, todo, false, <<"Print results with new lines rather than following challenge spec">>, #{{vbeam, type} => 'FlagConfig'}),
    Quiet = 'FlagParser.bool'(Fp, <<"quiet">>, todo, false, <<"Suppress the results output (e.g., if you only care about timing)">>, #{{vbeam, type} => 'FlagConfig'}),
    'FlagParser.limit_free_args_to_exactly'(Fp, 1),
    Path = lists:nth(1, 'FlagParser.remaining_parameters'(Fp)),
    Mf = mmap_file(Path),
    % TODO: unhandled stmt type
    Results = case Thread_count > 1 of
        true -> process_in_parallel(Mf, Thread_count);
        false -> process_chunk(maps:get(data, Mf), 0, maps:get(size, Mf))
    end,
    case not Quiet of
        true -> print_results(Results, Print_nicely);
        false -> ok
    end.

'ReadState__static__from'(Input) ->
    error(<<"invalid value">>).
