-module('v.flag').
-export(['UnknownFlagError.msg'/1, 'ArgsCountError.msg'/1, 'Flag.free'/1, 'Flag.str'/1, 'Flag.str'/1, 'FlagParser.free'/1, new_flag_parser/1, 'FlagParser.usage_example'/2, 'FlagParser.footer'/2, 'FlagParser.application'/2, 'FlagParser.version'/2, 'FlagParser.description'/2, 'FlagParser.skip_executable'/1, 'FlagParser.allow_unknown_args'/1, 'FlagParser.add_flag'/5, 'FlagParser.parse_value'/3, 'FlagParser.parse_bool_value'/3, 'FlagParser.bool_opt'/5, 'FlagParser.bool'/6, 'FlagParser.int_multi'/5, 'FlagParser.int_opt'/5, 'FlagParser.int'/6, 'FlagParser.float_multi'/5, 'FlagParser.float_opt'/5, 'FlagParser.float'/6, 'FlagParser.string_multi'/5, 'FlagParser.string_opt'/5, 'FlagParser.string'/6, 'FlagParser.limit_free_args_to_at_least'/2, 'FlagParser.limit_free_args_to_exactly'/2, 'FlagParser.limit_free_args'/3, 'FlagParser.arguments_description'/2, 'FlagParser.usage'/1, 'FlagParser.find_existing_flag'/2, 'FlagParser.handle_builtin_options'/1, 'FlagParser.finalize'/1, 'FlagParser.remaining_parameters'/1, 'StructField.shortest_match_name'/1, 'DocLayout.max_width'/1, trace_println/1, trace_dbg_println/1, 'FlagMapper.dbg_match'/5, 'FlagMapper.get_struct_info'/1, 'FlagData.query_flag_with_name'/2, to_struct/2, using/3, to_doc/1, 'FlagMapper.no_matches'/1, 'FlagMapper.parse'/1, 'FlagMapper.to_doc'/2, 'FlagMapper.fields_docs'/2, keep_at_max/2, 'FlagMapper.to_struct'/2, 'FlagMapper.map_v'/3, 'FlagMapper.map_v_flag_parser_short'/3, 'FlagMapper.map_v_flag_parser_long'/3, 'FlagMapper.map_go_flag_short'/3, 'FlagMapper.map_go_flag_long'/3, 'FlagMapper.map_gnu_long'/3, 'FlagMapper.map_posix_short_cluster'/2, 'FlagMapper.map_posix_short'/3, 'FlagMapper.map_cmd_exe'/3, 'ParseMode__static__from'/1, 'Style__static__from'/1, 'FieldHints.is_empty'/1, 'FieldHints.has'/2, 'FieldHints.all'/2, 'FieldHints.set'/2, 'FieldHints.set_all'/1, 'FieldHints.clear'/2, 'FieldHints.clear_all'/1, 'FieldHints.toggle'/2, 'FieldHints__static__zero'/0, 'FieldHints__static__from'/1, 'Show.is_empty'/1, 'Show.has'/2, 'Show.all'/2, 'Show.set'/2, 'Show.set_all'/1, 'Show.clear'/2, 'Show.clear_all'/1, 'Show.toggle'/2, 'Show__static__zero'/0, 'Show__static__from'/1]).

'UnknownFlagError.msg'(Err) ->
    <<"Unknown flag `", (maps:get(flag, Err))/binary, "`">>.

'ArgsCountError.msg'(Err) ->
    case maps:get(want, Err) == 0 of
        true -> <<"Expected no arguments, but got ", (integer_to_binary(maps:get(got, Err)))/binary>>;
        false -> case maps:get(got, Err) > maps:get(want, Err) of
            true -> <<"Expected at most ", (integer_to_binary(maps:get(want, Err)))/binary, " arguments, but got ", (integer_to_binary(maps:get(got, Err)))/binary>>;
            false -> <<"Expected at least ", (integer_to_binary(maps:get(want, Err)))/binary, " arguments, but got ", (integer_to_binary(maps:get(got, Err)))/binary>>
        end
    end.

'Flag.free'(F) ->
    % TODO: unhandled stmt type
    ok
'Flag.str'(F) ->
    <<"    flag:\n            name: ", (maps:get(name, F))/binary, "\n            abbr: `", ('u8.ascii_str'(maps:get(abbr, F)))/binary, "`\n            usage: ", (maps:get(usage, F))/binary, "\n            desc: ", (maps:get(val_desc, F))/binary>>.

'Flag.str'(Af) ->
    Res = [],
    Res bsl <<"\\n  []Flag = [">>,
    lists:foreach(fun(F) ->
        Res bsl 'Flag.str'(F),
        ok
    end, Af),
    Res bsl <<"  ]">>,
    '[]string.join'(Res, <<"\\n">>).

'FlagParser.free'(F) ->
    % TODO: unhandled stmt type
    ok
new_flag_parser(Args) ->
    Original_args = '[]string.clone'(Args),
    Idx_dashdash = '[]string.index'(Args, <<"--">>),
    All_before_dashdash = '[]string.clone'(Args),
    All_after_dashdash = [],
    case Idx_dashdash >= 0 of
        true -> begin
            '[]string.trim'(All_before_dashdash, Idx_dashdash),
            case Idx_dashdash < length(Original_args) of
                true -> ok;
                false -> ok
            end
        end;
        false -> ok
    end,
    #{original_args => Original_args, idx_dashdash => Idx_dashdash, all_after_dashdash => All_after_dashdash, args => All_before_dashdash, max_free_args => 4048, {vbeam, type} => 'FlagParser'}.

'FlagParser.usage_example'(Fs, Example) ->
    maps:get(usage_examples, Fs) bsl Example,
    ok.

'FlagParser.footer'(Fs, Footer) ->
    maps:get(footers, Fs) bsl Footer,
    ok.

'FlagParser.application'(Fs, Name) ->

'FlagParser.version'(Fs, Vers) ->

'FlagParser.description'(Fs, Desc) ->
    case length(maps:get(application_description, Fs)) == 0 of
        true -> ok;
        false -> ok
    end.

'FlagParser.skip_executable'(Fs) ->
    '[]string.delete'(maps:get(args, Fs), 0),
    ok.

'FlagParser.allow_unknown_args'(Fs) ->

'FlagParser.add_flag'(Fs, Name, Abbr, Usage, Desc) ->
    maps:get(flags, Fs) bsl #{name => Name, abbr => Abbr, usage => Usage, val_desc => Desc, {vbeam, type} => 'Flag'},
    ok.

'FlagParser.parse_value'(Fs, Longhand, Shorthand) ->
    Full = <<"--", (Longhand)/binary>>,
    % TODO: unhandled stmt type
    ok    Found_entries = [],
    To_delete = [],
    % TODO: unhandled stmt type
    ok    Should_skip_one = false,
    lists:foreach(fun(Arg) ->
        case Should_skip_one of
            true -> begin
                Should_skip_one1 = false,
                % TODO: unhandled stmt type
                ok            end;
            false -> ok
        end,
        case length(Arg) == 0 orelse lists:nth(1, Arg) /= todo of
            true -> ok;
            false -> ok
        end,
        case (length(Arg) == 2 andalso lists:nth(1, Arg) == todo andalso lists:nth(2, Arg) == Shorthand) orelse Arg == Full of
            true -> begin
                case I + 1 >= length(maps:get(args, Fs)) of
                    true -> [];
                    false -> ok
                end,
                Nextarg = lists:nth(I + 1 + 1, maps:get(args, Fs)),
                case length(Nextarg) > 2 of
                    true -> begin
                        Nextarg_rest = lists:nth(todo + 1, Nextarg),
                        case Nextarg_rest == <<"--">> of
                            true -> begin
                                todo,
                                []
                            end;
                            false -> ok
                        end,
                        todo
                    end;
                    false -> ok
                end,
                Found_entries bsl lists:nth(I + 1 + 1, maps:get(args, Fs)),
                To_delete bsl I,
                To_delete bsl I + 1,
                Should_skip_one2 = true,
                % TODO: unhandled stmt type
                ok            end;
            false -> ok
        end,
        case length(Arg) > length(Full) + 1 andalso lists:nth(todo + 1, Arg) == <<(Full)/binary, "=">> of
            true -> begin
                Found_entries bsl lists:nth(todo + 1, Arg),
                To_delete bsl I,
                % TODO: unhandled stmt type
                ok            end;
            false -> ok
        end,
        ok
    end, maps:get(args, Fs)),
    lists:foreach(fun(Del) ->
        '[]string.delete'(maps:get(args, Fs), Del - I),
        ok
    end, To_delete),
    Found_entries.

'FlagParser.parse_bool_value'(Fs, Longhand, Shorthand) ->
    Full = <<"--", (Longhand)/binary>>,
    lists:foreach(fun(Arg) ->
        case length(Arg) == 0 of
            true -> ok;
            false -> ok
        end,
        case lists:nth(1, Arg) /= todo of
            true -> ok;
            false -> ok
        end,
        case (length(Arg) == 2 andalso lists:nth(1, Arg) == todo andalso lists:nth(2, Arg) == Shorthand) orelse Arg == Full of
            true -> case length(maps:get(args, Fs)) > I + 1 andalso lists:member(lists:nth(I + 1 + 1, maps:get(args, Fs)), [<<"true">>, <<"false">>]) of
                true -> begin
                    Val = lists:nth(I + 1 + 1, maps:get(args, Fs)),
                    '[]string.delete'(maps:get(args, Fs), I + 1),
                    '[]string.delete'(maps:get(args, Fs), I),
                    Val
                end;
                false -> begin
                    '[]string.delete'(maps:get(args, Fs), I),
                    <<"true">>
                end
            end;
            false -> ok
        end,
        case length(Arg) > length(Full) + 1 andalso lists:nth(todo + 1, Arg) == <<(Full)/binary, "=">> of
            true -> begin
                Val1 = lists:nth(todo + 1, Arg),
                '[]string.delete'(maps:get(args, Fs), I),
                Val1
            end;
            false -> ok
        end,
        case length(Arg) > 1 andalso lists:nth(1, Arg) == todo andalso lists:nth(2, Arg) /= todo of
            true -> begin
                Found = false,
                lists:foreach(fun(J) ->
                    case 'u8.is_space'(lists:nth(J + 1, Arg)) of
                        true -> ok;
                        false -> case lists:nth(J + 1, Arg) == Shorthand of
                            true -> ok;
                            false -> ok
                        end
                    end,
                    ok
                end, lists:seq(1, length(Arg) - 1)),
                case Found of
                    true -> begin
                        <<"true">>
                    end;
                    false -> ok
                end
            end;
            false -> ok
        end,
        ok
    end, maps:get(args, Fs)),
    error(<<"parameter '", (Longhand)/binary, "' not found">>).

'FlagParser.bool_opt'(Fs, Name, Abbr, Usage, C) ->
    Val_desc = case maps:get(val_desc, C) == <<"">> of
        true -> <<"<bool>">>;
        false -> maps:get(val_desc, C)
    end,
    'FlagParser.add_flag'(Fs, Name, Abbr, Usage, Val_desc),
    Parsed = 'FlagParser.parse_bool_value'(Fs, Name, Abbr),
    Parsed == <<"true">>.

'FlagParser.bool'(Fs, Name, Abbr, Bdefault, Usage, C) ->
    Value = 'FlagParser.bool_opt'(Fs, Name, Abbr, Usage, C),
    Value.

'FlagParser.int_multi'(Fs, Name, Abbr, Usage, C) ->
    Val_desc = case maps:get(val_desc, C) == <<"">> of
        true -> <<"<multiple ints>">>;
        false -> maps:get(val_desc, C)
    end,
    'FlagParser.add_flag'(Fs, Name, Abbr, Usage, Val_desc),
    Parsed = 'FlagParser.parse_value'(Fs, Name, Abbr),
    Value = [],
    lists:foreach(fun(Val) ->
        Value bsl 'string.int'(Val),
        ok
    end, Parsed),
    Value.

'FlagParser.int_opt'(Fs, Name, Abbr, Usage, C) ->
    Val_desc = case maps:get(val_desc, C) == <<"">> of
        true -> <<"<int>">>;
        false -> maps:get(val_desc, C)
    end,
    'FlagParser.add_flag'(Fs, Name, Abbr, Usage, Val_desc),
    Parsed = 'FlagParser.parse_value'(Fs, Name, Abbr),
    case length(Parsed) == 0 of
        true -> error(<<"parameter '", (Name)/binary, "' not provided">>);
        false -> begin
            Parsed0 = lists:nth(1, Parsed),
            'string.int'(Parsed0)
        end
        end.

'FlagParser.int'(Fs, Name, Abbr, Idefault, Usage, C) ->
    Value = 'FlagParser.int_opt'(Fs, Name, Abbr, Usage, C),
    Value.

'FlagParser.float_multi'(Fs, Name, Abbr, Usage, C) ->
    Val_desc = case maps:get(val_desc, C) == <<"">> of
        true -> <<"<multiple floats>">>;
        false -> maps:get(val_desc, C)
    end,
    'FlagParser.add_flag'(Fs, Name, Abbr, Usage, Val_desc),
    Parsed = 'FlagParser.parse_value'(Fs, Name, Abbr),
    Value = [],
    lists:foreach(fun(Val) ->
        Value bsl 'string.f64'(Val),
        ok
    end, Parsed),
    Value.

'FlagParser.float_opt'(Fs, Name, Abbr, Usage, C) ->
    Val_desc = case maps:get(val_desc, C) == <<"">> of
        true -> <<"<float>">>;
        false -> maps:get(val_desc, C)
    end,
    'FlagParser.add_flag'(Fs, Name, Abbr, Usage, Val_desc),
    Parsed = 'FlagParser.parse_value'(Fs, Name, Abbr),
    case length(Parsed) == 0 of
        true -> error(<<"parameter '", (Name)/binary, "' not provided">>);
        false -> 'string.f64'(lists:nth(1, Parsed))
        end.

'FlagParser.float'(Fs, Name, Abbr, Fdefault, Usage, C) ->
    Value = 'FlagParser.float_opt'(Fs, Name, Abbr, Usage, C),
    Value.

'FlagParser.string_multi'(Fs, Name, Abbr, Usage, C) ->
    Val_desc = case maps:get(val_desc, C) == <<"">> of
        true -> <<"<multiple strings>">>;
        false -> maps:get(val_desc, C)
    end,
    'FlagParser.add_flag'(Fs, Name, Abbr, Usage, Val_desc),
    'FlagParser.parse_value'(Fs, Name, Abbr).

'FlagParser.string_opt'(Fs, Name, Abbr, Usage, C) ->
    Val_desc = case maps:get(val_desc, C) == <<"">> of
        true -> <<"<string>">>;
        false -> maps:get(val_desc, C)
    end,
    'FlagParser.add_flag'(Fs, Name, Abbr, Usage, Val_desc),
    Parsed = 'FlagParser.parse_value'(Fs, Name, Abbr),
    case length(Parsed) == 0 of
        true -> error(<<"parameter '", (Name)/binary, "' not provided">>);
        false -> lists:nth(1, Parsed)
        end.

'FlagParser.string'(Fs, Name, Abbr, Sdefault, Usage, C) ->
    Value = 'FlagParser.string_opt'(Fs, Name, Abbr, Usage, C),
    Value.

'FlagParser.limit_free_args_to_at_least'(Fs, N) ->
    case N > 4048 of
        true -> error(<<"flag.limit_free_args_to_at_least expect n to be smaller than ", (integer_to_binary(4048))/binary>>);
        false -> 
            case N =< 0 of
                true -> error(<<"flag.limit_free_args_to_at_least expect n to be a positive number">>);
                false -> begin
                    ok
                end
                        end
                end.

'FlagParser.limit_free_args_to_exactly'(Fs, N) ->
    case N > 4048 of
        true -> error(<<"flag.limit_free_args_to_exactly expect n to be smaller than ", (integer_to_binary(4048))/binary>>);
        false -> 
            case N < 0 of
                true -> error(<<"flag.limit_free_args_to_exactly expect n to be a non negative number">>);
                false -> begin
                    ok
                end
                        end
                end.

'FlagParser.limit_free_args'(Fs, Min, Max) ->
    case Min > Max of
        true -> error(<<"flag.limit_free_args expect min < max, got ", (integer_to_binary(Min))/binary, " >= ", (integer_to_binary(Max))/binary>>);
        false -> begin
            ok
        end
        end.

'FlagParser.arguments_description'(Fs, Description) ->

'FlagParser.usage'(Fs) ->
    Positive_min_arg = (maps:get(min_free_args, Fs) > 0),
    Positive_max_arg = (maps:get(max_free_args, Fs) > 0 andalso maps:get(max_free_args, Fs) /= 4048),
    No_arguments = (maps:get(min_free_args, Fs) == 0 andalso maps:get(max_free_args, Fs) == 0),
    Adesc = case length(maps:get(args_description, Fs)) > 0 of
        true -> maps:get(args_description, Fs);
        false -> <<"[ARGS]">>
    end,
    case No_arguments of
        true -> ok;
        false -> ok
    end,
    Use = [],
    case maps:get(application_version, Fs) /= <<"">> of
        true -> begin
            Use bsl <<(maps:get(application_name, Fs))/binary, " ", (maps:get(application_version, Fs))/binary>>,
            Use bsl <<"-----------------------------------------------">>
        end;
        false -> ok
    end,
    case length(maps:get(usage_examples, Fs)) == 0 of
        true -> Use bsl <<"Usage: ", (maps:get(application_name, Fs))/binary, " [options] ", (Adesc)/binary>>;
        false -> ok
    end,
    Use bsl <<"">>,
    case maps:get(application_description, Fs) /= <<"">> of
        true -> begin
            Use bsl <<"Description: ", (maps:get(application_description, Fs))/binary>>,
            Use bsl <<"">>
        end;
        false -> ok
    end,
    case Positive_min_arg orelse Positive_max_arg orelse No_arguments of
        true -> case No_arguments of
            true -> begin
                Use bsl <<"This application does not expect any arguments">>,
                Use bsl <<"">>
            end;
            false -> begin
                S = [],
                case Positive_min_arg of
                    true -> S bsl <<"at least ", (integer_to_binary(maps:get(min_free_args, Fs)))/binary>>;
                    false -> ok
                end,
                case Positive_max_arg of
                    true -> S bsl <<"at most ", (integer_to_binary(maps:get(max_free_args, Fs)))/binary>>;
                    false -> ok
                end,
                case Positive_min_arg andalso Positive_max_arg andalso maps:get(min_free_args, Fs) == maps:get(max_free_args, Fs) of
                    true -> ok;
                    false -> ok
                end,
                Sargs = '[]string.join'(S, <<" and ">>),
                Use bsl <<"The arguments should be ", (Sargs)/binary, " in number.">>,
                Use bsl <<"">>
            end
        end;
        false -> ok
    end,
    case length(maps:get(flags, Fs)) > 0 of
        true -> begin
            Use bsl <<"Options:">>,
            lists:foreach(fun(F) ->
                Onames = [],
                case maps:get(abbr, F) /= 0 of
                    true -> Onames bsl <<"-", ('u8.ascii_str'(maps:get(abbr, F)))/binary>>;
                    false -> ok
                end,
                case maps:get(name, F) /= <<"">> of
                    true -> case not 'string.contains'(maps:get(val_desc, F), <<"<bool>">>) of
                        true -> Onames bsl <<"--", (maps:get(name, F))/binary, " ", (maps:get(val_desc, F))/binary>>;
                        false -> Onames bsl <<"--", (maps:get(name, F))/binary>>
                    end;
                    false -> ok
                end,
                Option_names = <<(<<"  ">>)/binary, ('[]string.join'(Onames, <<", ">>))/binary>>,
                Xspace = <<"">>,
                case length(Option_names) > length(<<"                            ">>) - 2 of
                    true -> ok;
                    false -> ok
                end,
                Fdesc = <<(Option_names)/binary, (Xspace)/binary, (maps:get(usage, F))/binary>>,
                Use bsl Fdesc,
                ok
            end, maps:get(flags, Fs)),
        end;
        false -> ok
    end,
    lists:foreach(fun(Footer) ->
        Use bsl Footer,
        ok
    end, maps:get(footers, Fs)),
    'string.replace'('[]string.join'(Use, <<"\\n">>), <<"- ,">>, <<"   ">>).

'FlagParser.find_existing_flag'(Fs, Fname) ->
    lists:foreach(fun(F) ->
        case maps:get(name, F) == Fname of
            true -> F;
            false -> ok
        end,
        ok
    end, maps:get(flags, Fs)),
    error(<<"no such flag">>).

'FlagParser.handle_builtin_options'(Fs) ->
    Show_version = false,
    Show_help = false,
    'FlagParser.find_existing_flag'(Fs, <<"help">>),
    'FlagParser.find_existing_flag'(Fs, <<"version">>),
    case Show_help of
        true -> begin
            vbeam_io:println('FlagParser.usage'(Fs)),
            exit(0)
        end;
        false -> ok
    end,
    case Show_version of
        true -> begin
            vbeam_io:println(<<(maps:get(application_name, Fs))/binary, " ", (maps:get(application_version, Fs))/binary>>),
            exit(0)
        end;
        false -> ok
    end.

'FlagParser.finalize'(Fs) ->
    'FlagParser.handle_builtin_options'(Fs),
    Remaining = '[]string.clone'(maps:get(args, Fs)),
    case not maps:get(allow_unknown_args, Fs) of
        true -> ok;
        false -> ok
    end,
    Remaining bsl maps:get(all_after_dashdash, Fs),
    case maps:get(min_free_args, Fs) > length(Remaining) of
        true -> todo;
        false -> 
            case maps:get(max_free_args, Fs) < length(Remaining) of
                true -> todo;
                false -> Remaining
                        end
                end.

'FlagParser.remaining_parameters'(Fs) ->
    'FlagParser.finalize'(Fs).

'StructField.shortest_match_name'(Sf) ->
    Name = maps:get(short, Sf),
    case Name == <<"">> andalso length(maps:get(match_name, Sf)) == 1 of
        true -> ok;
        false -> ok
    end,
    case Name /= <<"">> of
        true -> Name;
        false -> todo
        end.

'DocLayout.max_width'(Dl) ->
    maps:get(flag_indent, Dl) + maps:get(description_padding, Dl) + maps:get(description_width, Dl).

trace_println(Str) ->
    vbeam_io:println(Str),
    ok.

trace_dbg_println(Str) ->
    vbeam_io:println(Str),
    ok.

'FlagMapper.dbg_match'(Fm, Flag_ctx, Field, Arg, Field_extra) ->
    Struct_name = maps:get(name, maps:get(si, Fm)),
    Extra = case Field_extra /= <<"">> of
        true -> <<(<<"/">>)/binary, (Field_extra)/binary>>;
        false -> <<"">>
    end,
    <<(Struct_name)/binary, ".", (maps:get(name, Field))/binary, "/", (maps:get(short, Field))/binary, (Extra)/binary, " in ", (maps:get(raw, Flag_ctx))/binary, "/", (maps:get(name, Flag_ctx))/binary, " = `", (Arg)/binary, "`">>.

'FlagMapper.get_struct_info'(Fm) ->
    Struct_fields = #{},
    Struct_attrs = #{},
    Struct_name = <<"">>,
    Used_names = [],
    #{name => Struct_name, attrs => Struct_attrs, fields => Struct_fields, {vbeam, type} => 'StructInfo'}.

'FlagData.query_flag_with_name'(M, Name) ->
    lists:foreach(fun(Flag_data) ->
        case maps:get(name, Flag_data) == Name of
            true -> Flag_data;
            false -> ok
        end,
        ok
    end, M),
    todo.

to_struct(Input, Config) ->
    Fm = #{config => Config, input => Input, {vbeam, type} => 'FlagMapper'},
    'unknown.parse'(Fm),
    St = 'unknown.to_struct'(Fm, todo),
    St.

using(Defaults, Input, Config) ->
    Fm = #{config => Config, input => Input, {vbeam, type} => 'FlagMapper'},
    'unknown.parse'(Fm),
    St = 'unknown.to_struct'(Fm, Defaults),
    St.

to_doc(Dc) ->
    Fm = #{config => #{delimiter => maps:get(delimiter, Dc), style => maps:get(style, Dc), {vbeam, type} => 'ParseConfig'}, input => [], {vbeam, type} => 'FlagMapper'},
    'unknown.to_doc'(Fm, Dc).

'FlagMapper.no_matches'(Fm) ->
    Non_matching = [],
    lists:foreach(fun(I) ->
        Non_matching bsl lists:nth(I + 1, maps:get(input, Fm)),
        ok
    end, maps:get(no_match, Fm)),
    Non_matching.

'FlagMapper.parse'(Fm) ->
    Config = maps:get(config, Fm),
    Style = maps:get(style, Config),
    Delimiter = maps:get(delimiter, Config),
    Args = maps:get(input, Fm),
    trace_println(<<": parsing ">>),
    case maps:get(skip, Config) > 0 of
        true -> ok;
        false -> ok
    end,
    Struct_name = maps:get(name, maps:get(si, Fm)),
    Pos_last_flag = -1,
    lists:foreach(fun(Arg) ->
        case 'unknown.starts_with'(Arg, Delimiter) of
            true -> ok;
            false -> ok
        end,
        ok
    end, Args),
    lists:foreach(fun(Arg) ->
        case Arg == <<"">> of
            true -> begin
                maps:get(no_match, Fm) bsl Pos,
                % TODO: unhandled stmt type
                ok            end;
            false -> ok
        end.
        Pos_is_handled = lists:member(Pos, maps:get(handled_pos, Fm)),
        case not Pos_is_handled of
            true -> case Arg == maps:get(stop, Config) of
                true -> begin
                    trace_println(<<": reached option stop (", ") at index ">>),
                    case Pos < length(Args) - 1 of
                        true -> ok;
                        false -> ok
                    end,
                    % TODO: unhandled stmt type
                    ok                end;
                false -> ok
            end;
            false -> ok
        end.
        Next = case Pos + 1 < length(Args) of
            true -> lists:nth(Pos + 1 + 1, Args);
            false -> <<"">>
        end,
        Is_flag = false,
        Flag_name = <<"">>,
        case 'unknown.starts_with'(Arg, Delimiter) of
            true -> begin
                Is_flag1 = true,
                Flag_name1 = 'unknown.trim_left'(Arg, Delimiter),
                case lists:member(Style, [long, short_long, go_flag]) of
                    true -> ok;
                    false -> ok
                end
            end;
            false -> ok
        end.
        case Is_flag1 of
            true -> begin
                Used_delimiter = 'unknown.all_before'(Arg, Flag_name1),
                Is_long_delimiter = 'unknown.count'(Used_delimiter, Delimiter) == 2,
                Is_short_delimiter = 'unknown.count'(Used_delimiter, Delimiter) == 1,
                Is_invalid_delimiter = not Is_long_delimiter andalso not Is_short_delimiter,
                case Is_invalid_delimiter of
                    true -> begin
                        case maps:get(mode, Config) == relaxed of
                            true -> begin
                                maps:get(no_match, Fm) bsl Pos,
                                % TODO: unhandled stmt type
                                ok                            end;
                            false -> ok
                        end,
                        error(<<"invalid delimiter `", "` for flag `", "`">>)
                    end;
                    false -> ok
                end,
                case Is_long_delimiter of
                    true -> begin
                        case Style == v of
                            true -> begin
                                case maps:get(mode, Config) == relaxed of
                                    true -> begin
                                        maps:get(no_match, Fm) bsl Pos,
                                        % TODO: unhandled stmt type
                                        ok                                    end;
                                    false -> ok
                                end,
                                error(<<"long delimiter `", "` encountered in flag `", "` in ", " (V) style parsing mode. Maybe you meant `.v_flag_parser`?">>)
                            end;
                            false -> ok
                        end,
                        case Style == short of
                            true -> begin
                                case maps:get(mode, Config) == relaxed of
                                    true -> begin
                                        maps:get(no_match, Fm) bsl Pos,
                                        % TODO: unhandled stmt type
                                        ok                                    end;
                                    false -> ok
                                end,
                                error(<<"long delimiter `", "` encountered in flag `", "` in ", " (POSIX) style parsing mode">>)
                            end;
                            false -> ok
                        end
                    end;
                    false -> ok
                end,
                case Is_short_delimiter of
                    true -> begin
                        case Style == long of
                            true -> begin
                                case maps:get(mode, Config) == relaxed of
                                    true -> begin
                                        maps:get(no_match, Fm) bsl Pos,
                                        % TODO: unhandled stmt type
                                        ok                                    end;
                                    false -> ok
                                end,
                                error(<<"short delimiter `", "` encountered in flag `", "` in ", " (GNU) style parsing mode">>)
                            end;
                            false -> ok
                        end,
                        case Style == short_long andalso length(Flag_name1) > 1 andalso 'unknown.contains'(Flag_name1, <<"-">>) of
                            true -> begin
                                case maps:get(mode, Config) == relaxed of
                                    true -> begin
                                        maps:get(no_match, Fm) bsl Pos,
                                        % TODO: unhandled stmt type
                                        ok                                    end;
                                    false -> ok
                                end,
                                error(<<"long name `", "` used with short delimiter `", "` in flag `", "` in ", " (POSIX/GNU) style parsing mode">>)
                            end;
                            false -> ok
                        end
                    end;
                    false -> ok
                end,
                case Flag_name1 == <<"">> of
                    true -> begin
                        case maps:get(mode, Config) == relaxed of
                            true -> begin
                                maps:get(no_match, Fm) bsl Pos,
                                % TODO: unhandled stmt type
                                ok                            end;
                            false -> ok
                        end,
                        error(<<"invalid delimiter-only flag `", "`">>)
                    end;
                    false -> ok
                end,
                Flag_ctx = #{raw => Arg, delimiter => Used_delimiter, name => Flag_name1, next => Next, pos => Pos, {vbeam, type} => 'FlagContext'},
                case Is_short_delimiter andalso lists:member(Style, [short, short_long]) of
                    true -> 'unknown.map_posix_short_cluster'(Fm, Flag_ctx);
                    false -> ok
                end,
                Pos_is_handled1 = lists:member(Pos, maps:get(handled_pos, Fm)),
                case Pos_is_handled1 of
                    true -> begin
                        trace_dbg_println(<<": skipping position \"", "\". Already handled">>),
                        % TODO: unhandled stmt type
                        ok                    end;
                    false -> ok
                end,
                lists:foreach(fun(Field) ->
                    case 'unknown.has'(maps:get(hints, Field), is_ignore) of
                        true -> begin
                            trace_dbg_println(<<": skipping field \"", "\" has an @[ignore] attribute">>),
                            % TODO: unhandled stmt type
                            ok                        end;
                        false -> ok
                    end.
                    case todo of
                        true -> begin
                            trace_dbg_println(<<": skipping field \"", "\" already identified">>),
                            % TODO: unhandled stmt type
                            ok                        end;
                        false -> ok
                    end.
                    trace_println(<<": matching `", "` ", " flag \"", "/", "\" is it matching \"", "\"?">>),
                    ok.
                    case 'unknown.has'(maps:get(hints, Field), short_only) of
                        true -> trace_println(<<": skipping long delimiter `", "` match for ", ".", " since it has [only: ", "]">>);
                        false -> ok
                    end.
                    case Is_short_delimiter of
                        true -> case lists:member(Style, [short, short_long]) of
                            true -> case 'unknown.map_posix_short'(Fm, Flag_ctx, Field) of
                                true -> ok;
                                false -> ok
                            end;
                            false -> case Style == v of
                                true -> case 'unknown.map_v'(Fm, Flag_ctx, Field) of
                                    true -> ok;
                                    false -> ok
                                end;
                                false -> case Style == v_flag_parser of
                                    true -> case 'unknown.map_v_flag_parser_short'(Fm, Flag_ctx, Field) of
                                        true -> ok;
                                        false -> ok
                                    end;
                                    false -> case Style == cmd_exe of
                                        true -> case 'unknown.map_cmd_exe'(Fm, Flag_ctx, Field) of
                                            true -> ok;
                                            false -> ok
                                        end;
                                        false -> case Style == go_flag of
                                            true -> case 'unknown.map_go_flag_short'(Fm, Flag_ctx, Field) of
                                                true -> ok;
                                                false -> ok
                                            end;
                                            false -> ok
                                        end
                                    end
                                end
                            end
                        end;
                        false -> ok
                    end.
                    case Is_long_delimiter of
                        true -> case lists:member(Style, [long, short_long]) of
                            true -> case 'unknown.map_gnu_long'(Fm, Flag_ctx, Field) of
                                true -> ok;
                                false -> ok
                            end;
                            false -> case Style == v_flag_parser of
                                true -> case 'unknown.map_v_flag_parser_long'(Fm, Flag_ctx, Field) of
                                    true -> ok;
                                    false -> ok
                                end;
                                false -> case Style == go_flag of
                                    true -> case 'unknown.map_go_flag_long'(Fm, Flag_ctx, Field) of
                                        true -> ok;
                                        false -> ok
                                    end;
                                    false -> ok
                                end
                            end
                        end;
                        false -> ok
                    end.
                    ok
                end, maps:get(fields, maps:get(si, Fm))),
            end;
            false -> ok
        end.
        case Pos >= Pos_last_flag + 1 of
            true -> begin
                trace_dbg_println(<<": (tail) looking for tail match for position \"", "\"...">>),
                Pos_is_handled2 = lists:member(Pos, maps:get(handled_pos, Fm)),
                case Pos_is_handled2 of
                    true -> begin
                        trace_dbg_println(<<": (tail) skipping position \"", "\". Already handled">>),
                        % TODO: unhandled stmt type
                        ok                    end;
                    false -> ok
                end,
                lists:foreach(fun(Field) ->
                    case 'unknown.has'(maps:get(hints, Field), is_ignore) of
                        true -> begin
                            trace_dbg_println(<<": (tail) skipping field \"", "\" has an @[ignore] attribute">>),
                            % TODO: unhandled stmt type
                            ok                        end;
                        false -> ok
                    end.
                    case todo of
                        true -> begin
                            trace_dbg_println(<<": (tail) skipping field \"", "\" already identified">>),
                            % TODO: unhandled stmt type
                            ok                        end;
                        false -> ok
                    end.
                    case 'unknown.has'(maps:get(hints, Field), has_tail) of
                        true -> begin
                            trace_dbg_println(<<": (tail) field \"", "\" has a tail attribute. fm.handled_pos.len: ">>),
                            Last_handled_pos = lists:nth(length(maps:get(handled_pos, Fm)) - 1 + 1, maps:get(handled_pos, Fm)),
                            trace_println(<<": (tail) flag `", "` last_handled_pos: ", " pos: ">>),
                            case Pos == Last_handled_pos + 1 orelse Pos == Pos_last_flag + 1 of
                                true -> begin
                                    case 'unknown.has'(maps:get(hints, Field), is_array) of
                                        true -> lists:nth(maps:get(name, Field) + 1, maps:get(array_field_map_flag, Fm)) bsl #{raw => Arg, field_name => maps:get(name, Field), arg => todo, pos => Pos, {vbeam, type} => 'FlagData'};
                                        false -> ok
                                    end,
                                    maps:get(handled_pos, Fm) bsl Pos,
                                    % TODO: unhandled stmt type
                                    ok                                end;
                                false -> ok
                            end
                        end;
                        false -> ok
                    end.
                    ok
                end, maps:get(fields, maps:get(si, Fm))),
            end;
            false -> ok
        end.
        case (not lists:member(Pos, maps:get(handled_pos, Fm))) andalso (not lists:member(Pos, maps:get(no_match, Fm))) of
            true -> begin
                maps:get(no_match, Fm) bsl Pos,
                case todo of
                    true -> error(<<"flag `", " ", "` is already mapped to field `", "` via `", " ", "`">>);
                    false -> ok
                end
            end;
            false -> ok
        end.
        ok
    end, Args),

'FlagMapper.to_doc'(Fm, Dc) ->
    Docs = [],
    Name_and_version = <<"">>,
    case 'Show.has'(maps:get(show, maps:get(options, Dc)), name) of
        true -> begin
            App_name = <<"">>,
            case todo of
                true -> ok;
                false -> ok
            end,
            case maps:get(name, Dc) /= <<"">> of
                true -> ok;
                false -> ok
            end,
            case App_name /= <<"">> of
                true -> ok;
                false -> ok
            end
        end;
        false -> ok
    end,
    case 'Show.has'(maps:get(show, maps:get(options, Dc)), version) of
        true -> begin
            App_version = <<"">>,
            case todo of
                true -> ok;
                false -> ok
            end,
            case maps:get(version, Dc) /= <<"">> of
                true -> ok;
                false -> ok
            end,
            case App_version /= <<"">> of
                true -> case Name_and_version /= <<"">> of
                    true -> ok;
                    false -> ok
                end;
                false -> ok
            end
        end;
        false -> ok
    end,
    case Name_and_version /= <<"">> of
        true -> Docs bsl Name_and_version;
        false -> ok
    end,
    case 'Show.has'(maps:get(show, maps:get(options, Dc)), description) of
        true -> begin
            Description = <<"">>,
            case todo of
                true -> ok;
                false -> ok
            end,
            case maps:get(description, Dc) /= <<"">> of
                true -> ok;
                false -> ok
            end,
            case Description /= <<"">> of
                true -> Docs bsl keep_at_max(Description, 'DocLayout.max_width'(maps:get(layout, Dc)));
                false -> ok
            end
        end;
        false -> ok
    end,
    case 'Show.has'(maps:get(show, maps:get(options, Dc)), flags) of
        true -> begin
            Fields_docs = 'FlagMapper.fields_docs'(Fm, Dc),
            case length(Fields_docs) > 0 of
                true -> begin
                    case 'Show.has'(maps:get(show, maps:get(options, Dc)), flags_header) of
                        true -> Docs bsl maps:get(flag_header, maps:get(options, Dc));
                        false -> ok
                    end,
                    Docs bsl Fields_docs
                end;
                false -> ok
            end
        end;
        false -> ok
    end,
    case 'Show.has'(maps:get(show, maps:get(options, Dc)), footer) of
        true -> begin
            Footer = <<"">>,
            case todo of
                true -> ok;
                false -> ok
            end,
            case maps:get(footer, Dc) /= <<"">> of
                true -> ok;
                false -> ok
            end,
            case Footer /= <<"">> of
                true -> Docs bsl keep_at_max(Footer, 'DocLayout.max_width'(maps:get(layout, Dc)));
                false -> ok
            end
        end;
        false -> ok
    end,
    case Name_and_version /= <<"">> of
        true -> begin
            Longest_line = 0,
            lists:foreach(fun(Doc_line) ->
                Lines = 'string.split'(Doc_line, <<"\\n">>),
                lists:foreach(fun(Line) ->
                    case length(Line) > Longest_line of
                        true -> ok;
                        false -> ok
                    end,
                    ok
                end, Lines),
                ok
            end, Docs),
            '[]string.insert'(Docs, 1, 'string.repeat'(<<"-">>, Longest_line))
        end;
        false -> ok
    end,
    '[]string.join'(Docs, <<"\\n">>).

'FlagMapper.fields_docs'(Fm, Dc) ->
    Short_delimiter = case maps:get(style, Dc) of
        short; short_long; v; v_flag_parser; go_flag; cmd_exe -> maps:get(delimiter, Dc);
        long -> 'string.repeat'(maps:get(delimiter, Dc), 2)
    end,
    Long_delimiter = case maps:get(style, Dc) of
        short; v; go_flag; cmd_exe -> maps:get(delimiter, Dc);
        long; v_flag_parser; short_long -> 'string.repeat'(maps:get(delimiter, Dc), 2)
    end,
    Pad_desc = case maps:get(description_padding, maps:get(layout, Dc)) < 0 of
        true -> 0;
        false -> maps:get(description_padding, maps:get(layout, Dc))
    end,
    Empty_padding = 'string.repeat'(<<" ">>, Pad_desc),
    Indent_flags = case maps:get(flag_indent, maps:get(layout, Dc)) < 0 of
        true -> 0;
        false -> maps:get(flag_indent, maps:get(layout, Dc))
    end,
    Indent_flags_padding = 'string.repeat'(<<" ">>, Indent_flags),
    Desc_max = case maps:get(description_width, maps:get(layout, Dc)) < 1 of
        true -> 1;
        false -> maps:get(description_width, maps:get(layout, Dc))
    end,
    Docs = [],
    lists:foreach(fun(Field) ->
        case 'FieldHints.has'(maps:get(hints, Field), is_ignore) of
            true -> begin
                trace_println(<<(todo)/binary, ": skipping field \"", (maps:get(name, Field))/binary, "\" has an @[ignore] attribute">>),
                % TODO: unhandled stmt type
                ok            end;
            false -> ok
        end,
        Doc = maps:get(maps:get(name, Field), maps:get(fields, Dc)),
        Short = 'StructField.shortest_match_name'(Field),
        Long = maps:get(match_name, Field),
        Flag_line = Indent_flags_padding,
        Flag_line1 = case Short /= <<"">> of
            true -> <<(Short_delimiter)/binary, (Short)/binary>>;
            false -> <<"">>
        end,
        case not 'FieldHints.has'(maps:get(hints, Field), short_only) andalso Long /= <<"">> of
            true -> begin
                case Short /= <<"">> of
                    true -> ok;
                    false -> ok
                end,
                Flag_line2 = <<(Long_delimiter)/binary, (Long)/binary>>,
            end;
            false -> ok
        end,
        case 'Show.has'(maps:get(show, maps:get(options, Dc)), flag_type) andalso maps:get(type_name, Field) /= <<"bool">> of
            true -> case not 'FieldHints.has'(maps:get(hints, Field), can_repeat) of
                true -> ok;
                false -> ok
            end;
            false -> ok
        end,
        case 'Show.has'(maps:get(show, maps:get(options, Dc)), flag_hint) of
            true -> begin
                case 'FieldHints.has'(maps:get(hints, Field), is_array) of
                    true -> ok;
                    false -> ok
                end,
                case 'FieldHints.has'(maps:get(hints, Field), can_repeat) of
                    true -> ok;
                    false -> ok
                end
            end;
            false -> ok
        end,
        Flag_line_diff = length(Flag_line2) - Pad_desc,
        case Flag_line_diff < 0 of
            true -> begin
                Diff = -Flag_line_diff,
                Line = <<(<<(Flag_line2)/binary, ('string.repeat'(<<" ">>, Diff))/binary>>)/binary, ('string.replace'(keep_at_max(Doc, Desc_max), <<"\\n">>, <<"\\n", (Empty_padding)/binary>>))/binary>>,
                Docs bsl 'string.trim_space_right'(Line)
            end;
            false -> begin
                Docs bsl 'string.trim_space_right'(Flag_line2),
                case Doc /= <<"">> of
                    true -> begin
                        Line1 = <<(Empty_padding)/binary, ('string.replace'(keep_at_max(Doc, Desc_max), <<"\\n">>, <<"\\n", (Empty_padding)/binary>>))/binary>>,
                        Docs bsl 'string.trim_space_right'(Line1)
                    end;
                    false -> ok
                end
            end
        end,
        case not maps:get(compact, maps:get(options, Dc)) of
            true -> Docs bsl <<"">>;
            false -> ok
        end,
        ok
    end, maps:get(fields, maps:get(si, Fm))),
    lists:foreach(fun(Doc) ->
        case 'string.starts_with'(Entry, maps:get(delimiter, Dc)) of
            true -> begin
                Flag_line_diff1 = length(Entry) - Pad_desc + Indent_flags,
                case Flag_line_diff1 < 0 of
                    true -> begin
                        Diff1 = -Flag_line_diff1,
                        Line2 = <<(<<(<<(Indent_flags_padding)/binary, ('string.trim'(Entry, <<" ">>))/binary>>)/binary, ('string.repeat'(<<" ">>, Diff1))/binary>>)/binary, ('string.replace'(keep_at_max(Doc, Desc_max), <<"\\n">>, <<"\\n", (Empty_padding)/binary>>))/binary>>,
                        Docs bsl 'string.trim_space_right'(Line2)
                    end;
                    false -> begin
                        Docs bsl <<(Indent_flags_padding)/binary, ('string.trim'(Entry, <<" ">>))/binary>>,
                        Line3 = <<(Empty_padding)/binary, ('string.replace'(keep_at_max(Doc, Desc_max), <<"\\n">>, <<"\\n", (Empty_padding)/binary>>))/binary>>,
                        Docs bsl 'string.trim_space_right'(Line3)
                    end
                end,
                case not maps:get(compact, maps:get(options, Dc)) of
                    true -> Docs bsl <<"">>;
                    false -> ok
                end
            end;
            false -> ok
        end,
        ok
    end, maps:get(fields, Dc)),
    case length(Docs) > 0 of
        true -> case not maps:get(compact, maps:get(options, Dc)) of
            true -> '[]string.delete_last'(Docs);
            false -> ok
        end;
        false -> ok
    end,
    Docs.

keep_at_max(Str, Max) ->
    Safe_max = case Max =< 0 of
        true -> 1;
        false -> Max
    end,
    case length(Str) =< Safe_max orelse 'string.count'(Str, <<" ">>) == 0 of
        true -> Str;
        false -> begin
            Fitted = <<"">>,
            Width = 0,
            Last_possible_break = 'string.index'(Str, <<" ">>),
            Never_touched = true,
            S = 'string.trim_space'(Str),
            lists:foreach(fun(C) ->
                todo,
                case C == todo of
                    true -> ok;
                    false -> case C == todo of
                        true -> ok;
                        false -> ok
                    end
                end,
                case Width == Safe_max of
                    true -> begin
                        Never_touched1 = false,
                        Fitted1 = <<(lists:nth(todo + 1, S))/binary, (<<"\\n">>)/binary>>,
                        Fitted2 = 'string.trim'(keep_at_max('string.trim'('string.replace'(lists:nth(todo + 1, S), <<"\\n">>, <<" ">>), <<" ">>), Safe_max), <<" ">>),
                    end;
                    false -> case Width > Safe_max of
                        true -> ok;
                        false -> ok
                    end
                end,
                ok
            end, S),
            case Never_touched1 of
                true -> Str;
                false -> Fitted2
                        end
        end
        end.

'FlagMapper.to_struct'(Fm, Defaults) ->
    Result = Defaults,
    The_default = Defaults,
    Result.

'FlagMapper.map_v'(Fm, Flag_ctx, Field) ->
    Flag_raw = maps:get(raw, Flag_ctx),
    Flag_name = maps:get(name, Flag_ctx),
    Pos = maps:get(pos, Flag_ctx),
    Used_delimiter = maps:get(delimiter, Flag_ctx),
    Next = maps:get(next, Flag_ctx),
    case 'FieldHints.has'(maps:get(hints, Field), is_bool) of
        true -> case Flag_name == maps:get(match_name, Field) orelse Flag_name == maps:get(short, Field) of
            true -> begin
                Arg = case 'string.contains'(Flag_raw, <<"=">>) of
                    true -> 'string.all_after'(Flag_raw, <<"=">>);
                    false -> <<"">>
                end,
                case Arg /= <<"">> of
                    true -> error(<<"flag `", (Flag_raw)/binary, "` can not be assigned to bool field \"", (maps:get(name, Field))/binary, "\"">>);
                    false -> ok
                end,
                trace_println(<<(todo)/binary, ": found match for (bool) ", ('FlagMapper.dbg_match'(Fm, Flag_ctx, Field, <<"true">>, <<"">>))/binary>>),
                maps:get(handled_pos, Fm) bsl Pos,
                true
            end;
            false -> ok
        end;
        false -> ok
    end,
    case Flag_name == maps:get(match_name, Field) orelse Flag_name == maps:get(short, Field) of
        true -> true;
        false -> false
        end.

'FlagMapper.map_v_flag_parser_short'(Fm, Flag_ctx, Field) ->
    Flag_raw = maps:get(raw, Flag_ctx),
    Flag_name = maps:get(name, Flag_ctx),
    Pos = maps:get(pos, Flag_ctx),
    Used_delimiter = maps:get(delimiter, Flag_ctx),
    Next = maps:get(next, Flag_ctx),
    case length(Flag_name) /= 1 of
        true -> error(<<"`", (Flag_raw)/binary, "` is not supported in V `flag.FlagParser` (short) style parsing mode. Only single character flag names are supported. Use `-f value` instead">>);
        false -> 
            case 'string.contains'(Flag_raw, <<"=">>) of
                true -> error(<<"`=` in flag `", (Flag_raw)/binary, "` is not supported in V `flag.FlagParser` (short) style parsing mode. Use `-f value` instead">>);
                false -> begin
                    case 'FieldHints.has'(maps:get(hints, Field), is_bool) of
                        true -> case Flag_name == maps:get(match_name, Field) orelse Flag_name == maps:get(short, Field) of
                            true -> begin
                                trace_println(<<(todo)/binary, ": found match for (bool) ", ('FlagMapper.dbg_match'(Fm, Flag_ctx, Field, <<"true">>, <<"">>))/binary>>),
                                maps:get(handled_pos, Fm) bsl Pos,
                                true
                            end;
                            false -> ok
                        end;
                        false -> ok
                    end,
                    case Flag_name == maps:get(match_name, Field) orelse Flag_name == maps:get(short, Field) of
                        true -> true;
                        false -> false
                                        end
                end
                        end
                end.

'FlagMapper.map_v_flag_parser_long'(Fm, Flag_ctx, Field) ->
    Flag_raw = maps:get(raw, Flag_ctx),
    Flag_name = maps:get(name, Flag_ctx),
    Pos = maps:get(pos, Flag_ctx),
    Used_delimiter = maps:get(delimiter, Flag_ctx),
    Next = maps:get(next, Flag_ctx),
    case 'string.contains'(Flag_raw, <<"=">>) of
        true -> error(<<"`=` in flag `", (Flag_raw)/binary, "` is not supported in V `flag.FlagParser` (long) style parsing mode. Use `--flag value` instead">>);
        false -> begin
            case 'FieldHints.has'(maps:get(hints, Field), is_bool) of
                true -> case Flag_name == maps:get(match_name, Field) of
                    true -> begin
                        trace_println(<<(todo)/binary, ": found match for (bool) ", ('FlagMapper.dbg_match'(Fm, Flag_ctx, Field, <<"true">>, <<"">>))/binary>>),
                        maps:get(handled_pos, Fm) bsl Pos,
                        true
                    end;
                    false -> ok
                end;
                false -> ok
            end,
            case Flag_name == maps:get(match_name, Field) orelse Flag_name == maps:get(short, Field) of
                true -> true;
                false -> false
                        end
        end
        end.

'FlagMapper.map_go_flag_short'(Fm, Flag_ctx, Field) ->
    Flag_raw = maps:get(raw, Flag_ctx),
    Flag_name = maps:get(name, Flag_ctx),
    Pos = maps:get(pos, Flag_ctx),
    Used_delimiter = maps:get(delimiter, Flag_ctx),
    Next = maps:get(next, Flag_ctx),
    case 'FieldHints.has'(maps:get(hints, Field), is_bool) of
        true -> case Flag_name == maps:get(match_name, Field) of
            true -> begin
                Arg = case 'string.contains'(Flag_raw, <<"=">>) of
                    true -> 'string.all_after'(Flag_raw, <<"=">>);
                    false -> <<"">>
                end,
                case Arg /= <<"">> of
                    true -> error(<<"flag `", (Flag_raw)/binary, "` can not be assigned to bool field \"", (maps:get(name, Field))/binary, "\"">>);
                    false -> ok
                end,
                trace_println(<<(todo)/binary, ": found match for (bool) ", ('FlagMapper.dbg_match'(Fm, Flag_ctx, Field, <<"true">>, <<"">>))/binary>>),
                maps:get(handled_pos, Fm) bsl Pos,
                true
            end;
            false -> ok
        end;
        false -> ok
    end,
    case Flag_name == maps:get(match_name, Field) orelse Flag_name == maps:get(short, Field) of
        true -> true;
        false -> false
        end.

'FlagMapper.map_go_flag_long'(Fm, Flag_ctx, Field) ->
    Flag_raw = maps:get(raw, Flag_ctx),
    Flag_name = maps:get(name, Flag_ctx),
    Pos = maps:get(pos, Flag_ctx),
    Used_delimiter = maps:get(delimiter, Flag_ctx),
    case Flag_name == maps:get(match_name, Field) of
        true -> true;
        false -> false
        end.

'FlagMapper.map_gnu_long'(Fm, Flag_ctx, Field) ->
    Flag_raw = maps:get(raw, Flag_ctx),
    Flag_name = maps:get(name, Flag_ctx),
    Pos = maps:get(pos, Flag_ctx),
    Used_delimiter = maps:get(delimiter, Flag_ctx),
    case Flag_name == maps:get(match_name, Field) of
        true -> true;
        false -> false
        end.

'FlagMapper.map_posix_short_cluster'(Fm, Flag_ctx) ->
    Flag_name = maps:get(name, Flag_ctx),
    case length(Flag_name) =< 1 of
        true -> ok;
        false -> 
            case lists:nth(1, Flag_name) == lists:nth(2, Flag_name) of
                true -> ok;
                false -> begin
                    case length(Flag_name) > 1 of
                        true -> begin
                            Split = 'string.split'(Flag_name, <<"">>),
                            Matched_fields = #{},
                            lists:foreach(fun(Mflag) ->
                                Matched = false,
                                lists:foreach(fun(Field) ->
                                    case todo of
                                        true -> case Mflag == Smatch_name of
                                            true -> begin
                                                trace_println(<<(todo)/binary, ": cluster flag `", (Mflag)/binary, "` matches field `", (Smatch_name)/binary, "`">>),
                                                Matched1 = true,
                                            end;
                                            false -> ok
                                        end;
                                        false -> ok
                                    end,
                                    ok
                                end, maps:get(fields, maps:get(si, Fm))),
                                case not Matched1 of
                                    true -> ok;
                                    false -> ok
                                end,
                                ok
                            end, Split),
                            case maps:size(Matched_fields) == 0 of
                                true -> ok;
                                false -> ok
                            end,
                            % TODO: unhandled stmt type
                            ok                        end;
                        false -> ok
                    end,
                    ok
                end
                        end
                end.

'FlagMapper.map_posix_short'(Fm, Flag_ctx, Field) ->
    Flag_raw = maps:get(raw, Flag_ctx),
    Flag_name = maps:get(name, Flag_ctx),
    Pos = maps:get(pos, Flag_ctx),
    Used_delimiter = maps:get(delimiter, Flag_ctx),
    Next = maps:get(next, Flag_ctx),
    Struct_name = maps:get(name, maps:get(si, Fm)),
    First_letter = lists:nth(1, 'string.split'(Flag_name, <<"">>)),
    Next_first_letter = case Next /= <<"">> of
        true -> lists:nth(1, 'string.split'(Next, <<"">>));
        false -> <<"">>
    end,
    Count_of_first_letter_repeats = 'string.count'(Flag_name, First_letter),
    Count_of_next_first_letter_repeats = 'string.count'(Next, Next_first_letter),
    case 'FieldHints.has'(maps:get(hints, Field), is_bool) of
        true -> begin
            case Flag_name == maps:get(match_name, Field) of
                true -> begin
                    Arg = case 'string.contains'(Flag_raw, <<"=">>) of
                        true -> 'string.all_after'(Flag_raw, <<"=">>);
                        false -> <<"">>
                    end,
                    case Arg /= <<"">> of
                        true -> error(<<"flag `", (Flag_raw)/binary, "` can not be assigned to bool field \"", (maps:get(name, Field))/binary, "\"">>);
                        false -> ok
                    end,
                    trace_println(<<(todo)/binary, ": found match for (bool) ", ('FlagMapper.dbg_match'(Fm, Flag_ctx, Field, <<"true">>, <<"">>))/binary>>),
                    maps:get(handled_pos, Fm) bsl Pos,
                    true
                end;
                false -> ok
            end,
            case maps:get(short, Field) == Flag_name of
                true -> begin
                    trace_println(<<(todo)/binary, ": found match for (bool) ", ('FlagMapper.dbg_match'(Fm, Flag_ctx, Field, <<"true">>, <<"">>))/binary>>),
                    maps:get(handled_pos, Fm) bsl Pos,
                    true
                end;
                false -> ok
            end
        end;
        false -> ok
    end,
    case First_letter == maps:get(short, Field) of
        true -> case 'FieldHints.has'(maps:get(hints, Field), can_repeat) of
            true -> begin
                Do_continue = false,
                case Count_of_first_letter_repeats == length(Flag_name) of
                    true -> begin
                        trace_println(<<(todo)/binary, ": found match for (repeatable) ", ('FlagMapper.dbg_match'(Fm, Flag_ctx, Field, <<"true">>, <<"">>))/binary>>),
                        maps:get(handled_pos, Fm) bsl Pos,
                        Do_continue1 = true,
                        case Next_first_letter == First_letter andalso Count_of_next_first_letter_repeats == length(Next) of
                            true -> begin
                                trace_println(<<(todo)/binary, ": field \"", (maps:get(name, Field))/binary, "\" allow repeats and ", (Flag_raw)/binary, " ", (Next)/binary, " repeats ", (integer_to_binary(Count_of_next_first_letter_repeats + Count_of_first_letter_repeats))/binary, " times (via argument)">>),
                                maps:get(handled_pos, Fm) bsl Pos,
                                maps:get(handled_pos, Fm) bsl Pos + 1,
                                Do_continue2 = true,
                            end;
                            false -> trace_println(<<(todo)/binary, ": field \"", (maps:get(name, Field))/binary, "\" allow repeats and ", (Flag_raw)/binary, " repeats ", (integer_to_binary(Count_of_first_letter_repeats))/binary, " times">>)
                        end,
                        case Do_continue2 of
                            true -> true;
                            false -> ok
                        end
                    end;
                    false -> ok
                end
            end;
            false -> case 'FieldHints.has'(maps:get(hints, Field), is_array) of
                true -> begin
                    Split = 'string.trim_string_left'(Flag_name, maps:get(short, Field)),
                    Next_is_handled = true,
                    case Split /= <<"">> of
                        true -> begin
                            Next1 = Split,
                            Flag_name1 = 'string.trim_string_right'(Flag_name, Split),
                            Next_is_handled1 = false,
                        end;
                        false -> ok
                    end,
                    case Next1 == <<"">> of
                        true -> error(<<"flag \"", (Flag_raw)/binary, "\" expects an argument">>);
                        false -> ok
                    end,
                    trace_println(<<(todo)/binary, ": found match for (multiple occurrences) ", ('FlagMapper.dbg_match'(Fm, Flag_ctx, Field, Next1, <<"">>))/binary>>),
                    maps:get(maps:get(name, Field), maps:get(array_field_map_flag, Fm)) bsl #{raw => Flag_raw, field_name => maps:get(name, Field), delimiter => Used_delimiter, name => Flag_name1, arg => todo, pos => Pos, {vbeam, type} => 'FlagData'},
                    maps:get(handled_pos, Fm) bsl Pos,
                    case Next_is_handled1 of
                        true -> maps:get(handled_pos, Fm) bsl Pos + 1;
                        false -> ok
                    end,
                    true
                end;
                false -> case not 'string.starts_with'(maps:get(next, Flag_ctx), Used_delimiter) of
                    true -> case maps:get(short, Field) == Flag_name1 of
                        true -> begin
                            trace_println(<<(todo)/binary, ": found match for (", (maps:get(type_name, Field))/binary, ") ", ('FlagMapper.dbg_match'(Fm, Flag_ctx, Field, Next1, <<"">>))/binary>>),
                            maps:get(handled_pos, Fm) bsl Pos,
                            maps:get(handled_pos, Fm) bsl Pos + 1,
                            true
                        end;
                        false -> ok
                    end;
                    false -> ok
                end
            end
        end;
        false -> ok
    end,
    case (maps:get(style, maps:get(config, Fm)) == short orelse 'FieldHints.has'(maps:get(hints, Field), short_only)) andalso First_letter == maps:get(short, Field) of
        true -> begin
            Split1 = 'string.trim_string_left'(Flag_name1, maps:get(short, Field)),
            Next_is_handled2 = true,
            case Split1 /= <<"">> of
                true -> begin
                    Next2 = Split1,
                    Flag_name2 = 'string.trim_string_right'(Flag_name1, Split1),
                    Next_is_handled3 = false,
                end;
                false -> ok
            end,
            case Next2 == <<"">> of
                true -> error(<<"flag \"", (Flag_raw)/binary, "\" expects an argument">>);
                false -> ok
            end,
            trace_println(<<(todo)/binary, ": found match for (short only) ", (Struct_name)/binary, ".", (maps:get(name, Field))/binary, " (", (maps:get(match_name, Field))/binary, ") = ", (maps:get(short, Field))/binary, " = ", (Next2)/binary>>),
            maps:get(handled_pos, Fm) bsl Pos,
            case Next_is_handled3 of
                true -> maps:get(handled_pos, Fm) bsl Pos + 1;
                false -> ok
            end,
            true
        end;
        false -> case Flag_name2 == maps:get(match_name, Field) andalso not ('FieldHints.has'(maps:get(hints, Field), short_only) andalso Flag_name2 == maps:get(short, Field)) of
            true -> begin
                trace_println(<<(todo)/binary, ": found match for (repeats) ", ('FlagMapper.dbg_match'(Fm, Flag_ctx, Field, Next2, <<"">>))/binary>>),
                case Next2 == <<"">> of
                    true -> error(<<"flag \"", (Flag_raw)/binary, "\" expects an argument">>);
                    false -> ok
                end,
                maps:get(handled_pos, Fm) bsl Pos,
                maps:get(handled_pos, Fm) bsl Pos + 1,
                true
            end;
            false -> ok
        end
    end,
    false.

'FlagMapper.map_cmd_exe'(Fm, Flag_ctx, Field) ->
    Flag_raw = maps:get(raw, Flag_ctx),
    Flag_name = maps:get(name, Flag_ctx),
    Pos = maps:get(pos, Flag_ctx),
    Used_delimiter = maps:get(delimiter, Flag_ctx),
    Next = maps:get(next, Flag_ctx),
    case Flag_name == maps:get(match_name, Field) of
        true -> true;
        false -> begin
            case todo of
                true -> case Flag_name == Shortest_match_name of
                    true -> begin
                        case 'FieldHints.has'(maps:get(hints, Field), is_bool) of
                            true -> begin
                                trace_println(<<(todo)/binary, ": found match for (bool) (CMD.EXE style) ", ('FlagMapper.dbg_match'(Fm, Flag_ctx, Field, <<"true">>, <<"">>))/binary>>),
                                maps:get(handled_pos, Fm) bsl Pos,
                                true
                            end;
                            false -> ok
                        end,
                        case 'FieldHints.has'(maps:get(hints, Field), is_array) of
                            true -> begin
                                trace_println(<<(todo)/binary, ": found match for (CMD.EXE style multiple occurrences) ", ('FlagMapper.dbg_match'(Fm, Flag_ctx, Field, Next, <<"">>))/binary>>),
                                maps:get(maps:get(name, Field), maps:get(array_field_map_flag, Fm)) bsl #{raw => Flag_raw, field_name => maps:get(name, Field), delimiter => Used_delimiter, name => Flag_name, arg => todo, pos => Pos, {vbeam, type} => 'FlagData'}
                            end;
                            false -> begin
                                trace_println(<<(todo)/binary, ": found match for (CMD.EXE style) ", ('FlagMapper.dbg_match'(Fm, Flag_ctx, Field, Next, <<"">>))/binary>>),
                            end
                        end,
                        maps:get(handled_pos, Fm) bsl Pos,
                        maps:get(handled_pos, Fm) bsl Pos + 1,
                        true
                    end;
                    false -> ok
                end;
                false -> ok
            end,
            false
        end
        end.

'ParseMode__static__from'(Input) ->
    error(<<"invalid value">>).

'Style__static__from'(Input) ->
    error(<<"invalid value">>).

'FieldHints.is_empty'(E) ->
    todo == 0.

'FieldHints.has'(E, Flag_) ->
    (todo band (todo)) /= 0.

'FieldHints.all'(E, Flag_) ->
    (todo band (todo)) == todo.

'FieldHints.set'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'FieldHints.set_all'(E) ->
    % TODO: unhandled stmt type
    ok
'FieldHints.clear'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'FieldHints.clear_all'(E) ->
    % TODO: unhandled stmt type
    ok
'FieldHints.toggle'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'FieldHints__static__zero'() ->
    todo.

'FieldHints__static__from'(Input) ->
    error(<<"invalid value">>).

'Show.is_empty'(E) ->
    todo == 0.

'Show.has'(E, Flag_) ->
    (todo band (todo)) /= 0.

'Show.all'(E, Flag_) ->
    (todo band (todo)) == todo.

'Show.set'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'Show.set_all'(E) ->
    % TODO: unhandled stmt type
    ok
'Show.clear'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'Show.clear_all'(E) ->
    % TODO: unhandled stmt type
    ok
'Show.toggle'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'Show__static__zero'() ->
    todo.

'Show__static__from'(Input) ->
    error(<<"invalid value">>).
