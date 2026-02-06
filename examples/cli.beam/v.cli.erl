-module('v.cli').
-export(['FnCommandCallback.str'/1, 'Command.str'/1, 'Command.is_root'/1, 'Command.root'/1, 'Command.full_name'/1, 'Command.add_commands'/2, 'Command.add_command'/2, 'Command.setup'/1, 'Command.add_flags'/2, 'Command.add_flag'/2, 'Command.parse_defaults'/1, 'Command.parse'/2, 'Command.add_default_flags'/1, 'Command.add_default_commands'/1, 'Command.parse_flags'/1, 'Command.parse_commands'/1, 'Command.handle_cb'/3, 'Command.check_help_flag'/1, 'Command.check_man_flag'/1, 'Command.check_version_flag'/1, 'Command.check_required_flags'/1, 'Command.execute_help'/1, 'Command.execute_man'/1, 'Command.get'/2, 'Command.contains'/2, eprintln_exit/1, 'Flag.get_all_found'/1, 'Flag.get_bool'/1, 'Flag.get_bool'/2, 'Flag.get_int'/1, 'Flag.get_ints'/1, 'Flag.get_int'/2, 'Flag.get_ints'/2, 'Flag.get_float'/1, 'Flag.get_floats'/1, 'Flag.get_float'/2, 'Flag.get_floats'/2, 'Flag.get_string'/1, 'Flag.get_strings'/1, 'Flag.get_string'/2, 'Flag.get_strings'/2, 'Flag.parse'/3, 'Flag.matches'/3, 'Flag.parse_raw'/2, 'Flag.parse_bool'/2, 'Flag.get'/2, 'Flag.contains'/2, 'Flag.get_value_or_default_value'/1, help_flag/1, help_cmd/0, print_help_for_command/1, 'Command.help_message'/1, pretty_description/2, max/2, man_flag/0, man_cmd/0, print_manpage_for_command/1, 'Command.manpage'/1, version_flag/1, version_cmd/0, print_version_for_command/1, 'Command.version'/1, 'FlagType__static__from'/1]).

'FnCommandCallback.str'(F) ->
    <<(<<"FnCommandCallback=>">>)/binary, (ptr_str(F))/binary>>.

'Command.str'(Cmd) ->
    Res = [],
    Res bsl <<"Command{">>,
    Res bsl <<"\tname: \"", (maps:get(name, Cmd))/binary, "\"">>,
    Res bsl <<"\tusage: \"", (maps:get(usage, Cmd))/binary, "\"">>,
    Res bsl <<"\tversion: \"", (maps:get(version, Cmd))/binary, "\"">>,
    Res bsl <<"\tdescription: \"", (maps:get(description, Cmd))/binary, "\"">>,
    Res bsl <<"\tman_description: \"", (maps:get(man_description, Cmd))/binary, "\"">>,
    Res bsl <<"\tdisable_flags: ", (atom_to_binary(maps:get(disable_flags, Cmd)))/binary>>,
    Res bsl <<"\tsort_flags: ", (atom_to_binary(maps:get(sort_flags, Cmd)))/binary>>,
    Res bsl <<"\tsort_commands: ", (atom_to_binary(maps:get(sort_commands, Cmd)))/binary>>,
    Res bsl <<"\tcb execute: ", (maps:get(execute, Cmd))/binary>>,
    Res bsl <<"\tcb pre_execute: ", (maps:get(pre_execute, Cmd))/binary>>,
    Res bsl <<"\tcb post_execute: ", (maps:get(post_execute, Cmd))/binary>>,
    case todo of
        true -> Res bsl <<"\tparent: &Command(0)">>;
        false -> Res bsl <<"\tparent: &Command{", (maps:get(name, maps:get(parent, Cmd)))/binary, " ...}">>
    end,
    Res bsl <<"\tcommands: ", (maps:get(commands, Cmd))/binary>>,
    Res bsl <<"\tflags: ", (maps:get(flags, Cmd))/binary>>,
    Res bsl <<"\trequired_args: ", (integer_to_binary(maps:get(required_args, Cmd)))/binary>>,
    Res bsl <<"\targs: ", (maps:get(args, Cmd))/binary>>,
    Res bsl <<"\tposix_mode: ", (atom_to_binary(maps:get(posix_mode, Cmd)))/binary>>,
    case maps:get(help, maps:get(defaults, Cmd)) of
        todo -> Res bsl <<"\tdefaults.help: ", (atom_to_binary(maps:get(help, maps:get(defaults, Cmd))))/binary>>;
        todo -> begin
            Res bsl <<"\tdefaults.help.command: ", (atom_to_binary(maps:get(command, maps:get(help, maps:get(defaults, Cmd)))))/binary>>,
            Res bsl <<"\tdefaults.help.flag: ", (atom_to_binary(maps:get(flag, maps:get(help, maps:get(defaults, Cmd)))))/binary>>
        end
    end,
    case maps:get(man, maps:get(defaults, Cmd)) of
        todo -> Res bsl <<"\tdefaults.man: ", (atom_to_binary(maps:get(man, maps:get(defaults, Cmd))))/binary>>;
        todo -> begin
            Res bsl <<"\tdefaults.man.command: ", (atom_to_binary(maps:get(command, maps:get(man, maps:get(defaults, Cmd)))))/binary>>,
            Res bsl <<"\tdefaults.man.flag: ", (atom_to_binary(maps:get(flag, maps:get(man, maps:get(defaults, Cmd)))))/binary>>
        end
    end,
    case maps:get(version, maps:get(defaults, Cmd)) of
        todo -> Res bsl <<"\tdefaults.version: ", (atom_to_binary(maps:get(version, maps:get(defaults, Cmd))))/binary>>;
        todo -> begin
            Res bsl <<"\tdefaults.version.command: ", (atom_to_binary(maps:get(command, maps:get(version, maps:get(defaults, Cmd)))))/binary>>,
            Res bsl <<"\tdefaults.version.flag: ", (atom_to_binary(maps:get(flag, maps:get(version, maps:get(defaults, Cmd)))))/binary>>
        end
    end,
    Res bsl <<"}">>,
    '[]string.join'(Res, <<"\\n">>).

'Command.is_root'(Cmd) ->
    isnil(maps:get(parent, Cmd)).

'Command.root'(Cmd) ->
    case 'Command.is_root'(Cmd) of
        true -> *Cmd;
        false -> 'Command.root'(maps:get(parent, Cmd))
        end.

'Command.full_name'(Cmd) ->
    case 'Command.is_root'(Cmd) of
        true -> maps:get(name, Cmd);
        false -> <<('Command.full_name'(maps:get(parent, Cmd)))/binary, (<<" ", (maps:get(name, Cmd))/binary>>)/binary>>
        end.

'Command.add_commands'(Cmd, Commands) ->
    lists:foreach(fun(Command) ->
        'Command.add_command'(Cmd, Command),
        ok.
        ok
    end, Commands),

'Command.add_command'(Cmd, Command) ->
    Subcmd = Command,
    case 'Command.contains'(maps:get(commands, Cmd), maps:get(name, Subcmd)) of
        true -> eprintln_exit(<<"Command with the name `", (maps:get(name, Subcmd))/binary, "` already exists">>);
        false -> ok
    end,
    maps:get(commands, Cmd) bsl Subcmd,
    ok.

'Command.setup'(Cmd) ->
    lists:foreach(fun(Subcmd) ->
        'Command.setup'(Subcmd),
        ok.
        ok
    end, maps:get(commands, Cmd)),

'Command.add_flags'(Cmd, Flags) ->
    lists:foreach(fun(Flag) ->
        'Command.add_flag'(Cmd, Flag),
        ok.
        ok
    end, Flags),

'Command.add_flag'(Cmd, Flag) ->
    case 'Flag.contains'(maps:get(flags, Cmd), maps:get(name, Flag)) of
        true -> eprintln_exit(<<"Flag with the name `", (maps:get(name, Flag))/binary, "` already exists">>);
        false -> ok
    end,
    maps:get(flags, Cmd) bsl Flag,
    ok.

'Command.parse_defaults'(Cmd) ->
    case maps:get(help, maps:get(defaults, Cmd)) is todo of
        true -> begin
        end;
        false -> case maps:get(help, maps:get(defaults, Cmd)) is todo of
            true -> begin
            end;
            false -> ok
        end
    end,
    case maps:get(version, maps:get(defaults, Cmd)) is todo of
        true -> begin
        end;
        false -> case maps:get(version, maps:get(defaults, Cmd)) is todo of
            true -> begin
            end;
            false -> ok
        end
    end,
    case maps:get(man, maps:get(defaults, Cmd)) is todo of
        true -> begin
        end;
        false -> case maps:get(man, maps:get(defaults, Cmd)) is todo of
            true -> begin
            end;
            false -> ok
        end
    end,
    case not maps:get(disable_flags, Cmd) of
        true -> 'Command.add_default_flags'(Cmd);
        false -> ok
    end,
    'Command.add_default_commands'(Cmd),
    ok.

'Command.parse'(Cmd, Args) ->
    'Command.parse_defaults'(Cmd),
    case maps:get(sort_flags, Cmd) of
        true -> 'Flag.sort'(maps:get(flags, Cmd), maps:get(name, A) < maps:get(name, B));
        false -> ok
    end,
    case maps:get(sort_commands, Cmd) of
        true -> 'Command.sort'(maps:get(commands, Cmd), maps:get(name, A) < maps:get(name, B));
        false -> ok
    end,
    case not maps:get(disable_flags, Cmd) of
        true -> 'Command.parse_flags'(Cmd);
        false -> ok
    end,
    'Command.parse_commands'(Cmd),
    ok.

'Command.add_default_flags'(Cmd) ->
    case maps:get(flag, maps:get(help, maps:get(parsed, maps:get(defaults, Cmd)))) andalso not 'Flag.contains'(maps:get(flags, Cmd), <<"help">>) of
        true -> begin
            Use_help_abbrev = not 'Flag.contains'(maps:get(flags, Cmd), <<"h">>) andalso maps:get(posix_mode, Cmd),
            'Command.add_flag'(Cmd, help_flag(Use_help_abbrev))
        end;
        false -> ok
    end,
    case maps:get(flag, maps:get(version, maps:get(parsed, maps:get(defaults, Cmd)))) andalso maps:get(version, Cmd) /= <<"">> andalso not 'Flag.contains'(maps:get(flags, Cmd), <<"version">>) of
        true -> begin
            Use_version_abbrev = not 'Flag.contains'(maps:get(flags, Cmd), <<"v">>) andalso maps:get(posix_mode, Cmd),
            'Command.add_flag'(Cmd, version_flag(Use_version_abbrev))
        end;
        false -> ok
    end,
    case maps:get(flag, maps:get(man, maps:get(parsed, maps:get(defaults, Cmd)))) andalso not 'Flag.contains'(maps:get(flags, Cmd), <<"man">>) of
        true -> 'Command.add_flag'(Cmd, man_flag());
        false -> ok
    end.

'Command.add_default_commands'(Cmd) ->
    case maps:get(command, maps:get(help, maps:get(parsed, maps:get(defaults, Cmd)))) andalso not 'Command.contains'(maps:get(commands, Cmd), <<"help">>) andalso 'Command.is_root'(Cmd) of
        true -> 'Command.add_command'(Cmd, help_cmd());
        false -> ok
    end,
    case maps:get(command, maps:get(version, maps:get(parsed, maps:get(defaults, Cmd)))) andalso maps:get(version, Cmd) /= <<"">> andalso not 'Command.contains'(maps:get(commands, Cmd), <<"version">>) of
        true -> 'Command.add_command'(Cmd, version_cmd());
        false -> ok
    end,
    case maps:get(command, maps:get(man, maps:get(parsed, maps:get(defaults, Cmd)))) andalso not 'Command.contains'(maps:get(commands, Cmd), <<"man">>) andalso 'Command.is_root'(Cmd) of
        true -> 'Command.add_command'(Cmd, man_cmd());
        false -> ok
    end.

'Command.parse_flags'(Cmd) ->
    % TODO: unhandled stmt type
    ok
'Command.parse_commands'(Cmd) ->
    Global_flags = 'Flag.filter'(maps:get(flags, Cmd), maps:get(global, It)),
    'Command.check_help_flag'(Cmd),
    'Command.check_version_flag'(Cmd),
    'Command.check_man_flag'(Cmd),
    Arg = lists:foldl(fun(I, ArgAcc) ->
        ArgOut = lists:nth(I + 1, maps:get(args, Cmd)),
        Command = lists:foldl(fun(J, CommandAcc) ->
            CommandOut = lists:nth(J + 1, maps:get(commands, Cmd)),
            case maps:get(name, Command) == Arg of
                true -> begin
                    lists:foreach(fun(Flag) ->
                        'Command.add_flag'(Command, Flag),
                        ok
                    end, Global_flags),
                    'Command.parse'(Command, lists:nth(todo + 1, maps:get(args, Cmd))),
                end;
                false -> ok
            end,
            CommandOut
        end, Command, lists:seq(0, length(maps:get(commands, Cmd)) - 1)),
        ArgOut
    end, Arg, lists:seq(0, length(maps:get(args, Cmd)) - 1)),
    case 'Command.is_root'(Cmd) andalso isnil(maps:get(execute, Cmd)) of
        true -> case maps:get(command, maps:get(help, maps:get(parsed, maps:get(defaults, Cmd)))) of
            true -> begin
                'Command.execute_help'(Cmd),
            end;
            false -> ok
        end;
        false -> ok
    end,
    case maps:get(required_args, Cmd) > 0 of
        true -> case maps:get(required_args, Cmd) > length(maps:get(args, Cmd)) of
            true -> begin
                Descriptor = case maps:get(required_args, Cmd) == 1 of
                    true -> <<"argument">>;
                    false -> <<"arguments">>
                end,
                eprintln_exit(<<"Command `", (maps:get(name, Cmd))/binary, "` needs at least ", (integer_to_binary(maps:get(required_args, Cmd)))/binary, " ", (Descriptor)/binary>>)
            end;
            false -> ok
        end;
        false -> ok
    end,
    'Command.check_required_flags'(Cmd),
    'Command.handle_cb'(Cmd, maps:get(pre_execute, Cmd), <<"preexecution">>),
    'Command.handle_cb'(Cmd, maps:get(execute, Cmd), <<"execution">>),
    'Command.handle_cb'(Cmd, maps:get(post_execute, Cmd), <<"postexecution">>),
    ok.

'Command.handle_cb'(Cmd, Cb, Label) ->
    case not isnil(Cb) of
        true -> cb(*Cmd);
        false -> ok
    end.

'Command.check_help_flag'(Cmd) ->
    case maps:get(flag, maps:get(help, maps:get(parsed, maps:get(defaults, Cmd)))) andalso 'Flag.contains'(maps:get(flags, Cmd), <<"help">>) of
        true -> begin
            Help_flag = 'Flag.get_bool'(maps:get(flags, Cmd), <<"help">>),
            case Help_flag of
                true -> begin
                    'Command.execute_help'(Cmd),
                    exit(0)
                end;
                false -> ok
            end
        end;
        false -> ok
    end.

'Command.check_man_flag'(Cmd) ->
    case maps:get(flag, maps:get(man, maps:get(parsed, maps:get(defaults, Cmd)))) andalso 'Flag.contains'(maps:get(flags, Cmd), <<"man">>) of
        true -> begin
            Man_flag = 'Flag.get_bool'(maps:get(flags, Cmd), <<"man">>),
            case Man_flag of
                true -> begin
                    'Command.execute_man'(Cmd),
                    exit(0)
                end;
                false -> ok
            end
        end;
        false -> ok
    end.

'Command.check_version_flag'(Cmd) ->
    case maps:get(flag, maps:get(version, maps:get(parsed, maps:get(defaults, Cmd)))) andalso maps:get(version, Cmd) /= <<"">> andalso 'Flag.contains'(maps:get(flags, Cmd), <<"version">>) of
        true -> begin
            Version_flag = 'Flag.get_bool'(maps:get(flags, Cmd), <<"version">>),
            case Version_flag of
                true -> begin
                    print_version_for_command(Cmd),
                    exit(0)
                end;
                false -> ok
            end
        end;
        false -> ok
    end.

'Command.check_required_flags'(Cmd) ->
    lists:foreach(fun(Flag) ->
        case maps:get(required, Flag) andalso length(maps:get(value, Flag)) == 0 of
            true -> begin
                Full_name = 'Command.full_name'(Cmd),
                eprintln_exit(<<"Flag `", (maps:get(name, Flag))/binary, "` is required by `", (Full_name)/binary, "`">>)
            end;
            false -> ok
        end.
        ok
    end, maps:get(flags, Cmd)),

'Command.execute_help'(Cmd) ->
    case 'Command.contains'(maps:get(commands, Cmd), <<"help">>) of
        true -> begin
            Help_cmd = 'Command.get'(maps:get(commands, Cmd), <<"help">>),
            case not isnil(maps:get(execute, Help_cmd)) of
                true -> begin
                    'Command.execute'(Help_cmd, Help_cmd),
                end;
                false -> ok
            end
        end;
        false -> ok
    end,
    print('Command.help_message'(Cmd)),
    ok.

'Command.execute_man'(Cmd) ->
    case 'Command.contains'(maps:get(commands, Cmd), <<"man">>) of
        true -> begin
            Man_cmd = 'Command.get'(maps:get(commands, Cmd), <<"man">>),
            'Command.execute'(Man_cmd, Man_cmd)
        end;
        false -> print('Command.manpage'(Cmd))
    end.

'Command.get'(Cmds, Name) ->
    lists:foreach(fun(Cmd) ->
        case maps:get(name, Cmd) == Name of
            true -> Cmd;
            false -> ok
        end,
        ok
    end, Cmds),
    error(<<"Command `", (Name)/binary, "` not found in ", (Cmds)/binary>>).

'Command.contains'(Cmds, Name) ->
    lists:foreach(fun(Cmd) ->
        case maps:get(name, Cmd) == Name of
            true -> true;
            false -> ok
        end,
        ok
    end, Cmds),
    false.

eprintln_exit(Message) ->
    eprintln(Message),
    exit(1),
    ok.

'Flag.get_all_found'(Flags) ->
    'Flag.filter'(Flags, maps:get(found, It)).

'Flag.get_bool'(Flag) ->
    case maps:get(flag, Flag) /= bool of
        true -> error(<<(maps:get(name, Flag))/binary, ": Invalid flag type `", (maps:get(flag, Flag))/binary, "`, expected `bool`">>);
        false -> begin
            Val = 'Flag.get_value_or_default_value'(Flag),
            length(Val) > 0 andalso lists:nth(1, Val) == <<"true">>
        end
        end.

'Flag.get_bool'(Flags, Name) ->
    Flag = 'Flag.get'(Flags, Name),
    'Flag.get_bool'(Flag).

'Flag.get_int'(Flag) ->
    case maps:get(flag, Flag) /= int of
        true -> error(<<(maps:get(name, Flag))/binary, ": Invalid flag type `", (maps:get(flag, Flag))/binary, "`, expected `int`">>);
        false -> begin
            Val = 'Flag.get_value_or_default_value'(Flag),
            case length(Val) == 0 of
                true -> 0;
                false -> 'string.int'(lists:nth(1, Val))
            end
        end
        end.

'Flag.get_ints'(Flag) ->
    case maps:get(flag, Flag) /= int_array of
        true -> error(<<(maps:get(name, Flag))/binary, ": Invalid flag type `", (maps:get(flag, Flag))/binary, "`, expected `int_array`">>);
        false -> begin
            Val = 'Flag.get_value_or_default_value'(Flag),
            case length(Val) == 0 of
                true -> [];
                false -> begin
                    Values = [],
                    lists:foreach(fun(F) ->
                        Values bsl 'string.int'(F),
                        ok
                        ok
                    end, Val),
                    Values
                end
            end
        end
        end.

'Flag.get_int'(Flags, Name) ->
    Flag = 'Flag.get'(Flags, Name),
    'Flag.get_int'(Flag).

'Flag.get_ints'(Flags, Name) ->
    Flag = 'Flag.get'(Flags, Name),
    'Flag.get_ints'(Flag).

'Flag.get_float'(Flag) ->
    case maps:get(flag, Flag) /= float of
        true -> error(<<(maps:get(name, Flag))/binary, ": Invalid flag type `", (maps:get(flag, Flag))/binary, "`, expected `float`">>);
        false -> begin
            Val = 'Flag.get_value_or_default_value'(Flag),
            case length(Val) == 0 of
                true -> 0.0;
                false -> 'string.f64'(lists:nth(1, Val))
            end
        end
        end.

'Flag.get_floats'(Flag) ->
    case maps:get(flag, Flag) /= float_array of
        true -> error(<<(maps:get(name, Flag))/binary, ": Invalid flag type `", (maps:get(flag, Flag))/binary, "`, expected `float_array`">>);
        false -> begin
            Val = 'Flag.get_value_or_default_value'(Flag),
            case length(Val) == 0 of
                true -> [];
                false -> begin
                    Values = [],
                    lists:foreach(fun(F) ->
                        Values bsl 'string.f64'(F),
                        ok
                        ok
                    end, Val),
                    Values
                end
            end
        end
        end.

'Flag.get_float'(Flags, Name) ->
    Flag = 'Flag.get'(Flags, Name),
    'Flag.get_float'(Flag).

'Flag.get_floats'(Flags, Name) ->
    Flag = 'Flag.get'(Flags, Name),
    'Flag.get_floats'(Flag).

'Flag.get_string'(Flag) ->
    case maps:get(flag, Flag) /= string of
        true -> error(<<(maps:get(name, Flag))/binary, ": Invalid flag type `", (maps:get(flag, Flag))/binary, "`, expected `string`">>);
        false -> begin
            Val = 'Flag.get_value_or_default_value'(Flag),
            case length(Val) == 0 of
                true -> <<"">>;
                false -> lists:nth(1, Val)
            end
        end
        end.

'Flag.get_strings'(Flag) ->
    case maps:get(flag, Flag) /= string_array of
        true -> error(<<(maps:get(name, Flag))/binary, ": Invalid flag type `", (maps:get(flag, Flag))/binary, "`, expected `string_array`">>);
        false -> begin
            Val = 'Flag.get_value_or_default_value'(Flag),
            case length(Val) == 0 of
                true -> [];
                false -> Val
            end
        end
        end.

'Flag.get_string'(Flags, Name) ->
    Flag = 'Flag.get'(Flags, Name),
    'Flag.get_string'(Flag).

'Flag.get_strings'(Flags, Name) ->
    Flag = 'Flag.get'(Flags, Name),
    'Flag.get_strings'(Flag).

'Flag.parse'(Flag, Args, Posix_mode) ->
    if
        not 'Flag.matches'(Flag, lists:nth(1, Args), Posix_mode) -> Args;
        maps:get(flag, Flag) == bool -> 'Flag.parse_bool'(Flag, Args);
        length(maps:get(value, Flag)) > 0 andalso (not lists:member(maps:get(flag, Flag), [int_array, float_array, string_array])) -> error(<<"The argument `", (maps:get(name, Flag))/binary, "` accept only one value!">>);
        true -> 'Flag.parse_raw'(Flag, Args)
    end.

'Flag.matches'(Flag, Arg, Posix_mode) ->
    Prefix = case Posix_mode of
        true -> <<"--">>;
        false -> <<"-">>
    end,
    (maps:get(name, Flag) /= <<"">> andalso Arg == <<(Prefix)/binary, (maps:get(name, Flag))/binary>>) orelse (maps:get(name, Flag) /= <<"">> andalso 'string.starts_with'(Arg, <<(Prefix)/binary, (maps:get(name, Flag))/binary, "=">>)) orelse (maps:get(abbrev, Flag) /= <<"">> andalso Arg == <<"-", (maps:get(abbrev, Flag))/binary>>) orelse (maps:get(abbrev, Flag) /= <<"">> andalso 'string.starts_with'(Arg, <<"-", (maps:get(abbrev, Flag))/binary, "=">>)).

'Flag.parse_raw'(Flag, Args) ->
    case length(lists:nth(1, Args)) > length(maps:get(name, Flag)) andalso 'string.contains'(lists:nth(1, Args), <<"=">>) of
        true -> begin
            maps:get(value, Flag) bsl lists:nth(2, 'string.split'(lists:nth(1, Args), <<"=">>)),
            lists:nth(todo + 1, Args)
        end;
        false -> case length(Args) >= 2 of
            true -> begin
                maps:get(value, Flag) bsl lists:nth(2, Args),
                lists:nth(todo + 1, Args)
            end;
            false -> ok
        end
    end,
    error(<<"Missing argument for `", (maps:get(name, Flag))/binary, "`">>).

'Flag.parse_bool'(Flag, Args) ->
    case length(lists:nth(1, Args)) > length(maps:get(name, Flag)) andalso 'string.contains'(lists:nth(1, Args), <<"=">>) of
        true -> begin
            lists:nth(todo + 1, Args)
        end;
        false -> case length(Args) >= 2 of
            true -> case lists:member(lists:nth(2, Args), [<<"true">>, <<"false">>]) of
                true -> begin
                    lists:nth(todo + 1, Args)
                end;
                false -> ok
            end;
            false -> ok
        end
    end,
    lists:nth(todo + 1, Args).

'Flag.get'(Flags, Name) ->
    lists:foreach(fun(Flag) ->
        case maps:get(name, Flag) == Name of
            true -> Flag;
            false -> ok
        end,
        ok
    end, Flags),
    error(<<"Flag `", (Name)/binary, "` not found in ", (Flags)/binary>>).

'Flag.contains'(Flags, Name) ->
    lists:foreach(fun(Flag) ->
        case maps:get(name, Flag) == Name orelse maps:get(abbrev, Flag) == Name of
            true -> true;
            false -> ok
        end,
        ok
    end, Flags),
    false.

'Flag.get_value_or_default_value'(Flag) ->
    case length(maps:get(value, Flag)) == 0 andalso length(maps:get(default_value, Flag)) > 0 of
        true -> maps:get(default_value, Flag);
        false -> maps:get(value, Flag)
    end.

help_flag(With_abbrev) ->
    Sabbrev = case With_abbrev of
        true -> <<"h">>;
        false -> <<"">>
    end,
    #{flag => bool, name => <<"help">>, abbrev => Sabbrev, description => <<"Prints help information.">>, {vbeam, type} => 'Flag'}.

help_cmd() ->
    #{name => <<"help">>, usage => <<"<command>">>, description => <<"Prints help information.">>, execute => Cli.print_help_for_command, {vbeam, type} => 'Command'}.

print_help_for_command(Cmd) ->
    case length(maps:get(args, Cmd)) > 0 of
        true -> begin
            lists:foreach(fun(Sub_cmd) ->
                case maps:get(name, Sub_cmd) == lists:nth(1, maps:get(args, Cmd)) of
                    true -> begin
                        Cmd_ = todo,
                        print('Command.help_message'(Cmd_)),
                    end;
                    false -> ok
                end,
                ok
            end, maps:get(commands, Cmd)),
            print(<<"Invalid command: ", ('[]string.join'(maps:get(args, Cmd), <<" ">>))/binary>>)
        end;
        false -> case maps:get(parent, Cmd) /= todo of
            true -> print('Command.help_message'(maps:get(parent, Cmd)));
            false -> ok
        end
    end,
    ok.

'Command.help_message'(Cmd) ->
    Help = <<"">>,
    Help1 = <<"Usage: ", ('Command.full_name'(Cmd))/binary>>,
    case length(maps:get(flags, Cmd)) > 0 of
        true -> ok;
        false -> ok
    end,
    case length(maps:get(commands, Cmd)) > 0 of
        true -> ok;
        false -> ok
    end,
    case length(maps:get(usage, Cmd)) > 0 of
        true -> ok;
        false -> ok
    end,
    Help2 = <<"\\n">>,
    case maps:get(description, Cmd) /= <<"">> of
        true -> ok;
        false -> ok
    end,
    Abbrev_len = 0,
    Name_len = 20,
    case maps:get(posix_mode, Cmd) of
        true -> begin
            lists:foreach(fun(Flag) ->
                case maps:get(abbrev, Flag) /= <<"">> of
                    true -> ok;
                    false -> ok
                end,
                Name_len1 = max(Name_len, Abbrev_len + length(maps:get(name, Flag)) + 2 + 2),
                ok
            end, maps:get(flags, Cmd)),
            lists:foreach(fun(Command) ->
                Name_len2 = max(Name_len1, length(maps:get(name, Command)) + 2),
                ok
            end, maps:get(commands, Cmd)),
        end;
        false -> begin
            lists:foreach(fun(Flag) ->
                case maps:get(abbrev, Flag) /= <<"">> of
                    true -> ok;
                    false -> ok
                end,
                Name_len3 = max(Name_len2, Abbrev_len + length(maps:get(name, Flag)) + 2 + 1),
                ok
            end, maps:get(flags, Cmd)),
            lists:foreach(fun(Command) ->
                Name_len4 = max(Name_len3, length(maps:get(name, Command)) + 2),
                ok
            end, maps:get(commands, Cmd)),
        end
    end,
    case length(maps:get(flags, Cmd)) > 0 of
        true -> begin
            Help3 = <<"\\nFlags:\\n">>,
            lists:foreach(fun(Flag) ->
                Flag_name = <<"">>,
                Prefix = case maps:get(posix_mode, Cmd) of
                    true -> <<"--">>;
                    false -> <<"-">>
                end,
                case maps:get(abbrev, Flag) /= <<"">> of
                    true -> begin
                        Abbrev_indent = 'string.repeat'(<<" ">>, Abbrev_len - length(maps:get(abbrev, Flag)) - 1),
                        Flag_name1 = <<"-", (maps:get(abbrev, Flag))/binary, (Abbrev_indent)/binary, (Prefix)/binary, (maps:get(name, Flag))/binary>>,
                    end;
                    false -> begin
                        Abbrev_indent1 = 'string.repeat'(<<" ">>, Abbrev_len),
                        Flag_name2 = <<(Abbrev_indent1)/binary, (Prefix)/binary, (maps:get(name, Flag))/binary>>,
                    end
                end,
                Required = <<"">>,
                case maps:get(required, Flag) of
                    true -> ok;
                    false -> ok
                end,
                Base_indent = 'string.repeat'(<<" ">>, 2),
                Description_indent = 'string.repeat'(<<" ">>, Name_len4 - length(Flag_name2)),
                Help4 = <<(<<(<<(Base_indent)/binary, (Flag_name2)/binary, (Description_indent)/binary>>)/binary, (pretty_description(<<(maps:get(description, Flag))/binary, (Required)/binary>>, 2 + Name_len4))/binary>>)/binary, (<<"\\n">>)/binary>>,
                ok
            end, maps:get(flags, Cmd)),
        end;
        false -> ok
    end,
    case length(maps:get(commands, Cmd)) > 0 of
        true -> begin
            Help5 = <<"\\nCommands:\\n">>,
            lists:foreach(fun(Command) ->
                Base_indent1 = 'string.repeat'(<<" ">>, 2),
                Description_indent1 = 'string.repeat'(<<" ">>, Name_len4 - length(maps:get(name, Command))),
                Help6 = <<(<<(<<(Base_indent1)/binary, (maps:get(name, Command))/binary, (Description_indent1)/binary>>)/binary, (pretty_description(maps:get(description, Command), 2 + Name_len4))/binary>>)/binary, (<<"\\n">>)/binary>>,
                ok
            end, maps:get(commands, Cmd)),
        end;
        false -> ok
    end,
    Help6.

pretty_description(S, Indent_len) ->
    Width = element(1, get_terminal_size()),
    case Indent_len > Width of
        true -> S;
        false -> begin
            Indent = 'string.repeat'(<<" ">>, Indent_len),
            Chars_per_line = Width - Indent_len,
            Acc = new_builder(((length(S) div Chars_per_line) + 1) * (Width + 1)),
            lists:foreach(fun(Line) ->
                case K /= 0 of
                    true -> 'Builder.write_string'(Acc, <<"\\n", (Indent)/binary>>);
                    false -> ok
                end,
                I = Chars_per_line - 2,
                J = 0,
                % TODO: unhandled stmt type
                ok                case J /= 0 of
                    true -> 'Builder.write_string'(Acc, Indent);
                    false -> ok
                end,
                'Builder.write_string'(Acc, 'string.trim_space'(lists:nth(todo + 1, Line))),
                ok
            end, 'string.split'(S, <<"\\n">>)),
            'Builder.str'(Acc)
        end
        end.

max(A, B) ->
    Res = case A > B of
        true -> A;
        false -> B
    end,
    Res.

man_flag() ->
    #{flag => bool, name => <<"man">>, description => <<"Prints the auto-generated manpage.">>, {vbeam, type} => 'Flag'}.

man_cmd() ->
    #{name => <<"man">>, usage => <<"<subcommand>">>, description => <<"Prints the auto-generated manpage.">>, execute => Cli.print_manpage_for_command, {vbeam, type} => 'Command'}.

print_manpage_for_command(Cmd) ->
    case length(maps:get(args, Cmd)) > 0 of
        true -> begin
            lists:foreach(fun(Sub_cmd) ->
                case maps:get(name, Sub_cmd) == lists:nth(1, maps:get(args, Cmd)) of
                    true -> begin
                        Man_cmd = todo,
                        print('Command.manpage'(Man_cmd)),
                    end;
                    false -> ok
                end,
                ok
            end, maps:get(commands, Cmd)),
            print(<<"Invalid command: ", ('[]string.join'(maps:get(args, Cmd), <<" ">>))/binary>>)
        end;
        false -> case maps:get(parent, Cmd) /= todo of
            true -> print('Command.manpage'(maps:get(parent, Cmd)));
            false -> ok
        end
    end,
    ok.

'Command.manpage'(Cmd) ->
    Mdoc = <<".Dd ", ('Time.strftime'(now(), <<"%B %d, %Y">>))/binary, "\\n">>,
    Mdoc1 = <<".Dt ", ('string.to_upper'('string.replace'('Command.full_name'(Cmd), <<" ">>, <<"-">>)))/binary, " 1\\n">>,
    Mdoc2 = <<".Os\\n.Sh NAME\\n.Nm ", ('string.replace'('Command.full_name'(Cmd), <<" ">>, <<"-">>))/binary, "\\n.Nd ", (maps:get(description, Cmd))/binary, "\\n">>,
    Mdoc3 = <<".Sh SYNOPSIS\\n">>,
    Mdoc4 = <<".Nm ", (maps:get(name, 'Command.root'(Cmd)))/binary, "\\n">>,
    case todo of
        true -> begin
            Parents = [],
            case not 'Command.is_root'(maps:get(parent, Cmd)) of
                true -> begin
                    'Command.prepend'(Parents, maps:get(parent, Cmd)),
                    % TODO: unhandled stmt type
                    ok                    lists:foreach(fun(C) ->
                        Mdoc5 = <<".Ar ", (maps:get(name, C))/binary, "\\n">>,
                        ok
                    end, Parents),
                end;
                false -> ok
            end,
            Mdoc6 = <<".Ar ", (maps:get(name, Cmd))/binary, "\\n">>,
        end;
        false -> ok
    end,
    lists:foreach(fun(Flag) ->
        Mdoc7 = <<".Op">>,
        case maps:get(abbrev, Flag) /= <<"">> of
            true -> ok;
            false -> case maps:get(posix_mode, Cmd) of
                true -> ok;
                false -> ok
            end
        end,
        case maps:get(flag, Flag) of
            int; float; int_array; float_array -> ok;
            string; string_array -> ok;
            _ -> ok
        end,
        Mdoc8 = <<"\\n">>,
        ok
    end, maps:get(flags, Cmd)),
    Mdoc9 = lists:foldl(fun(I, MdocAcc) ->
        MdocOut = <<".Ar arg", (integer_to_binary(I))/binary, "\\n">>,
        MdocOut
    end, Mdoc8, lists:seq(0, maps:get(required_args, Cmd) - 1)),
    case length(maps:get(commands, Cmd)) > 0 of
        true -> begin
            Mdoc10 = <<".Nm ", (maps:get(name, 'Command.root'(Cmd)))/binary, "\\n">>,
            case todo of
                true -> begin
                    Parents1 = [],
                    case not 'Command.is_root'(maps:get(parent, Cmd)) of
                        true -> begin
                            'Command.prepend'(Parents1, maps:get(parent, Cmd)),
                            % TODO: unhandled stmt type
                            ok                            lists:foreach(fun(C) ->
                                Mdoc11 = <<".Ar ", (maps:get(name, C))/binary, "\\n">>,
                                ok
                            end, Parents1),
                        end;
                        false -> ok
                    end,
                    Mdoc12 = <<".Ar ", (maps:get(name, Cmd))/binary, "\\n">>,
                end;
                false -> ok
            end,
            Mdoc13 = <<".Ar subcommand\\n">>,
        end;
        false -> ok
    end,
    Mdoc14 = <<".Sh DESCRIPTION\\n">>,
    case maps:get(man_description, Cmd) /= <<"">> of
        true -> ok;
        false -> case maps:get(description, Cmd) /= <<"">> of
            true -> ok;
            false -> ok
        end
    end,
    case length(maps:get(flags, Cmd)) > 0 of
        true -> begin
            Mdoc15 = <<".Pp\\nThe options are as follows:\\n">>,
            Mdoc16 = <<".Bl -tag -width indent\\n">>,
            lists:foreach(fun(Flag) ->
                Mdoc17 = <<".It">>,
                case maps:get(abbrev, Flag) /= <<"">> of
                    true -> ok;
                    false -> ok
                end,
                case maps:get(posix_mode, Cmd) of
                    true -> ok;
                    false -> ok
                end,
                Mdoc18 = <<"\\n">>,
                case maps:get(description, Flag) /= <<"">> of
                    true -> ok;
                    false -> ok
                end,
                ok
            end, maps:get(flags, Cmd)),
            Mdoc19 = <<".El\\n">>,
        end;
        false -> ok
    end,
    case length(maps:get(commands, Cmd)) > 0 of
        true -> begin
            Mdoc20 = <<".Pp\\nThe subcommands are as follows:\\n">>,
            Mdoc21 = <<".Bl -tag -width indent\\n">>,
            lists:foreach(fun(C) ->
                Mdoc22 = <<".It Cm ", (maps:get(name, C))/binary, "\\n">>,
                case maps:get(description, C) /= <<"">> of
                    true -> ok;
                    false -> ok
                end,
                ok
            end, maps:get(commands, Cmd)),
            Mdoc23 = <<".El\\n">>,
        end;
        false -> ok
    end,
    case length(maps:get(commands, Cmd)) > 0 of
        true -> begin
            Mdoc24 = <<".Sh SEE ALSO\\n">>,
            Cmds = [],
            case todo of
                true -> Cmds bsl 'string.replace'('Command.full_name'(maps:get(parent, Cmd)), <<" ">>, <<"-">>);
                false -> ok
            end,
            lists:foreach(fun(C) ->
                Cmds bsl 'string.replace'('Command.full_name'(C), <<" ">>, <<"-">>),
                ok
            end, maps:get(commands, Cmd)),
            '[]string.sort'(Cmds),
            I = 1,
            lists:foreach(fun(C) ->
                Mdoc25 = <<".Xr ", (C)/binary, " 1">>,
                case I == length(Cmds) of
                    true -> ok;
                    false -> ok
                end,
                todo,
                ok
            end, Cmds),
        end;
        false -> ok
    end,
    Mdoc25.

version_flag(With_abbrev) ->
    Sabbrev = case With_abbrev of
        true -> <<"v">>;
        false -> <<"">>
    end,
    #{flag => bool, name => <<"version">>, abbrev => Sabbrev, description => <<"Prints version information.">>, {vbeam, type} => 'Flag'}.

version_cmd() ->
    #{name => <<"version">>, description => <<"Prints version information.">>, execute => Cli.print_version_for_command, {vbeam, type} => 'Command'}.

print_version_for_command(Cmd) ->
    case length(maps:get(args, Cmd)) > 0 of
        true -> begin
            lists:foreach(fun(Sub_cmd) ->
                case maps:get(name, Sub_cmd) == lists:nth(1, maps:get(args, Cmd)) of
                    true -> begin
                        Version_cmd = todo,
                        print('Command.version'(Version_cmd)),
                    end;
                    false -> ok
                end,
                ok
            end, maps:get(commands, Cmd)),
            vbeam_io:println(<<"Invalid command: ", ('[]string.join'(maps:get(args, Cmd), <<" ">>))/binary>>)
        end;
        false -> case maps:get(parent, Cmd) /= todo of
            true -> vbeam_io:println('Command.version'(maps:get(parent, Cmd)));
            false -> vbeam_io:println('Command.version'(Cmd))
        end
    end,
    ok.

'Command.version'(Cmd) ->
    <<(maps:get(name, Cmd))/binary, " version ", (maps:get(version, Cmd))/binary>>.

'FlagType__static__from'(Input) ->
    error(<<"invalid value">>).
