%% vbeam_compiler - In-memory Core Erlang compilation service (Tier 3)
%%
%% Compiles Core Erlang text directly to .beam using compile:forms/2,
%% bypassing file I/O for intermediate .core files when used in pipe mode.
%%
%% Usage:
%%   escript vbeam_compiler.erl --dir <directory>     Compile all .core/.erl in dir
%%   escript vbeam_compiler.erl <file.core> [outdir]  Compile single .core file
%%   escript vbeam_compiler.erl --pipe <outdir>        Read Core Erlang from stdin
-module(vbeam_compiler).
-export([main/1, compile_core_file/2, compile_core_text/2, compile_dir/1]).

%% escript entry point
main(["--dir", Dir]) ->
    {OkCount, ErrCount, Errors} = compile_dir(Dir),
    case Errors of
        [] -> ok;
        _ ->
            lists:foreach(fun({F, E}) ->
                io:format(standard_error, "WARN ~s: ~p~n", [F, E])
            end, Errors)
    end,
    io:format("Compiled ~B module(s)", [OkCount]),
    case ErrCount of
        0 -> io:format("~n");
        _ -> io:format(", ~B skipped~n", [ErrCount])
    end;
main(["--pipe", OutDir]) ->
    Text = read_all_stdin(),
    case compile_core_text(Text, OutDir) of
        {ok, Mod, OutFile} ->
            io:format("ok:~s:~s~n", [Mod, OutFile]);
        {error, Reason} ->
            io:format(standard_error, "error:~p~n", [Reason]),
            halt(1)
    end;
main(["--batch", BatchFile, OutDir]) ->
    %% Batch mode: read a single file with multiple Core Erlang modules
    %% separated by null bytes (\0\0\0). Compiles all in one VM invocation.
    case file:read_file(BatchFile) of
        {ok, Bin} ->
            Modules = split_batch(binary_to_list(Bin)),
            {OkCount, ErrCount, Errors} = compile_batch(Modules, OutDir),
            case Errors of
                [] -> ok;
                _ ->
                    lists:foreach(fun({N, E}) ->
                        io:format(standard_error, "WARN module ~B: ~p~n", [N, E])
                    end, Errors)
            end,
            io:format("Compiled ~B module(s)", [OkCount]),
            case ErrCount of
                0 -> io:format("~n");
                _ -> io:format(", ~B skipped~n", [ErrCount])
            end;
        {error, Reason} ->
            io:format(standard_error, "error reading ~s: ~p~n", [BatchFile, Reason]),
            halt(1)
    end;
main([CoreFile]) ->
    OutDir = filename:dirname(CoreFile),
    compile_one(CoreFile, OutDir);
main([CoreFile, OutDir]) ->
    compile_one(CoreFile, OutDir);
main(_) ->
    io:format(standard_error,
        "vbeam_compiler - Core Erlang in-memory compiler (Tier 3)~n"
        "Usage:~n"
        "  escript vbeam_compiler.erl --dir <directory>~n"
        "  escript vbeam_compiler.erl <file.core> [outdir]~n"
        "  escript vbeam_compiler.erl --pipe <outdir>~n", []),
    halt(1).

compile_one(CoreFile, OutDir) ->
    case compile_core_file(CoreFile, OutDir) of
        {ok, Mod, OutFile} ->
            io:format("ok:~s:~s~n", [Mod, OutFile]);
        {error, Reason} ->
            io:format(standard_error, "error:~s: ~p~n", [CoreFile, Reason]),
            halt(1)
    end.

%% Compile a .core file to .beam using in-memory compilation
compile_core_file(CoreFile, OutDir) ->
    case file:read_file(CoreFile) of
        {ok, Bin} ->
            compile_core_text(binary_to_list(Bin), OutDir);
        {error, Reason} ->
            {error, {read_error, CoreFile, Reason}}
    end.

%% Compile Core Erlang text directly to .beam (the Tier 3 core)
%% Flow: text -> core_scan -> core_parse -> compile:forms -> .beam binary
compile_core_text(Text, OutDir) ->
    case core_scan:string(Text) of
        {ok, Tokens, _} ->
            case core_parse:parse(Tokens) of
                {ok, Forms} ->
                    case compile:forms(Forms, [from_core, binary, debug_info, return_errors]) of
                        {ok, ModName, Binary} ->
                            write_beam(ModName, Binary, OutDir);
                        {ok, ModName, Binary, _Warnings} ->
                            write_beam(ModName, Binary, OutDir);
                        error ->
                            {error, {compile_error, internal}};
                        {error, Errors, _Warnings} ->
                            {error, {compile_error, Errors}}
                    end;
                {error, ParseError} ->
                    {error, {parse_error, ParseError}}
            end;
        {error, ScanError, _} ->
            {error, {scan_error, ScanError}}
    end.

write_beam(ModName, Binary, OutDir) ->
    OutFile = filename:join(OutDir, atom_to_list(ModName) ++ ".beam"),
    case file:write_file(OutFile, Binary) of
        ok -> {ok, ModName, OutFile};
        {error, Reason} -> {error, {write_error, OutFile, Reason}}
    end.

%% Compile all .core and .erl files in a directory
%% Returns {OkCount, ErrCount, Errors} — continues on failure
compile_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            CoreFiles = [F || F <- lists:sort(Files),
                         filename:extension(F) == ".core"],
            %% Compile .erl files that don't have corresponding .core
            CoreRoots = [filename:rootname(F) || F <- CoreFiles],
            ErlFiles = [F || F <- lists:sort(Files),
                        filename:extension(F) == ".erl",
                        not lists:member(filename:rootname(F), CoreRoots)],
            %% Compile .core files using in-memory compilation
            CoreResults = lists:map(fun(F) ->
                Path = filename:join(Dir, F),
                case compile_core_file(Path, Dir) of
                    {ok, _Mod, _Out} -> {ok, F};
                    {error, Reason} -> {error, F, Reason}
                end
            end, CoreFiles),
            %% Compile .erl files using standard compiler
            ErlResults = lists:map(fun(F) ->
                Path = filename:join(Dir, F),
                case compile:file(Path, [{outdir, Dir}, debug_info, return_errors]) of
                    {ok, _Mod} -> {ok, F};
                    {ok, _Mod, _Warns} -> {ok, F};
                    error -> {error, F, internal};
                    {error, Errs, _Warns} -> {error, F, Errs}
                end
            end, ErlFiles),
            AllResults = CoreResults ++ ErlResults,
            Errors = [{F, E} || {error, F, E} <- AllResults],
            OkCount = length([ok || {ok, _} <- AllResults]),
            ErrCount = length(Errors),
            {OkCount, ErrCount, Errors};
        {error, Reason} ->
            {0, 1, [{Dir, Reason}]}
    end.

%% Split batch file by null-byte delimiter (\0\0\0)
split_batch(Text) ->
    split_batch(Text, [], []).

split_batch([], Current, Acc) ->
    case lists:flatten(lists:reverse(Current)) of
        [] -> lists:reverse(Acc);
        Mod -> lists:reverse([Mod | Acc])
    end;
split_batch([0, 0, 0 | Rest], Current, Acc) ->
    Mod = lists:flatten(lists:reverse(Current)),
    case Mod of
        [] -> split_batch(Rest, [], Acc);
        _ -> split_batch(Rest, [], [Mod | Acc])
    end;
split_batch([C | Rest], Current, Acc) ->
    split_batch(Rest, [[C] | Current], Acc).

%% Compile a list of Core Erlang text modules
compile_batch(Modules, OutDir) ->
    Results = lists:map(fun({N, Text}) ->
        case compile_core_text(Text, OutDir) of
            {ok, _Mod, _Out} -> {ok, N};
            {error, Reason} -> {error, N, Reason}
        end
    end, lists:zip(lists:seq(1, length(Modules)), Modules)),
    Errors = [{N, E} || {error, N, E} <- Results],
    OkCount = length([ok || {ok, _} <- Results]),
    ErrCount = length(Errors),
    {OkCount, ErrCount, Errors}.

%% Read all input from stdin
read_all_stdin() ->
    read_all_stdin([]).

read_all_stdin(Acc) ->
    case io:get_line("") of
        eof -> lists:flatten(lists:reverse(Acc));
        {error, _} -> lists:flatten(lists:reverse(Acc));
        Line -> read_all_stdin([Line | Acc])
    end.
