%% vbeam_compiler - In-memory Core Erlang compilation service (Tier 3)
%%
%% Compiles Core Erlang text directly to .beam using compile:noenv_forms/2,
%% bypassing file I/O for intermediate .core files when used in pipe mode.
%% Uses noenv_forms to prevent ERL_COMPILER_OPTIONS pollution.
%%
%% Supports two input formats:
%%   --batch: Core Erlang text (parsed via core_scan/core_parse)
%%   --batch-etf: Erlang term text representing Core Erlang AST directly
%%
%% Usage:
%%   escript vbeam_compiler.erl --dir <directory>     Compile all .core/.erl in dir
%%   escript vbeam_compiler.erl <file.core> [outdir]  Compile single .core file
%%   escript vbeam_compiler.erl --pipe <outdir>        Read Core Erlang from stdin
%%   escript vbeam_compiler.erl --batch-etf <file> <outdir>  Compile ETF terms
-module(vbeam_compiler).
-export([main/1, compile_core_file/2, compile_core_text/2, compile_core_forms/2,
         compile_dir/1]).

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
main(["--batch-etf", BatchFile, OutDir]) ->
    %% ETF batch mode: read Erlang term text (Core Erlang AST terms directly).
    %% Each module is an Erlang term ({c_module,...}) separated by \0\0\0.
    %% Bypasses core_scan/core_parse entirely.
    case file:read_file(BatchFile) of
        {ok, Bin} ->
            Modules = split_batch(binary_to_list(Bin)),
            {OkCount, ErrCount, Errors} = compile_etf_batch(Modules, OutDir),
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
main(["--core-to-erl", CoreFile, OutDir]) ->
    %% Transpile .core -> .erl -> compile -> .beam
    %% Reads Core Erlang source, transpiles to Erlang source via
    %% vbeam_core_to_erl, writes .erl, then compiles with erlc.
    case file:read_file(CoreFile) of
        {ok, Bin} ->
            ErlText = vbeam_core_to_erl:transpile(binary_to_list(Bin)),
            %% Determine module name from the Core Erlang to get correct filename
            {ok, Tokens, _} = core_scan:string(binary_to_list(Bin)),
            {ok, CoreMod} = core_parse:parse(Tokens),
            ModAtom = cerl:concrete(cerl:module_name(CoreMod)),
            ErlFile = filename:join(OutDir, atom_to_list(ModAtom) ++ ".erl"),
            ok = file:write_file(ErlFile, ErlText),
            case compile:noenv_file(ErlFile, [{outdir, OutDir}, debug_info, return_errors]) of
                {ok, _Mod} ->
                    io:format("ok:~s:~s~n", [ModAtom, ErlFile]);
                {ok, _Mod, _Warns} ->
                    io:format("ok:~s:~s~n", [ModAtom, ErlFile]);
                error ->
                    io:format(standard_error, "error compiling ~s~n", [ErlFile]),
                    halt(1);
                {error, Errs, _Warns} ->
                    io:format(standard_error, "error compiling ~s: ~p~n", [ErlFile, Errs]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format(standard_error, "error reading ~s: ~p~n", [CoreFile, Reason]),
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
        "  escript vbeam_compiler.erl --pipe <outdir>~n"
        "  escript vbeam_compiler.erl --core-to-erl <file.core> <outdir>~n", []),
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
%% Flow: text -> core_scan -> core_parse -> compile:noenv_forms -> .beam binary
compile_core_text(Text, OutDir) ->
    case core_scan:string(Text) of
        {ok, Tokens, _} ->
            case core_parse:parse(Tokens) of
                {ok, Forms} ->
                    compile_core_forms(Forms, OutDir);
                {error, ParseError} ->
                    {error, {parse_error, ParseError}}
            end;
        {error, ScanError, _} ->
            {error, {scan_error, ScanError}}
    end.

%% Compile Core Erlang AST forms directly to .beam
%% Used by both text mode (after core_parse) and ETF mode (after erl_parse).
%% Uses noenv_forms to prevent ERL_COMPILER_OPTIONS pollution.
compile_core_forms(Forms, OutDir) ->
    CompileOpts = [from_core, binary, debug_info, return_errors,
                   {compile_info, [{source, <<"vbeam">>},
                                   {version, <<"0.1.0">>},
                                   {compiler, <<"vbeam_compiler">>}]}],
    case compile:noenv_forms(Forms, CompileOpts) of
        {ok, ModName, Binary} ->
            write_beam(ModName, Binary, OutDir);
        {ok, ModName, Binary, _Warnings} ->
            write_beam(ModName, Binary, OutDir);
        error ->
            {error, {compile_error, internal}};
        {error, Errors, _Warnings} ->
            {error, {compile_error, Errors}}
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
                case compile:noenv_file(Path, [{outdir, Dir}, debug_info, return_errors]) of
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

%% Parse Erlang term text into a Core Erlang AST term.
%% The text should be a single Erlang term like {c_module,[],{c_literal,...},...}
%% terminated with a period.
parse_etf_text(Text) ->
    %% Ensure the text ends with a period for erl_scan
    Text2 = case lists:last(Text) of
        $. -> Text;
        _ -> Text ++ "."
    end,
    case erl_scan:string(Text2) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> {ok, Term};
                {error, Reason} -> {error, {etf_parse_error, Reason}}
            end;
        {error, Reason, _} ->
            {error, {etf_scan_error, Reason}}
    end.

%% Compile a list of Erlang term text modules (ETF batch mode)
compile_etf_batch(Modules, OutDir) ->
    Results = lists:map(fun({N, Text}) ->
        case parse_etf_text(Text) of
            {ok, Forms} ->
                case compile_core_forms(Forms, OutDir) of
                    {ok, _Mod, _Out} -> {ok, N};
                    {error, Reason} -> {error, N, Reason}
                end;
            {error, Reason} ->
                {error, N, Reason}
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
