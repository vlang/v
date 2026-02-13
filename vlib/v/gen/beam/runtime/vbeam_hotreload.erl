%% vbeam_hotreload - Hot code reloading support for V-on-BEAM
%%
%% Provides ergonomic wrappers around BEAM's code server for
%% live code reloading. BEAM natively supports keeping two versions
%% of a module (old + current). This module handles the purge/load
%% cycle and provides V-friendly APIs.
%%
%% Usage from V:
%%   vbeam_hotreload.reload('my_module')     — reload a single module
%%   vbeam_hotreload.reload_all('/path/dir')  — reload all .beam in dir
%%   vbeam_hotreload.watch('/path/dir', 1000) — poll directory for changes
-module(vbeam_hotreload).

-moduledoc """
Supports hot reload workflows for running BEAM modules.
""".






-export([reload/1, reload_from/2, reload_all/1, watch/2, watch/3]).

%% Export protocol types for cross-module contracts
-export_type([reload_msg/0, reload_result/0, reload_error_entry/0]).

%% Hot-reload protocol types
-type reload_msg() ::
    {reload, atom()}
    | {reload_from, atom(), string()}
    | {reload_all, string()}.
-type reload_result() :: {ok, atom()} | {error, term()}.
-type reload_error_entry() :: {atom() | string(), term()}.

%% Reload a module that's already on the code path.
%% Purges old version, loads new version.
%% Returns {ok, Module} | {error, Reason}.
-doc """
reload/1 is a public runtime entrypoint in `vbeam_hotreload`.
Parameters: `atom()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec reload(atom()) -> reload_result().

reload(Module) when is_atom(Module) ->
    true = atom_to_list(Module) =/= [],
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} -> {ok, Module};
        {error, Reason} -> {error, Reason}
    end.

%% Reload a module from a specific .beam file path.
-spec reload_from(atom(), string()) -> reload_result().
reload_from(Module, BeamPath) when is_atom(Module), is_list(BeamPath) ->
    true = filename:extension(BeamPath) =:= ".beam",
    case file:read_file(BeamPath) of
        {ok, Binary} ->
            code:purge(Module),
            case code:load_binary(Module, BeamPath, Binary) of
                {module, Module} -> {ok, Module};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, {read_error, Reason}}
    end.

%% Reload all .beam files in a directory.
%% Returns {ok, Loaded, Errors} with counts.
-doc """
reload_all/1 is a public runtime entrypoint in `vbeam_hotreload`.
Parameters: `string()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec reload_all(string()) -> {ok, non_neg_integer(), [reload_error_entry()]}.

reload_all(Dir) when is_list(Dir), Dir =/= [] ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            BeamFiles = [F || F <- Files, filename:extension(F) == ".beam"],
            %% Add dir to code path if not already there
            AbsDir = filename:absname(Dir),
            case code:add_pathz(AbsDir) of
                true -> ok;
                {error, _} -> ok
            end,
            Results = lists:map(fun(F) ->
                Mod = list_to_atom(filename:rootname(F)),
                case reload(Mod) of
                    {ok, _} -> {ok, Mod};
                    {error, R} -> {error, Mod, R}
                end
            end, BeamFiles),
            Errors = [{M, R} || {error, M, R} <- Results],
            OkCount = length([ok || {ok, _} <- Results]),
            {ok, OkCount, Errors};
        {error, Reason} ->
            {ok, 0, [{Dir, Reason}]}
    end.

%% Watch a directory and reload changed .beam files.
%% Polls every IntervalMs milliseconds. Runs in calling process.
-doc """
watch/2 is a public runtime entrypoint in `vbeam_hotreload`.
Parameters: `string()`, `pos_integer()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec watch(string(), pos_integer()) -> no_return().

watch(Dir, IntervalMs) when is_list(Dir), is_integer(IntervalMs), IntervalMs > 0 ->

    watch(Dir, IntervalMs, fun(Mod, Status) ->
        io:format("~s: ~p~n", [Mod, Status])
    end).

%% Watch with a callback function for each reload.
-spec watch(string(), pos_integer(), fun((atom(), reload_result()) -> any())) -> no_return().
watch(Dir, IntervalMs, Callback)
  when is_list(Dir), is_integer(IntervalMs), IntervalMs > 0, is_function(Callback, 2) ->
    AbsDir = filename:absname(Dir),
    code:add_pathz(AbsDir),
    Mtimes = scan_mtimes(AbsDir),
    watch_loop(AbsDir, IntervalMs, Callback, Mtimes).

%% Internal: scan directory for .beam file modification times
scan_mtimes(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            BeamFiles = [F || F <- Files, filename:extension(F) == ".beam"],
            maps:from_list(lists:filtermap(fun(F) ->
                Path = filename:join(Dir, F),
                case filelib:last_modified(Path) of
                    0 -> false;
                    Mtime -> {true, {F, Mtime}}
                end
            end, BeamFiles));
        {error, _} ->
            #{}
    end.

%% Internal: poll loop
watch_loop(Dir, IntervalMs, Callback, OldMtimes) ->
    timer:sleep(IntervalMs),
    NewMtimes = scan_mtimes(Dir),
    %% Find changed files
    maps:foreach(fun(File, NewTime) ->
        case maps:find(File, OldMtimes) of
            {ok, OldTime} when OldTime =/= NewTime ->
                %% File changed — reload
                Mod = list_to_atom(filename:rootname(File)),
                Result = reload(Mod),
                Callback(Mod, Result);
            error ->
                %% New file — load
                Mod = list_to_atom(filename:rootname(File)),
                Result = reload(Mod),
                Callback(Mod, Result);
            _ ->
                ok
        end
    end, NewMtimes),
    watch_loop(Dir, IntervalMs, Callback, NewMtimes).





