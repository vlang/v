%% vbeam_registry - Process registry for V language BEAM backend
%%
%% Provides named process registration for V programs. While Erlang has
%% built-in registration (register/2), this module adds:
%%   - Consistent V-friendly API
%%   - Automatic cleanup on process death
%%   - Name-to-pid lookup with proper error handling
%%   - Support for both atom and binary names
%%
%% V Concept -> Registry Pattern:
%%   Named channels      -> register(name, channel_pid)
%%   Named shared vars   -> register(name, genserver_pid)
%%   Service discovery    -> lookup(name) / whereis(name)
%%
%% This wraps Erlang's built-in registration for atom names and uses
%% an ETS table for binary/string names (V strings are binaries).
%%
%% Note: For large-scale applications, consider using Erlang's `pg` module
%% or the `global` module for distributed registration.

-module(vbeam_registry).

-export([
    start/0,
    register/2,
    unregister/1,
    lookup/1,
    whereis/1,
    registered/0
]).

%% ETS table name for binary-name registry
-define(REG_TABLE, vbeam_registry_table).

%% ============================================================================
%% API Functions
%% ============================================================================

%% Initialize the registry (creates the ETS table).
%% Should be called once at application startup.
%% Idempotent - safe to call multiple times.
-spec start() -> ok.
start() ->
    case ets:info(?REG_TABLE) of
        undefined ->
            ?REG_TABLE = ets:new(?REG_TABLE, [
                named_table,
                public,
                set,
                {keypos, 1},
                {read_concurrency, true}
            ]),
            ok;
        _Info ->
            %% Already exists
            ok
    end.

%% Register a process under a name.
%% Name can be an atom or a binary (V string).
%%
%% For atom names: uses Erlang's built-in registration (global visibility)
%% For binary names: uses ETS table with automatic monitor/cleanup
%%
%% V: registry.register("counter", pid)
%% Erlang: vbeam_registry:register(<<"counter">>, Pid)
%%
%% Returns ok on success, {error, already_registered} if name is taken.
-spec register(Name :: atom() | binary(), Pid :: pid()) ->
    ok | {error, already_registered | not_a_pid}.
register(_Name, Pid) when not is_pid(Pid) ->
    {error, not_a_pid};

register(Name, Pid) when is_atom(Name) ->
    try
        erlang:register(Name, Pid),
        ok
    catch
        error:badarg ->
            {error, already_registered}
    end;

register(Name, Pid) when is_binary(Name) ->
    ensure_table(),
    %% Monitor the process to auto-unregister on death
    case ets:lookup(?REG_TABLE, Name) of
        [{Name, _OldPid, OldMonRef}] ->
            %% Name already taken - check if old process is alive
            case is_process_alive(_OldPid) of
                true ->
                    {error, already_registered};
                false ->
                    %% Old process is dead, clean up and re-register
                    demonitor(OldMonRef, [flush]),
                    MonRef = monitor(process, Pid),
                    ets:insert(?REG_TABLE, {Name, Pid, MonRef}),
                    ok
            end;
        [] ->
            MonRef = monitor(process, Pid),
            ets:insert(?REG_TABLE, {Name, Pid, MonRef}),
            ok
    end.

%% Unregister a name.
%%
%% V: registry.unregister("counter")
%% Erlang: vbeam_registry:unregister(<<"counter">>)
-spec unregister(Name :: atom() | binary()) -> ok.
unregister(Name) when is_atom(Name) ->
    try
        erlang:unregister(Name),
        ok
    catch
        error:badarg -> ok  %% Not registered, that's fine
    end;

unregister(Name) when is_binary(Name) ->
    ensure_table(),
    case ets:lookup(?REG_TABLE, Name) of
        [{Name, _Pid, MonRef}] ->
            demonitor(MonRef, [flush]),
            ets:delete(?REG_TABLE, Name),
            ok;
        [] ->
            ok
    end.

%% Look up a process by name.
%% Returns {ok, Pid} if found, error if not.
%%
%% V: if pid := registry.lookup("counter") { ... }
%% Erlang:
%%   case vbeam_registry:lookup(<<"counter">>) of
%%       {ok, Pid} -> ...;
%%       error -> ...
%%   end
-spec lookup(Name :: atom() | binary()) -> {ok, pid()} | error.
lookup(Name) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined -> error;
        Pid -> {ok, Pid}
    end;

lookup(Name) when is_binary(Name) ->
    ensure_table(),
    case ets:lookup(?REG_TABLE, Name) of
        [{Name, Pid, _MonRef}] ->
            case is_process_alive(Pid) of
                true -> {ok, Pid};
                false ->
                    %% Stale entry, clean up
                    ets:delete(?REG_TABLE, Name),
                    error
            end;
        [] ->
            error
    end.

%% Look up a process by name.
%% Returns Pid if found, undefined if not.
%% Compatible with Erlang's built-in whereis/1 behavior.
%%
%% V: pid := registry.whereis("counter")
-spec whereis(Name :: atom() | binary()) -> pid() | undefined.
whereis(Name) ->
    case lookup(Name) of
        {ok, Pid} -> Pid;
        error -> undefined
    end.

%% List all registered names.
%% Returns atom names from built-in registry + binary names from ETS.
%%
%% V: names := registry.registered()
-spec registered() -> [atom() | binary()].
registered() ->
    AtomNames = erlang:registered(),
    BinaryNames = case ets:info(?REG_TABLE) of
        undefined -> [];
        _Info ->
            lists:map(fun({Name, _Pid, _MonRef}) -> Name end,
                      ets:tab2list(?REG_TABLE))
    end,
    AtomNames ++ BinaryNames.

%% ============================================================================
%% Internal Functions
%% ============================================================================

%% Ensure the ETS table exists (lazy initialization).
-spec ensure_table() -> ok.
ensure_table() ->
    case ets:info(?REG_TABLE) of
        undefined -> start();
        _Info -> ok
    end.
