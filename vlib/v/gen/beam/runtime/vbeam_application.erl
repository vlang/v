%% vbeam_application - OTP Application wrapper for V language BEAM backend
%%
%% Provides the boilerplate for V programs to run as proper OTP applications.
%% A V program compiled to BEAM can be started as an OTP application, which
%% gives it:
%%   - Proper lifecycle (start/stop)
%%   - Dependency management (ensure other apps are started first)
%%   - A supervision tree root
%%   - Configuration via application environment
%%
%% V Concept -> Application Pattern:
%%   fn main()       -> application:start callback
%%   import module   -> ensure_started(dep)
%%   program config  -> application:get_env/set_env
%%
%% Usage from V codegen:
%%   vbeam_application:start()            %% Start the V application
%%   vbeam_application:ensure_started(crypto) %% Start a dependency
%%   vbeam_application:get_env(key, default)  %% Read config
%%   vbeam_application:set_env(key, value)    %% Write config

-module(vbeam_application).

-moduledoc """
Provides runtime helpers for OTP application lifecycle operations.
""".






-behaviour(application).

%% API
-export([
    start/0,
    stop/0,
    ensure_started/1,
    ensure_all_started/1,
    get_env/2,
    get_env/3,
    set_env/2,
    set_env/3
]).

%% application callbacks
-export([start/2, stop/1]).

%% ============================================================================
%% API Functions
%% ============================================================================

%% Start the vbeam application and its dependencies.
%% This is the entry point for V programs running as OTP apps.
%%
%% V: fn main() { ... }  (when compiled as OTP app)
-doc """
start/0 is a public runtime entrypoint in `vbeam_application`.
No parameters.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec start() -> ok | {error, term()}.

start() ->
    case application:ensure_all_started(vbeam) of
        {ok, _Started} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Stop the vbeam application.
-spec stop() -> ok | {error, term()}.

stop() ->
    application:stop(vbeam).

%% Ensure a single dependency application is started.
%% Starts it and all its dependencies if not already running.
%%
%% V: import crypto  (for BEAM backend, ensures OTP crypto app is up)
%% Erlang: vbeam_application:ensure_started(crypto)
-doc """
ensure_started/1 is a public runtime entrypoint in `vbeam_application`.
Parameters: `Application :: atom()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec ensure_started(Application :: atom()) -> ok | {error, term()}.

ensure_started(App) when is_atom(App), App =/= '' ->
    case application:ensure_all_started(App) of
        {ok, _Started} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Ensure a list of applications are started.
%% Useful when a V program has multiple BEAM dependencies.
%%
%% V: import [crypto, ssl, inets]
-spec ensure_all_started([atom()]) -> ok | {error, term()}.

ensure_all_started(Apps) when is_list(Apps) ->
    true = lists:all(fun is_atom/1, Apps),
    Results = lists:map(fun(App) -> ensure_started(App) end, Apps),
    case lists:filter(fun(R) -> R =/= ok end, Results) of
        [] -> ok;
        [FirstError | _] -> FirstError
    end.

%% Get an application environment variable with a default.
%% Uses the vbeam application namespace.
%%
%% V: config.get("port", 8080)
-doc """
get_env/2 is a public runtime entrypoint in `vbeam_application`.
Parameters: `Key :: atom()`, `Default :: term()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec get_env(Key :: atom(), Default :: term()) -> term().

get_env(Key, Default) when is_atom(Key), Key =/= '' ->
    get_env(vbeam, Key, Default).

%% Get an application environment variable from a specific application.
-spec get_env(App :: atom(), Key :: atom(), Default :: term()) -> term().

get_env(App, Key, Default) when is_atom(App), App =/= '', is_atom(Key), Key =/= '' ->
    case application:get_env(App, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

%% Set an application environment variable in the vbeam namespace.
%%
%% V: config.set("port", 8080)
-doc """
set_env/2 is a public runtime entrypoint in `vbeam_application`.
Parameters: `Key :: atom()`, `Value :: term()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec set_env(Key :: atom(), Value :: term()) -> ok.
set_env(Key, Value) when is_atom(Key), Key =/= '' ->
    set_env(vbeam, Key, Value).

%% Set an application environment variable in a specific application.
-spec set_env(App :: atom(), Key :: atom(), Value :: term()) -> ok.
set_env(App, Key, Value) when is_atom(App), App =/= '', is_atom(Key), Key =/= '' ->
    application:set_env(App, Key, Value).

%% ============================================================================
%% application Callbacks
%% ============================================================================

%% @private
%% Called by OTP when the application starts.
%% Creates a top-level supervisor for the V program.
-doc """
start/2 is a public runtime entrypoint in `vbeam_application`.
Parameters: `normal | {takeover, node()} | {failover, node()}`, `term()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec start(normal | {takeover, node()} | {failover, node()}, term()) ->
    {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Start a minimal top-level supervisor for the V application.
    %% V programs can add children to this supervisor at runtime.
    vbeam_supervisor:start_link(vbeam_app_sup, #{
        strategy => one_for_one,
        intensity => 5,
        period => 10,
        children => []
    }).

%% @private
%% Called by OTP when the application stops.
-spec stop(term()) -> ok.
stop(_State) ->
    ok.






