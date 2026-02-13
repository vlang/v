%% vbeam_supervisor - Supervisor wrapper for V language BEAM backend
%%
%% Provides OTP supervision trees for V programs. V's spawn model is
%% fire-and-forget by default, but with supervisors, spawned processes
%% can be automatically restarted on failure.
%%
%% V Concept -> Supervisor Pattern:
%%   spawn_supervised fn()  -> supervisor:start_child(Sup, ChildSpec)
%%   process group          -> supervisor with strategy
%%   fault tolerance        -> automatic restart based on strategy
%%
%% Supervision strategies:
%%   one_for_one  - Only the failed child restarts (default)
%%   one_for_all  - All children restart if one fails
%%   rest_for_one - Failed child and all children started after it restart
%%
%% Child spec as map:
%%   #{
%%       id => my_worker,
%%       start => {Module, Function, Args},  %% or fun() -> {ok, Pid}
%%       restart => permanent | transient | temporary,
%%       shutdown => 5000 | brutal_kill | infinity
%%   }

-module(vbeam_supervisor).

-moduledoc """
Provides supervisor-compatible utilities for V runtime services.
""".






-behaviour(supervisor).

%% API
-export([
    start_link/1,
    start_link/2,
    start_child/2,
    stop_child/2,
    restart_child/2,
    which_children/1,
    count_children/1
]).

%% Internal (called by supervisor for fun-based child specs)
-export([start_fun_child/1]).

%% supervisor callback
-export([init/1]).

%% Export protocol types for cross-module contracts
-export_type([child_spec/0, sup_msg/0]).

%% Supervisor message protocol types
-type child_start() :: {module(), atom(), [term()]} | fun(() -> term()).
-type child_spec() :: #{
    id := term(),
    start := child_start(),
    restart => permanent | transient | temporary,
    shutdown => brutal_kill | infinity | timeout(),
    type => worker | supervisor
}.
-type sup_msg() ::
    {start_child, child_spec()}
    | {stop_child, atom()}
    | {restart_child, atom()}.

%% ============================================================================
%% API Functions
%% ============================================================================

%% Start a supervisor with a spec map.
%%
%% SupSpec is a map:
%%   #{
%%       strategy => one_for_one | one_for_all | rest_for_one,
%%       intensity => non_neg_integer(),  %% max restarts (default 3)
%%       period => pos_integer(),         %% in seconds (default 5)
%%       children => [ChildSpec]          %% list of child specs
%%   }
%%
%% V: sup := supervisor.new(#{strategy: .one_for_one, children: [...]})
-doc """
start_link/1 is a public runtime entrypoint in `vbeam_supervisor`.
Parameters: `SupSpec :: map()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec start_link(SupSpec :: map()) -> {ok, pid()} | {error, term()}.

start_link(SupSpec) when is_map(SupSpec) ->
    Strategy = maps:get(strategy, SupSpec, one_for_one),
    true = lists:member(Strategy, [one_for_one, one_for_all, rest_for_one]),
    supervisor:start_link(?MODULE, SupSpec).

%% Start a named supervisor.
%% V: sup := supervisor.new(#{name: :my_sup, ...})
-spec start_link(Name :: atom(), SupSpec :: map()) -> {ok, pid()} | {error, term()}.
start_link(Name, SupSpec) when is_atom(Name), is_map(SupSpec) ->
    supervisor:start_link({local, Name}, ?MODULE, SupSpec).

%% Dynamically add a child to a running supervisor.
%%
%% ChildSpec is a map:
%%   #{
%%       id => atom(),                        %% required: child identifier
%%       start => {Mod, Fun, Args} | Fun,     %% required: how to start
%%       restart => permanent | transient | temporary,  %% default: permanent
%%       shutdown => integer() | brutal_kill | infinity, %% default: 5000
%%       type => worker | supervisor                     %% default: worker
%%   }
%%
%% V: sup.start_child(#{id: :worker1, start: fn() { ... }})
-doc """
start_child/2 is a public runtime entrypoint in `vbeam_supervisor`.
Parameters: `Sup :: pid() | atom()`, `ChildSpec :: map()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec start_child(Sup :: pid() | atom(), ChildSpec :: child_spec()) ->
    {ok, pid()} | {error, term()}.
start_child(Sup, ChildSpec)
  when (is_pid(Sup) orelse is_atom(Sup)), is_map(ChildSpec) ->
    true = maps:is_key(id, ChildSpec) andalso maps:is_key(start, ChildSpec),
    OtpSpec = to_otp_child_spec(ChildSpec),
    supervisor:start_child(Sup, OtpSpec).

%% Stop (terminate and delete) a child by its id.
%% V: sup.stop_child(:worker1)
-spec stop_child(Sup :: pid() | atom(), ChildId :: atom()) -> ok | {error, term()}.
stop_child(Sup, ChildId) when (is_pid(Sup) orelse is_atom(Sup)), is_atom(ChildId) ->
    case supervisor:terminate_child(Sup, ChildId) of
        ok ->
            supervisor:delete_child(Sup, ChildId);
        Error ->
            Error
    end.

%% Restart a child by its id.
%% V: sup.restart_child(:worker1)
-doc """
restart_child/2 is a public runtime entrypoint in `vbeam_supervisor`.
Parameters: `Sup :: pid() | atom()`, `ChildId :: atom()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec restart_child(Sup :: pid() | atom(), ChildId :: atom()) ->
    {ok, pid()} | {error, term()}.
restart_child(Sup, ChildId) when (is_pid(Sup) orelse is_atom(Sup)), is_atom(ChildId) ->
    supervisor:restart_child(Sup, ChildId).

%% List all children of a supervisor.
%% Returns list of #{id => Id, pid => Pid, type => Type}
%%
%% V: children := sup.which_children()
-spec which_children(Sup :: pid() | atom()) -> [map()].

which_children(Sup) when is_pid(Sup) orelse is_atom(Sup) ->
    Children = supervisor:which_children(Sup),
    lists:map(fun({Id, Pid, Type, _Modules}) ->
        #{id => Id, pid => Pid, type => Type}
    end, Children).

%% Count children by category.
%% Returns #{specs => N, active => N, supervisors => N, workers => N}
%%
%% V: counts := sup.count_children()
-doc """
count_children/1 is a public runtime entrypoint in `vbeam_supervisor`.
Parameters: `Sup :: pid() | atom()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec count_children(Sup :: pid() | atom()) -> map().

count_children(Sup) when is_pid(Sup) orelse is_atom(Sup) ->
    Counts = supervisor:count_children(Sup),
    maps:from_list(Counts).

%% ============================================================================
%% supervisor Callback
%% ============================================================================

%% @private
-spec init(map()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init(SupSpec) when is_map(SupSpec) ->
    Strategy = maps:get(strategy, SupSpec, one_for_one),
    Intensity = maps:get(intensity, SupSpec, 3),
    Period = maps:get(period, SupSpec, 5),
    true = lists:member(Strategy, [one_for_one, one_for_all, rest_for_one]),
    true = is_integer(Intensity) andalso Intensity >= 0,
    true = is_integer(Period) andalso Period > 0,

    SupFlags = #{
        strategy => Strategy,
        intensity => Intensity,
        period => Period
    },

    Children = maps:get(children, SupSpec, []),
    ChildSpecs = lists:map(fun to_otp_child_spec/1, Children),

    {ok, {SupFlags, ChildSpecs}}.

%% ============================================================================
%% Internal Functions
%% ============================================================================

%% Convert a V-friendly child spec map to OTP child_spec format.
%%
%% Input (V-style):
%%   #{id => worker1, start => {Mod, Fun, Args}, restart => permanent}
%%   #{id => worker1, start => fun() -> {ok, spawn_link(Fun)} end}
%%
%% Output (OTP):
%%   #{id => worker1, start => {Mod, Fun, Args}, ...}
-spec to_otp_child_spec(child_spec()) -> supervisor:child_spec().
to_otp_child_spec(Spec) when is_map(Spec) ->
    Id = maps:get(id, Spec),
    Start = normalize_start(maps:get(start, Spec)),
    Restart = maps:get(restart, Spec, permanent),
    Shutdown = maps:get(shutdown, Spec, 5000),
    Type = maps:get(type, Spec, worker),

    #{
        id => Id,
        start => Start,
        restart => Restart,
        shutdown => Shutdown,
        type => Type
    }.

%% Normalize start specification.
%% Accepts:
%%   {Module, Function, Args}  -> pass through
%%   fun() -> {ok, Pid}        -> wrap in vbeam_supervisor_helper MFA
-spec normalize_start(term()) -> {module(), atom(), [term()]}.
normalize_start({Mod, Fun, Args}) when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    {Mod, Fun, Args};
normalize_start(Fun) when is_function(Fun, 0) ->
    %% Wrap the function in a starter that OTP supervisors expect.
    %% The supervisor needs {Module, Function, Args} format,
    %% so we use proc_lib:start_link to start a fun as a proper OTP process.
    {vbeam_supervisor, start_fun_child, [Fun]}.

%% Start a child from a fun.
%% Used internally when child spec has a fun() instead of MFA tuple.
%% Exported so supervisors can call it, but not part of the public API.
-doc """
start_fun_child/1 is a public runtime entrypoint in `vbeam_supervisor`.
Parameters: `fun((`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec start_fun_child(fun(() -> term())) -> {ok, pid()}.
start_fun_child(Fun) when is_function(Fun, 0) ->
    Pid = proc_lib:spawn_link(fun() ->
        Fun()
    end),
    true = is_pid(Pid),
    {ok, Pid}.





