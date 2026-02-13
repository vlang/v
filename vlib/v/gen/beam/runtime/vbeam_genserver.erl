%% vbeam_genserver - GenServer wrapper for V language BEAM backend
%%
%% Maps V's stateful concurrent patterns to OTP gen_server behavior.
%% V programs use this module for managed state with synchronous/async access.
%%
%% V Concept -> GenServer Pattern:
%%   shared T + lock     -> gen_server call with state mutation
%%   shared T + rlock    -> gen_server call with state read
%%   struct methods      -> handler module callbacks
%%   inline closures     -> functional call/cast style
%%
%% Two usage styles:
%%   1. Handler module:  start(State, #{handler => Mod}) where Mod exports
%%                       handle_call/2, handle_cast/2
%%   2. Functional:      call(Pid, fun(State) -> {Reply, NewState} end)
%%                       for inline state transformations
%%
%% Example (V perspective):
%%   // V code
%%   shared counter := 0
%%   lock counter { counter++ }
%%   val := rlock counter { counter }
%%
%% Generated Erlang:
%%   {ok, Pid} = vbeam_genserver:start(0, #{}),
%%   vbeam_genserver:call(Pid, fun(S) -> {ok, S + 1} end),
%%   Val = vbeam_genserver:get_state(Pid).

-module(vbeam_genserver).

-moduledoc """
Provides GenServer compatibility for V runtime processes.
""".






-behaviour(gen_server).

%% API - what V codegen calls
-export([
    start/2,
    start_link/2,
    call/2, call/3,
    cast/2,
    stop/1,
    get_state/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Export protocol types for cross-module contracts
-export_type([
    call_request/0,
    call_reply/0,
    cast_request/0,
    info_message/0
]).

%% Internal state record
-record(state, {
    value :: term(),          %% The managed V value
    handler :: module() | undefined,  %% Optional callback module
    max_mailbox_len = 10000 :: integer()  %% Mailbox overflow threshold
}).

%% Inter-process message protocol types
-type call_payload() :: fun((term()) -> {term(), term()}) | term().
-type call_request() :: vbeam_get_state | {vbeam_call, call_payload()}.
-type call_reply() :: {reply, term(), #state{}}.
-type cast_payload() :: fun((term()) -> term()) | term().
-type cast_request() :: {vbeam_cast, cast_payload()}.
-type info_message() :: check_mailbox | term().

%% ============================================================================
%% API Functions
%% ============================================================================

%% Start a GenServer with initial state and options.
%% Options map:
%%   handler => Module  - Module with handle_call/2, handle_cast/2 callbacks
%%   name => atom()     - Optional registered name
%%
%% V: shared counter := 0
%% Erlang: {ok, Pid} = vbeam_genserver:start(0, #{})
-doc """
start/2 is a public runtime entrypoint in `vbeam_genserver`.
Parameters: `InitState :: term()`, `Options :: map()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec start(InitState :: term(), Options :: map()) ->
    {ok, pid()} | {error, term()}.

start(InitState, Options) when is_map(Options) ->
    Name = maps:get(name, Options, undefined),
    true = (Name =:= undefined) orelse is_atom(Name),
    case maps:get(name, Options, undefined) of
        undefined ->
            gen_server:start(?MODULE, {InitState, Options}, []);
        Name when is_atom(Name) ->
            gen_server:start({local, Name}, ?MODULE, {InitState, Options}, [])
    end.

%% Start a GenServer under a supervisor (linked).
%% V: spawn_supervised worker(...)
%% Erlang: {ok, Pid} = vbeam_genserver:start_link(State, Opts)
-doc """
start_link/2 is a public runtime entrypoint in `vbeam_genserver`.
Parameters: `InitState :: term()`, `Options :: map()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec start_link(InitState :: term(), Options :: map()) ->
    {ok, pid()} | {error, term()}.

start_link(InitState, Options) when is_map(Options) ->
    Name = maps:get(name, Options, undefined),
    true = (Name =:= undefined) orelse is_atom(Name),
    case maps:get(name, Options, undefined) of
        undefined ->
            gen_server:start_link(?MODULE, {InitState, Options}, []);
        Name when is_atom(Name) ->
            gen_server:start_link({local, Name}, ?MODULE, {InitState, Options}, [])
    end.

%% Synchronous call with default timeout (5 seconds).
%% Two styles:
%%   1. Handler module: call(Pid, Request) -> handler:handle_call(Request, State)
%%      Handler returns {Reply, NewState}
%%   2. Functional: call(Pid, fun(State) -> {Reply, NewState} end)
%%
%% V: lock counter { counter++ }   (returns ok, mutates state)
%% V: rlock counter { counter }    (returns value, state unchanged)
-doc """
call/2 is a public runtime entrypoint in `vbeam_genserver`.
Parameters: `pid() | atom()`, `term()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec call(pid() | atom(), call_payload()) -> term().

call(Server, Request) when is_pid(Server) orelse is_atom(Server) ->
    gen_server:call(Server, {vbeam_call, Request}, 5000).

%% Synchronous call with explicit timeout (milliseconds or infinity).
-spec call(pid() | atom(), call_payload(), timeout()) -> term().
call(Server, Request, Timeout)
  when (is_pid(Server) orelse is_atom(Server)),
       (Timeout =:= infinity orelse (is_integer(Timeout) andalso Timeout >= 0)) ->
    gen_server:call(Server, {vbeam_call, Request}, Timeout).

%% Asynchronous cast (fire and forget).
%% Handler module: cast(Pid, Msg) -> handler:handle_cast(Msg, State) -> NewState
%% Functional: cast(Pid, fun(State) -> NewState end)
%%
%% V: spawn fn() { shared_var.update(new_val) }
-doc """
cast/2 is a public runtime entrypoint in `vbeam_genserver`.
Parameters: `pid() | atom()`, `term()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec cast(pid() | atom(), cast_payload()) -> ok.

cast(Server, Msg) when is_pid(Server) orelse is_atom(Server) ->
    gen_server:cast(Server, {vbeam_cast, Msg}).

%% Stop the server gracefully.
%% V: (end of scope / explicit cleanup)
-spec stop(pid() | atom()) -> ok.

stop(Server) when is_pid(Server) orelse is_atom(Server) ->
    gen_server:stop(Server).

%% Read current state without modification.
%% V: rlock counter { counter }
%% This is a convenience for reading shared state atomically.
-doc """
get_state/1 is a public runtime entrypoint in `vbeam_genserver`.
Parameters: `pid() | atom()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec get_state(pid() | atom()) -> term().

get_state(Server) when is_pid(Server) orelse is_atom(Server) ->
    gen_server:call(Server, vbeam_get_state, 5000).

%% ============================================================================
%% gen_server Callbacks
%% ============================================================================

%% @private
-spec init({term(), map()}) -> {ok, #state{}}.

init({InitState, Options}) when is_map(Options) ->
    Handler = maps:get(handler, Options, undefined),
    MaxMailboxLen = maps:get(max_mailbox_len, Options, 10000),
    true = is_integer(MaxMailboxLen) andalso MaxMailboxLen >= 0,
    %% Start periodic mailbox monitoring
    erlang:send_after(5000, self(), check_mailbox),
    {ok, #state{value = InitState, handler = Handler, max_mailbox_len = MaxMailboxLen}}.

%% @private
-doc """
handle_call/3 is a public runtime entrypoint in `vbeam_genserver`.
Parameters: `term()`, `{pid(), term()}`, `#state{}`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec handle_call(call_request(), {pid(), term()}, #state{}) -> call_reply().

%% State read - returns current value

handle_call(vbeam_get_state, _From, State = #state{value = Value}) ->
    {reply, Value, State};

%% Functional style: caller passes a fun(State) -> {Reply, NewState}
handle_call({vbeam_call, Fun}, _From, State = #state{})
  when is_function(Fun, 1) ->
    {Reply, NewValue} = Fun(State#state.value),
    {reply, Reply, State#state{value = NewValue}};

%% Handler module style: delegate to handler:handle_call(Request, State)
handle_call({vbeam_call, Request}, _From, State = #state{handler = Handler})
  when Handler =/= undefined ->
    case Handler:handle_call(Request, State#state.value) of
        {Reply, NewValue} ->
            {reply, Reply, State#state{value = NewValue}};
        Reply ->
            %% If handler returns a single value, state is unchanged
            {reply, Reply, State}
    end;

%% No handler and not a function - treat request as direct value query
handle_call({vbeam_call, _Request}, _From, State) ->
    {reply, {error, no_handler}, State}.

%% @private
-doc """
handle_cast/2 is a public runtime entrypoint in `vbeam_genserver`.
Parameters: `term()`, `#state{}`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec handle_cast(cast_request(), #state{}) -> {noreply, #state{}}.

%% Functional style: caller passes a fun(State) -> NewState
handle_cast({vbeam_cast, Fun}, State = #state{})
  when is_function(Fun, 1) ->
    NewValue = Fun(State#state.value),
    {noreply, State#state{value = NewValue}};

%% Handler module style: delegate to handler:handle_cast(Msg, State)
handle_cast({vbeam_cast, Msg}, State = #state{handler = Handler})
  when Handler =/= undefined ->
    NewValue = Handler:handle_cast(Msg, State#state.value),
    {noreply, State#state{value = NewValue}};

%% No handler and not a function - ignore
handle_cast({vbeam_cast, _Msg}, State) ->
    {noreply, State}.

%% @private
-doc """
handle_info/2 is a public runtime entrypoint in `vbeam_genserver`.
Parameters: `term()`, `#state{}`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec handle_info(info_message(), #state{}) -> {noreply, #state{}}.
handle_info(check_mailbox, State = #state{max_mailbox_len = MaxLen}) ->
    case process_info(self(), message_queue_len) of
        {message_queue_len, Len} when Len > MaxLen ->
            error_logger:warning_msg("vbeam: mailbox overflow ~p (~p msgs, limit ~p)~n",
                                      [self(), Len, MaxLen]);
        _ -> ok
    end,
    erlang:send_after(5000, self(), check_mailbox),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.





