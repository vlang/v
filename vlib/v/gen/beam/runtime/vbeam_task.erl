%% vbeam_task - Async task module for V language BEAM backend
%%
%% Provides a higher-level abstraction over raw spawn_monitor for V programs
%% that need to run concurrent work and collect results. Similar to Elixir's
%% Task module.
%%
%% V Concept -> Task Pattern:
%%   t := spawn compute(5)    -> TaskRef = vbeam_task:async(fun() -> compute(5) end)
%%   result := t.wait()       -> Result = vbeam_task:await(TaskRef)
%%   results := parallel(fns) -> Results = vbeam_task:async_stream(Funs, #{})
%%
%% Key features:
%%   - Automatic monitor cleanup on await/yield
%%   - Configurable timeouts (default 5 seconds)
%%   - Linked execution: task dies if caller dies (and vice versa)
%%   - Non-blocking yield for polling patterns
%%
%% A TaskRef is an opaque map:
%%   #{pid => Pid, ref => MonitorRef, result_ref => ResultRef}

-module(vbeam_task).

-moduledoc """
Provides asynchronous task helpers for V runtime execution.
""".







-export([
    async/1,
    async/2,
    async_supervised/1,
    await/1, await/2,
    async_stream/1, async_stream/2,
    yield/1, yield/2
]).

%% Export protocol types for cross-module contracts
-export_type([
    task_ref/0,
    task_result/0,
    task_msg/0,
    task_down_msg/0,
    task_protocol_msg/0,
    yield_result/0
]).

%% Default timeout for await (5 seconds, matching Elixir's Task default)
-define(DEFAULT_TIMEOUT, 5000).

%% Inter-process task protocol types
-type task_ref() :: #{pid := pid(), ref := reference(), result_ref := reference()}.
-type task_result() :: {ok, term()} | {error, {atom(), term(), list()}}.
-type task_msg() :: {reference(), task_result()}.
-type task_down_msg() :: {'DOWN', reference(), process, pid(), term()}.
-type task_protocol_msg() :: task_msg() | task_down_msg().
-type yield_result() :: {ok, term()} | {error, term()} | timeout.

%% ============================================================================
%% API Functions
%% ============================================================================

%% Spawn an async task linked to the caller.
%% The task runs Fun and sends the result back.
%% If the caller dies, the task dies too (linked).
%%
%% V: t := spawn compute(5)
%% Erlang: TaskRef = vbeam_task:async(fun() -> compute(5) end)
%%
%% Returns a task reference map for use with await/yield.
-doc """
async/1 is a public runtime entrypoint in `vbeam_task`.
Parameters: `fun((`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec async(fun(() -> term())) -> task_ref().

async(Fun) when is_function(Fun, 0) ->
    TaskRef = async(Fun, #{}),
    true = maps:is_key(pid, TaskRef) andalso maps:is_key(ref, TaskRef)
           andalso maps:is_key(result_ref, TaskRef),
    TaskRef.

%% Spawn an async task with options.
%% Options:
%%   timeout => integer() | infinity  (for internal use, not enforced at spawn)
%%   link => boolean()                (default: true)
-spec async(fun(() -> term()), map()) -> task_ref().
async(Fun, Opts) when is_function(Fun, 0), is_map(Opts) ->
    Parent = self(),
    ResultRef = make_ref(),
    Link = maps:get(link, Opts, true),

    Pid = case Link of
        true ->
            spawn_link(fun() ->
                Result = try
                    {ok, Fun()}
                catch
                    Class:Reason:Stack ->
                        {error, {Class, Reason, Stack}}
                end,
                Parent ! {ResultRef, Result}
            end);
        false ->
            spawn(fun() ->
                Result = try
                    {ok, Fun()}
                catch
                    Class:Reason:Stack ->
                        {error, {Class, Reason, Stack}}
                end,
                Parent ! {ResultRef, Result}
            end)
    end,

    MonRef = monitor(process, Pid),

    #{
        pid => Pid,
        ref => MonRef,
        result_ref => ResultRef
    }.

%% Spawn an async task under the dynamic runtime supervisor.
%% Returns {Pid, Ref}; the task sends `{task_result, Ref, Result}` to caller.
-doc """
async_supervised/1 is a public runtime entrypoint in `vbeam_task`.
Parameters: `fun((`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec async_supervised(fun(() -> term())) -> {pid(), reference()}.
async_supervised(Fun) when is_function(Fun, 0) ->
    Ref = make_ref(),
    Parent = self(),
    Runner = fun() ->
        Result = try Fun() of
            Val ->
                {ok, Val}
        catch
            Class:Reason:Stack ->
                {error, {Class, Reason, Stack}}
        end,
        Parent ! {task_result, Ref, Result}
    end,
    case vbeam_supervisor:spawn_supervised(Runner, #{restart => temporary}) of
        {ok, Pid} ->
            {Pid, Ref};
        {error, _Reason} ->
            {spawn(Runner), Ref}
    end.

%% Wait for a task result with default timeout (5 seconds).
%% Raises an error if the task fails or times out.
%%
%% V: result := t.wait()
%% Erlang: Result = vbeam_task:await(TaskRef)
-doc """
await/1 is a public runtime entrypoint in `vbeam_task`.
Parameters: `map()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec await(task_ref()) -> term().

await(TaskRef) when is_map(TaskRef), map_size(TaskRef) > 0 ->
    await(TaskRef, ?DEFAULT_TIMEOUT).

%% Wait for a task result with explicit timeout (milliseconds or infinity).
%%
%% V: result := t.wait_timeout(10000)
%% Erlang: Result = vbeam_task:await(TaskRef, 10000)
%%
%% Returns the task's return value.
%% Raises {task_timeout, TaskRef} on timeout.
%% Re-raises the task's exception if it crashed.
-spec await(task_ref(), timeout()) -> term().
await(#{ref := MonRef, result_ref := ResultRef, pid := Pid} = _TaskRef, Timeout)
  when Timeout =:= infinity orelse (is_integer(Timeout) andalso Timeout >= 0) ->
    receive
        {ResultRef, {ok, Value}} ->
            demonitor(MonRef, [flush]),
            Value;
        {ResultRef, {error, {Class, Reason, Stack}}} ->
            demonitor(MonRef, [flush]),
            erlang:raise(Class, Reason, Stack);
        {'DOWN', MonRef, process, Pid, Reason} ->
            %% Task process died without sending a result
            erlang:error({task_failed, Reason})
    after Timeout ->
        demonitor(MonRef, [flush]),
        %% Kill the task on timeout (it's linked, but be explicit)
        exit(Pid, kill),
        %% Flush any late messages
        receive
            {ResultRef, _} -> ok
        after 0 -> ok
        end,
        erlang:error({task_timeout, Timeout})
    end.

%% Run multiple functions concurrently and collect all results.
%% Functions are started in order; results are returned in the same order.
%%
%% V: results := parallel([fn1, fn2, fn3])
%% Erlang: Results = vbeam_task:async_stream([Fun1, Fun2, Fun3])
%%
%% Uses default timeout (5 seconds per task).
-doc """
async_stream/1 is a public runtime entrypoint in `vbeam_task`.
Parameters: `[fun((`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec async_stream([fun(() -> term())]) -> [term()].

async_stream(Funs) when is_list(Funs) ->
    true = lists:all(fun(F) -> is_function(F, 0) end, Funs),
    async_stream(Funs, #{}).

%% Run multiple functions concurrently with options.
%% Options:
%%   timeout => integer() | infinity  (per-task timeout, default 5000)
%%   ordered => boolean()             (preserve order, default true)
%%
%% Returns results in the same order as the input functions.
-spec async_stream([fun(() -> term())], map()) -> [term()].
async_stream(Funs, Opts) when is_list(Funs), is_map(Opts) ->
    true = lists:all(fun(F) -> is_function(F, 0) end, Funs),
    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),

    %% Start all tasks
    TaskRefs = lists:map(fun(Fun) -> async(Fun, #{link => false}) end, Funs),

    %% Await all results in order
    lists:map(fun(TaskRef) -> await(TaskRef, Timeout) end, TaskRefs).

%% Non-blocking check for task completion.
%% Returns {ok, Result} if done, or timeout if not yet finished.
%%
%% V: if result := t.try_wait() { ... }
%% Erlang:
%%   case vbeam_task:yield(TaskRef) of
%%       {ok, Result} -> ...;
%%       timeout -> ...
%%   end
-doc """
yield/1 is a public runtime entrypoint in `vbeam_task`.
Parameters: `map()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec yield(task_ref()) -> {ok, term()} | timeout.

yield(TaskRef) when is_map(TaskRef), map_size(TaskRef) > 0 ->
    yield(TaskRef, 0).

%% Non-blocking check with a brief wait period.
%% Timeout of 0 means check immediately without waiting.
%%
%% Returns:
%%   {ok, Value}  - Task completed successfully
%%   {error, Reason} - Task crashed
%%   timeout      - Task still running
-spec yield(task_ref(), timeout()) -> yield_result().
yield(#{ref := MonRef, result_ref := ResultRef, pid := Pid}, Timeout)
  when Timeout =:= infinity orelse (is_integer(Timeout) andalso Timeout >= 0) ->
    receive
        {ResultRef, {ok, Value}} ->
            demonitor(MonRef, [flush]),
            {ok, Value};
        {ResultRef, {error, {_Class, Reason, _Stack}}} ->
            demonitor(MonRef, [flush]),
            {error, Reason};
        {'DOWN', MonRef, process, Pid, Reason} ->
            {error, {task_failed, Reason}}
    after Timeout ->
        timeout
    end.




