%% vbeam_concurrency - Concurrency primitives for V language BEAM backend
%% Maps V's spawn, channels, and shared variables to Erlang/OTP patterns
%%
%% V Concept -> Erlang Implementation:
%%   spawn fn()     -> erlang:spawn/1
%%   spawn + wait() -> spawn_monitor + receive
%%   chan T         -> process mailbox (simple) or gen_server (buffered)
%%   shared T       -> gen_server state
%%   lock/rlock     -> gen_server calls

-module(vbeam_concurrency).

-define(VSN, "1.0.0").
-vsn(?VSN).

-moduledoc """
Provides concurrency primitives and scheduling helpers.
""".






-export([
    %% Process spawning
    spawn_process/1,
    spawn_process_supervised/1,
    spawn_monitored/1,
    spawn_monitored_supervised/1,
    wait_for_result/1,
    wait_for_result/2,

    %% Direct message passing
    send_message/2,
    receive_message/0,
    receive_message/1,

    %% Simple channels (unbuffered, using process mailbox)
    channel_new/0,
    channel_new_supervised/0,
    channel_send/2,
    channel_receive/1,
    channel_try_receive/1,
    channel_close/1,

    %% Utility
    sleep/1
]).

-export([terminate/2, code_change/3, format_status/1]).

%% Export protocol types for cross-module contracts
-export_type([
    monitored_task/0,
    task_result/0,
    task_msg/0,
    monitor_down_msg/0,
    wait_result_msg/0,
    channel_msg/0,
    channel_reply/0,
    runtime_msg/0
]).

%% Inter-process message protocol types
-type monitored_task() :: {pid(), reference(), reference()}.
-type task_result() :: {ok, term()} | {error, {atom(), term(), list()}}.
-type task_msg() :: {reference(), task_result()}.
-type monitor_down_msg() :: {'DOWN', reference(), process, pid(), term()}.
-type wait_result_msg() :: task_msg() | monitor_down_msg().
-type channel_msg() ::
    check_mailbox
    | {recv, pid(), reference()}
    | {send, pid(), reference(), term()}
    | {try_recv, pid(), reference()}
    | {close, pid(), reference()}
    | stop.
-type channel_reply() ::
    {reference(), ok | closed | empty | {ok, term()} | {error, term()}}.
-type runtime_msg() :: wait_result_msg() | channel_msg() | channel_reply() | term().

%% ============================================================================
%% Process Spawning
%% ============================================================================

%% Spawn an Erlang process (fire and forget)
%% V: spawn worker(1)
%% Erlang: spawn(fun() -> worker(1) end)
-doc """
spawn_process/1 is a public runtime entrypoint in `vbeam_concurrency`.
Parameters: `fun((`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec spawn_process(fun(() -> any())) -> pid().

spawn_process(Fun) when is_function(Fun, 0) ->
    Pid = spawn(Fun),
    true = is_pid(Pid),
    Pid.

%% Spawn an Erlang process under the dynamic runtime supervisor.
-doc """
spawn_process_supervised/1 is a public runtime entrypoint in `vbeam_concurrency`.
Parameters: `fun((`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec spawn_process_supervised(fun(() -> any())) -> pid().
spawn_process_supervised(Fun) when is_function(Fun, 0) ->
    case vbeam_supervisor:spawn_supervised(Fun, #{restart => temporary}) of
        {ok, Pid} ->
            true = is_pid(Pid),
            Pid;
        {error, _Reason} ->
            spawn_process(Fun)
    end.

%% Spawn with monitor for wait() support
%% Returns {Pid, MonitorRef, ResultRef} for later waiting
%% V: t := spawn compute(5)
-spec spawn_monitored(fun(() -> any())) -> monitored_task().

spawn_monitored(Fun) when is_function(Fun, 0) ->
    Parent = self(),
    ResultRef = make_ref(),
    {Pid, MonRef} = spawn_monitor(fun() ->
        Result = try
            {ok, Fun()}
        catch
            Class:Reason:Stack ->
                {error, {Class, Reason, Stack}}
        end,
        Parent ! {ResultRef, Result}
    end),
    true = is_pid(Pid),
    true = is_reference(MonRef) andalso is_reference(ResultRef),
    {Pid, MonRef, ResultRef}.

%% Spawn with monitor under the dynamic runtime supervisor.
-doc """
spawn_monitored_supervised/1 is a public runtime entrypoint in `vbeam_concurrency`.
Parameters: `fun((`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec spawn_monitored_supervised(fun(() -> any())) -> monitored_task().
spawn_monitored_supervised(Fun) when is_function(Fun, 0) ->
    Parent = self(),
    ResultRef = make_ref(),
    Runner = fun() ->
        Result = try
            {ok, Fun()}
        catch
            Class:Reason:Stack ->
                {error, {Class, Reason, Stack}}
        end,
        Parent ! {ResultRef, Result}
    end,
    Pid = case vbeam_supervisor:spawn_supervised(Runner, #{restart => temporary}) of
        {ok, SupervisedPid} ->
            SupervisedPid;
        {error, _Reason} ->
            spawn(Runner)
    end,
    MonRef = monitor(process, Pid),
    true = is_pid(Pid),
    true = is_reference(MonRef) andalso is_reference(ResultRef),
    {Pid, MonRef, ResultRef}.

%% Wait for a spawned task's result (blocking)
%% V: result := t.wait()
-doc """
wait_for_result/1 is a public runtime entrypoint in `vbeam_concurrency`.
Parameters: `{pid(), reference(), reference()}`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec wait_for_result(monitored_task()) -> any().
wait_for_result(Task = {Pid, MonRef, ResultRef})
  when is_pid(Pid), is_reference(MonRef), is_reference(ResultRef) ->
    wait_for_result(Task, infinity).

%% Wait for a spawned task's result with timeout
-spec wait_for_result(monitored_task(), timeout()) -> any() | {error, timeout}.
wait_for_result({Pid, MonRef, ResultRef}, Timeout)
  when is_pid(Pid), is_reference(MonRef), is_reference(ResultRef),
       (Timeout =:= infinity orelse (is_integer(Timeout) andalso Timeout >= 0)) ->
    receive
        {ResultRef, {ok, Value}} ->
            demonitor(MonRef, [flush]),
            Value;
        {ResultRef, {error, {Class, Reason, Stack}}} ->
            demonitor(MonRef, [flush]),
            erlang:raise(Class, Reason, Stack);
        {'DOWN', MonRef, process, Pid, Reason} ->
            %% Process died without sending result
            erlang:error({spawn_failed, Reason})
    after Timeout ->
        demonitor(MonRef, [flush]),
        {error, timeout}
    end.

%% ============================================================================
%% Direct Message Passing
%% ============================================================================

%% Send a message to a process
-doc """
send_message/2 is a public runtime entrypoint in `vbeam_concurrency`.
Parameters: `pid()`, `any()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec send_message(pid(), runtime_msg()) -> runtime_msg().

send_message(Pid, Msg) when is_pid(Pid) ->
    true = is_process_alive(Pid),
    Pid ! Msg.

%% Receive a message (blocking, infinite timeout)
-spec receive_message() -> runtime_msg().

receive_message() ->
    receive
        Msg -> Msg
    end.

%% Receive a message with timeout (milliseconds)
%% Returns {ok, Msg} | timeout
-doc """
receive_message/1 is a public runtime entrypoint in `vbeam_concurrency`.
Parameters: `timeout()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec receive_message(timeout()) -> {ok, runtime_msg()} | timeout.
receive_message(Timeout)
  when Timeout =:= infinity orelse (is_integer(Timeout) andalso Timeout >= 0) ->
    receive
        Msg -> {ok, Msg}
    after Timeout ->
        timeout
    end.

%% ============================================================================
%% Simple Channels (Unbuffered)
%%
%% Implementation: A channel is a process that mediates between senders
%% and receivers. For unbuffered channels, sender blocks until receiver ready.
%% ============================================================================

%% Create a new unbuffered channel
%% V: ch := chan int
-doc """
channel_new/0 is a public runtime entrypoint in `vbeam_concurrency`.
No parameters.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec channel_new() -> pid().

channel_new() ->
    spawn(fun start_channel_process/0).

%% Create a new unbuffered channel under dynamic supervision.
-doc """
channel_new_supervised/0 is a public runtime entrypoint in `vbeam_concurrency`.
No parameters.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec channel_new_supervised() -> pid().
channel_new_supervised() ->
    case vbeam_supervisor:spawn_supervised(fun start_channel_process/0, #{restart => temporary}) of
        {ok, Pid} ->
            Pid;
        {error, _Reason} ->
            channel_new()
    end.

-spec start_channel_process() -> ok.
start_channel_process() ->
    %% Start periodic mailbox monitoring for channel process
    erlang:send_after(5000, self(), check_mailbox),
    channel_loop(#{
        closed => false,
        waiting_sender => none,
        waiting_receiver => none,
        max_mailbox_len => 10000
    }).

%% Internal channel process loop
channel_loop(State = #{closed := true}) ->
    %% Channel is closed, respond to any waiting operations
    receive
        check_mailbox ->
            check_channel_mailbox(State),
            channel_loop(State);
        {recv, From, Ref} ->
            From ! {Ref, closed},
            channel_loop(State);
        {send, From, Ref, _Value} ->
            From ! {Ref, closed},
            channel_loop(State);
        {try_recv, From, Ref} ->
            From ! {Ref, closed},
            channel_loop(State);
        {close, From, Ref} ->
            From ! {Ref, ok},
            channel_loop(State);
        stop ->
            ok
    end;

channel_loop(State = #{waiting_receiver := none, waiting_sender := none}) ->
    %% Neither sender nor receiver waiting
    receive
        check_mailbox ->
            check_channel_mailbox(State),
            channel_loop(State);

        {recv, From, Ref} ->
            %% Receiver arrived first, wait for sender
            channel_loop(State#{waiting_receiver := {From, Ref}});

        {send, From, Ref, Value} ->
            %% Sender arrived first, wait for receiver
            channel_loop(State#{waiting_sender := {From, Ref, Value}});

        {try_recv, From, Ref} ->
            %% Non-blocking receive, nothing available
            From ! {Ref, empty},
            channel_loop(State);

        {close, From, Ref} ->
            From ! {Ref, ok},
            channel_loop(State#{closed := true});

        stop ->
            ok
    end;

channel_loop(State = #{waiting_receiver := {RecvFrom, RecvRef}, waiting_sender := none}) ->
    %% Receiver waiting, looking for sender
    receive
        check_mailbox ->
            check_channel_mailbox(State),
            channel_loop(State);

        {send, SendFrom, SendRef, Value} ->
            %% Sender arrived, complete the exchange
            RecvFrom ! {RecvRef, {ok, Value}},
            SendFrom ! {SendRef, ok},
            channel_loop(State#{waiting_receiver := none});

        {recv, From, Ref} ->
            %% Another receiver - queue behind (simple: reject)
            From ! {Ref, {error, receiver_waiting}},
            channel_loop(State);

        {try_recv, From, Ref} ->
            From ! {Ref, empty},
            channel_loop(State);

        {close, From, Ref} ->
            RecvFrom ! {RecvRef, closed},
            From ! {Ref, ok},
            channel_loop(State#{closed := true, waiting_receiver := none});

        stop ->
            RecvFrom ! {RecvRef, closed},
            ok
    end;

channel_loop(State = #{waiting_sender := {SendFrom, SendRef, Value}, waiting_receiver := none}) ->
    %% Sender waiting, looking for receiver
    receive
        check_mailbox ->
            check_channel_mailbox(State),
            channel_loop(State);

        {recv, RecvFrom, RecvRef} ->
            %% Receiver arrived, complete the exchange
            RecvFrom ! {RecvRef, {ok, Value}},
            SendFrom ! {SendRef, ok},
            channel_loop(State#{waiting_sender := none});

        {send, From, Ref, _} ->
            %% Another sender - queue behind (simple: reject)
            From ! {Ref, {error, sender_waiting}},
            channel_loop(State);

        {try_recv, RecvFrom, RecvRef} ->
            %% Non-blocking receive can take the waiting value
            RecvFrom ! {RecvRef, {ok, Value}},
            SendFrom ! {SendRef, ok},
            channel_loop(State#{waiting_sender := none});

        {close, From, Ref} ->
            SendFrom ! {SendRef, closed},
            From ! {Ref, ok},
            channel_loop(State#{closed := true, waiting_sender := none});

        stop ->
            SendFrom ! {SendRef, closed},
            ok
    end.

%% Helper function for mailbox monitoring in channel process
check_channel_mailbox(State) ->
    MaxLen = maps:get(max_mailbox_len, State, 10000),
    case process_info(self(), message_queue_len) of
        {message_queue_len, Len} when Len > MaxLen ->
            error_logger:warning_msg("vbeam: channel mailbox overflow ~p (~p msgs, limit ~p)~n",
                                      [self(), Len, MaxLen]);
        _ -> ok
    end,
    erlang:send_after(5000, self(), check_mailbox).

%% Send a value to a channel (blocking)
%% V: ch <- 42
-doc """
channel_send/2 is a public runtime entrypoint in `vbeam_concurrency`.
Parameters: `pid()`, `any()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec channel_send(pid(), any()) -> ok | closed.

channel_send(Ch, Value) when is_pid(Ch) ->
    true = is_process_alive(Ch),
    Ref = make_ref(),
    Ch ! {send, self(), Ref, Value},
    receive
        {Ref, ok} -> ok;
        {Ref, closed} -> closed;
        {Ref, {error, Reason}} -> erlang:error({channel_send_error, Reason})
    end.

%% Receive a value from a channel (blocking)
%% V: value := <-ch
-spec channel_receive(pid()) -> {ok, any()} | closed.

channel_receive(Ch) when is_pid(Ch) ->
    true = is_process_alive(Ch),
    Ref = make_ref(),
    Ch ! {recv, self(), Ref},
    receive
        {Ref, {ok, Value}} -> {ok, Value};
        {Ref, closed} -> closed;
        {Ref, {error, Reason}} -> erlang:error({channel_recv_error, Reason})
    end.

%% Try to receive (non-blocking)
%% V: select { x := <-ch { ... } else { ... } }
-doc """
channel_try_receive/1 is a public runtime entrypoint in `vbeam_concurrency`.
Parameters: `pid()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec channel_try_receive(pid()) -> {ok, any()} | empty | closed.
channel_try_receive(Ch) when is_pid(Ch) ->
    true = is_process_alive(Ch),
    Ref = make_ref(),
    Ch ! {try_recv, self(), Ref},
    receive
        {Ref, Result} -> Result
    after 0 ->
        empty
    end.

%% Close a channel
%% V: ch.close()
-spec channel_close(pid()) -> ok.
channel_close(Ch) when is_pid(Ch) ->
    true = is_process_alive(Ch),
    Ref = make_ref(),
    Ch ! {close, self(), Ref},
    receive
        {Ref, ok} -> ok
    after 5000 ->
        %% Channel process might be stuck, force stop
        Ch ! stop,
        ok
    end.

%% ============================================================================
%% Utility Functions
%% ============================================================================

%% Sleep for given milliseconds
%% V: time.sleep(100 * time.millisecond)
-doc """
sleep/1 is a public runtime entrypoint in `vbeam_concurrency`.
Parameters: `non_neg_integer()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec sleep(non_neg_integer()) -> ok.
sleep(Ms) when is_integer(Ms), Ms >= 0 ->
    timer:sleep(Ms),
    ok.

%% OTP hot-code callbacks (Rule 30)
-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    {ok, State}.

-spec format_status(map()) -> map().
format_status(Status) ->
    Status.


