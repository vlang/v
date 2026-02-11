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
-export([
    %% Process spawning
    spawn_process/1,
    spawn_monitored/1,
    wait_for_result/1,
    wait_for_result/2,

    %% Direct message passing
    send_message/2,
    receive_message/0,
    receive_message/1,

    %% Simple channels (unbuffered, using process mailbox)
    channel_new/0,
    channel_send/2,
    channel_receive/1,
    channel_try_receive/1,
    channel_close/1,

    %% Utility
    sleep/1
]).

%% ============================================================================
%% Process Spawning
%% ============================================================================

%% Spawn an Erlang process (fire and forget)
%% V: spawn worker(1)
%% Erlang: spawn(fun() -> worker(1) end)
-spec spawn_process(fun(() -> any())) -> pid().
spawn_process(Fun) when is_function(Fun, 0) ->
    spawn(Fun).

%% Spawn with monitor for wait() support
%% Returns {Pid, MonitorRef, ResultRef} for later waiting
%% V: t := spawn compute(5)
-spec spawn_monitored(fun(() -> any())) -> {pid(), reference(), reference()}.
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
    {Pid, MonRef, ResultRef}.

%% Wait for a spawned task's result (blocking)
%% V: result := t.wait()
-spec wait_for_result({pid(), reference(), reference()}) -> any().
wait_for_result({Pid, MonRef, ResultRef}) ->
    wait_for_result({Pid, MonRef, ResultRef}, infinity).

%% Wait for a spawned task's result with timeout
-spec wait_for_result({pid(), reference(), reference()}, timeout()) -> any() | {error, timeout}.
wait_for_result({Pid, MonRef, ResultRef}, Timeout) ->
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
-spec send_message(pid(), any()) -> any().
send_message(Pid, Msg) ->
    Pid ! Msg.

%% Receive a message (blocking, infinite timeout)
-spec receive_message() -> any().
receive_message() ->
    receive
        Msg -> Msg
    end.

%% Receive a message with timeout (milliseconds)
%% Returns {ok, Msg} | timeout
-spec receive_message(timeout()) -> {ok, any()} | timeout.
receive_message(Timeout) ->
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
-spec channel_new() -> pid().
channel_new() ->
    spawn(fun() -> channel_loop(#{
        closed => false,
        waiting_sender => none,
        waiting_receiver => none
    }) end).

%% Internal channel process loop
channel_loop(State = #{closed := true}) ->
    %% Channel is closed, respond to any waiting operations
    receive
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

%% Send a value to a channel (blocking)
%% V: ch <- 42
-spec channel_send(pid(), any()) -> ok | closed.
channel_send(Ch, Value) ->
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
channel_receive(Ch) ->
    Ref = make_ref(),
    Ch ! {recv, self(), Ref},
    receive
        {Ref, {ok, Value}} -> {ok, Value};
        {Ref, closed} -> closed;
        {Ref, {error, Reason}} -> erlang:error({channel_recv_error, Reason})
    end.

%% Try to receive (non-blocking)
%% V: select { x := <-ch { ... } else { ... } }
-spec channel_try_receive(pid()) -> {ok, any()} | empty | closed.
channel_try_receive(Ch) ->
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
channel_close(Ch) ->
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
-spec sleep(non_neg_integer()) -> ok.
sleep(Ms) when is_integer(Ms), Ms >= 0 ->
    timer:sleep(Ms),
    ok.
