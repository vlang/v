-module(vbeam_process).

-moduledoc """
Provides process, port, and signal helpers for runtime tasks.
""".






-export([spawn_supervised/1, spawn_link_supervised/1,
         run/2, wait/1, kill/1, signal_term/1, signal_stop/1, signal_cont/1,
         stdin_write/2, stdout_slurp/1, stdout_read/1]).

%% Export protocol types for cross-module contracts
-export_type([process_msg/0]).

%% Port/process message protocol consumed by this module
-type process_msg() :: {port(), {exit_status, integer()}} | {port(), {data, binary()}}.

%% Spawn a function under the dynamic runtime supervisor.
-doc """
spawn_supervised/1 is a public runtime entrypoint in `vbeam_process`.
Parameters: `fun((`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec spawn_supervised(fun(() -> term())) -> pid().
spawn_supervised(Fun) when is_function(Fun, 0) ->
    case vbeam_supervisor:spawn_supervised(Fun) of
        {ok, Pid} ->
            Pid;
        {error, _Reason} ->
            spawn(Fun)
    end.

%% Spawn and link a function under the dynamic runtime supervisor.
-doc """
spawn_link_supervised/1 is a public runtime entrypoint in `vbeam_process`.
Parameters: `fun((`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec spawn_link_supervised(fun(() -> term())) -> pid().
spawn_link_supervised(Fun) when is_function(Fun, 0) ->
    case vbeam_supervisor:spawn_supervised(Fun, #{restart => temporary}) of
        {ok, Pid} ->
            link(Pid),
            Pid;
        {error, _Reason} ->
            spawn_link(Fun)
    end.

%% Run a process: returns {ok, Port} or {error, Reason}
%% Command and Args are binaries (V strings).
-doc """
run/2 is a public runtime entrypoint in `vbeam_process`.
Parameters: `binary()`, `[binary()]`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec run(binary(), [binary()]) -> {ok, port()} | {error, term()}.
run(Command, Args)
  when is_binary(Command), is_list(Args) ->
    true = lists:all(fun is_binary/1, Args),
    Cmd = binary_to_list(Command),
    ArgsList = [binary_to_list(A) || A <- Args],
    try
        Port = open_port({spawn_executable, Cmd},
                        [stream, binary, exit_status, stderr_to_stdout,
                         {args, ArgsList}]),
        {ok, Port}
    catch _:Reason -> {error, Reason}
    end.

%% Wait for process to exit, return exit code
-doc """
wait/1 is a public runtime entrypoint in `vbeam_process`.
Parameters: `port()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec wait(port()) -> integer().

wait(Port) when is_port(Port) ->
    true = erlang:port_info(Port) =/= undefined,
    wait_loop(Port).

-spec wait_loop(port()) -> integer().
wait_loop(Port) ->
    receive
        {Port, {exit_status, Code}} -> Code;
        {Port, {data, _Data}} ->
            %% Discard data while waiting for exit
            wait_loop(Port)
    end.

%% Force kill the OS process and close the port
-doc """
kill/1 is a public runtime entrypoint in `vbeam_process`.
Parameters: `port()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec kill(port()) -> ok.

kill(Port) when is_port(Port) ->
    true = erlang:port_info(Port) =/= undefined,
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            os:cmd("kill -9 " ++ integer_to_list(OsPid));
        undefined -> ok
    end,
    catch port_close(Port),
    ok.

%% Send SIGTERM to the OS process
-spec signal_term(port()) -> ok | {error, not_running}.

signal_term(Port) when is_port(Port) ->
    true = erlang:port_info(Port) =/= undefined,
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            os:cmd("kill -TERM " ++ integer_to_list(OsPid)),
            ok;
        undefined -> {error, not_running}
    end.

%% Send SIGSTOP to the OS process
-doc """
signal_stop/1 is a public runtime entrypoint in `vbeam_process`.
Parameters: `port()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec signal_stop(port()) -> ok | {error, not_running}.

signal_stop(Port) when is_port(Port) ->
    true = erlang:port_info(Port) =/= undefined,
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            os:cmd("kill -STOP " ++ integer_to_list(OsPid)),
            ok;
        undefined -> {error, not_running}
    end.

%% Send SIGCONT to the OS process
-spec signal_cont(port()) -> ok | {error, not_running}.

signal_cont(Port) when is_port(Port) ->
    true = erlang:port_info(Port) =/= undefined,
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            os:cmd("kill -CONT " ++ integer_to_list(OsPid)),
            ok;
        undefined -> {error, not_running}
    end.

%% Write binary data to the process's stdin
-doc """
stdin_write/2 is a public runtime entrypoint in `vbeam_process`.
Parameters: `port()`, `iodata()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec stdin_write(port(), iodata()) -> ok.
stdin_write(Port, Data)
  when is_port(Port), (is_binary(Data) orelse is_list(Data)) ->
    true = erlang:port_info(Port) =/= undefined,
    port_command(Port, Data),
    ok.

%% Read all available stdout data (non-blocking collect)
-spec stdout_slurp(port()) -> binary().
stdout_slurp(Port) when is_port(Port) ->
    true = erlang:port_info(Port) =/= undefined,
    stdout_slurp(Port, <<>>).

-spec stdout_slurp(port(), binary()) -> binary().
stdout_slurp(Port, Acc) ->
    receive
        {Port, {data, Data}} -> stdout_slurp(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, _}} -> Acc
    after 0 -> Acc
    end.

%% Read one chunk of stdout (blocking)
-doc """
stdout_read/1 is a public runtime entrypoint in `vbeam_process`.
Parameters: `port()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec stdout_read(port()) -> binary().
stdout_read(Port) when is_port(Port) ->
    true = erlang:port_info(Port) =/= undefined,
    receive
        {Port, {data, Data}} -> Data;
        {Port, {exit_status, _Code}} -> <<>>
    end.




