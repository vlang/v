-module(vbeam_process).
-export([run/2, wait/1, kill/1, signal_term/1, signal_stop/1, signal_cont/1,
         stdin_write/2, stdout_slurp/1, stdout_read/1]).

%% Run a process: returns {ok, Port} or {error, Reason}
%% Command and Args are binaries (V strings).
-spec run(binary(), [binary()]) -> {ok, port()} | {error, term()}.
run(Command, Args) ->
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
-spec wait(port()) -> integer().
wait(Port) ->
    wait_loop(Port).

wait_loop(Port) ->
    receive
        {Port, {exit_status, Code}} -> Code;
        {Port, {data, _Data}} ->
            %% Discard data while waiting for exit
            wait_loop(Port)
    end.

%% Force kill the OS process and close the port
-spec kill(port()) -> ok.
kill(Port) ->
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            os:cmd("kill -9 " ++ integer_to_list(OsPid));
        undefined -> ok
    end,
    catch port_close(Port),
    ok.

%% Send SIGTERM to the OS process
-spec signal_term(port()) -> ok | {error, not_running}.
signal_term(Port) ->
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            os:cmd("kill -TERM " ++ integer_to_list(OsPid)),
            ok;
        undefined -> {error, not_running}
    end.

%% Send SIGSTOP to the OS process
-spec signal_stop(port()) -> ok | {error, not_running}.
signal_stop(Port) ->
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            os:cmd("kill -STOP " ++ integer_to_list(OsPid)),
            ok;
        undefined -> {error, not_running}
    end.

%% Send SIGCONT to the OS process
-spec signal_cont(port()) -> ok | {error, not_running}.
signal_cont(Port) ->
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            os:cmd("kill -CONT " ++ integer_to_list(OsPid)),
            ok;
        undefined -> {error, not_running}
    end.

%% Write binary data to the process's stdin
-spec stdin_write(port(), iodata()) -> ok.
stdin_write(Port, Data) ->
    port_command(Port, Data),
    ok.

%% Read all available stdout data (non-blocking collect)
-spec stdout_slurp(port()) -> binary().
stdout_slurp(Port) ->
    stdout_slurp(Port, <<>>).

stdout_slurp(Port, Acc) ->
    receive
        {Port, {data, Data}} -> stdout_slurp(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, _}} -> Acc
    after 0 -> Acc
    end.

%% Read one chunk of stdout (blocking)
-spec stdout_read(port()) -> binary().
stdout_read(Port) ->
    receive
        {Port, {data, Data}} -> Data;
        {Port, {exit_status, _Code}} -> <<>>
    end.
