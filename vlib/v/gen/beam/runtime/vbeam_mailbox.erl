%% vbeam_mailbox - Mailbox monitoring utilities for V language BEAM backend
%%
%% Implements Iron Code Rule 26: Bounded mailboxes with monitoring
%% Prevents unbounded memory growth in process mailboxes
%%
%% Usage:
%%   1. Quick check in any process: vbeam_mailbox:check() or check(MaxLen)
%%   2. Monitor another process: vbeam_mailbox:monitor_start(Pid)
%%
%% The check functions return {warning, Len} when threshold exceeded,
%% and also log warnings to error_logger for visibility.

-module(vbeam_mailbox).

-moduledoc """
Provides mailbox and monitor utilities for V runtime processes.
""".






-export([check/0, check/1, monitor_start/1, monitor_start/2]).

%% Quick mailbox check with default limit (10000 messages)
%% Returns ok | {warning, CurrentLen}
%% V: vbeam_mailbox.check()
-doc """
check/0 is a public runtime entrypoint in `vbeam_mailbox`.
No parameters.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec check() -> ok | {warning, integer()}.
check() -> check(10000).

%% Quick mailbox check with custom limit
%% Returns ok | {warning, CurrentLen}
%% Logs warning to error_logger when threshold exceeded
%% V: vbeam_mailbox.check(5000)
-spec check(MaxLen :: integer()) -> ok | {warning, integer()}.

check(MaxLen) when is_integer(MaxLen), MaxLen >= 0 ->
    case process_info(self(), message_queue_len) of
        {message_queue_len, Len} when Len > MaxLen ->
            error_logger:warning_msg("vbeam: mailbox overflow ~p (~p msgs, limit ~p)~n",
                                      [self(), Len, MaxLen]),
            {warning, Len};
        _ -> ok
    end.

%% Start a monitoring process for another Pid (default 5 second interval)
%% The monitor checks mailbox size periodically and logs warnings
%% Returns reference to monitoring process
%% V: vbeam_mailbox.monitor_start(pid)
-doc """
monitor_start/1 is a public runtime entrypoint in `vbeam_mailbox`.
Parameters: `pid()`.
Returns the result value of this runtime operation.
Side effects: May perform runtime side effects such as I/O, process interaction, or external state updates.
""".
-spec monitor_start(pid()) -> pid().

monitor_start(Pid) when is_pid(Pid) ->
    true = is_process_alive(Pid),
    monitor_start(Pid, 5000).

%% Start a monitoring process with custom interval (milliseconds)
%% V: vbeam_mailbox.monitor_start(pid, 10000)  // 10 second checks
-spec monitor_start(pid(), Interval :: integer()) -> pid().

monitor_start(Pid, Interval) when is_pid(Pid), is_integer(Interval), Interval > 0 ->
    true = is_process_alive(Pid),
    spawn_link(fun() -> monitor_loop(Pid, Interval, 10000) end),
    ok.

%% Internal monitoring loop
%% Checks target process mailbox size at regular intervals
%% Stops automatically when target process dies
monitor_loop(Pid, Interval, MaxLen) ->
    case is_process_alive(Pid) of
        false -> ok;
        true ->
            case process_info(Pid, message_queue_len) of
                {message_queue_len, Len} when Len > MaxLen ->
                    error_logger:warning_msg("vbeam: mailbox overflow ~p (~p msgs, limit ~p)~n",
                                              [Pid, Len, MaxLen]);
                _ -> ok
            end,
            timer:sleep(Interval),
            monitor_loop(Pid, Interval, MaxLen)
    end.






