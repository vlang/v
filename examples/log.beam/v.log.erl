-module('v.log').
-export([tag_to_console/2, tag_to_file/2, level_from_tag/1, target_from_label/1, init/0, free_logger/1, set_logger/1, get_logger/0, get_level/0, set_level/1, fatal/1, error/1, warn/1, info/1, debug/1, set_always_flush/1, 'Log.get_level'/1, 'Log.set_level'/2, 'Log.set_full_logpath'/2, 'Log.set_output_label'/2, 'Log.set_output_path'/2, 'Log.set_output_stream'/2, 'Log.log_to_console_too'/1, 'Log.flush'/1, 'Log.close'/1, 'Log.reopen'/1, 'Log.log_file'/3, 'Log.log_stream'/3, 'Log.send_output'/3, 'Log.fatal'/2, 'Log.error'/2, 'Log.warn'/2, 'Log.info'/2, 'Log.debug'/2, 'Log.free'/1, 'Log.time_format'/2, 'Log.set_time_format'/2, 'Log.set_always_flush'/2, 'Log.get_time_format'/1, 'Log.set_custom_time_format'/2, 'Log.get_custom_time_format'/1, 'Log.set_short_tag'/2, 'Log.get_short_tag'/1, 'Log.set_local_time'/2, 'Log.get_local_time'/1, use_stdout/0, new_thread_safe_log/0, 'ThreadSafeLog.free'/1, 'ThreadSafeLog.set_level'/2, 'ThreadSafeLog.set_always_flush'/2, 'ThreadSafeLog.debug'/2, 'ThreadSafeLog.info'/2, 'ThreadSafeLog.warn'/2, 'ThreadSafeLog.error'/2, 'ThreadSafeLog.fatal'/2, 'Level__static__from'/1, 'LogTarget__static__from'/1, 'TimeFormat__static__from'/1]).

tag_to_console(L, Short_tag) ->
    case Short_tag of
        true -> case L of
            disabled -> <<" ">>;
            fatal -> red(<<"F">>);
            error -> red(<<"E">>);
            warn -> yellow(<<"W">>);
            info -> white(<<"I">>);
            debug -> magenta(<<"D">>)
        end;
        false -> case L of
            disabled -> <<"">>;
            fatal -> red(<<"FATAL">>);
            error -> red(<<"ERROR">>);
            warn -> yellow(<<"WARN ">>);
            info -> white(<<"INFO ">>);
            debug -> magenta(<<"DEBUG">>)
        end
    end.

tag_to_file(L, Short_tag) ->
    case Short_tag of
        true -> case L of
            disabled -> <<" ">>;
            fatal -> <<"F">>;
            error -> <<"E">>;
            warn -> <<"W">>;
            info -> <<"I">>;
            debug -> <<"D">>
        end;
        false -> case L of
            disabled -> <<"     ">>;
            fatal -> <<"FATAL">>;
            error -> <<"ERROR">>;
            warn -> <<"WARN ">>;
            info -> <<"INFO ">>;
            debug -> <<"DEBUG">>
        end
    end.

level_from_tag(Tag) ->
    case Tag of
        <<"DISABLED">>; <<" ">> -> disabled;
        <<"FATAL">>; <<"F">> -> fatal;
        <<"ERROR">>; <<"E">> -> error;
        <<"WARN">>; <<"W">> -> warn;
        <<"INFO">>; <<"I">> -> info;
        <<"DEBUG">>; <<"D">> -> debug;
        _ -> todo
    end.

target_from_label(Label) ->
    case Label of
        <<"console">> -> console;
        <<"file">> -> file;
        <<"both">> -> both;
        _ -> todo
    end.

init() ->
    Default_logger = new_thread_safe_log(),

free_logger(Logger) ->
    case todo == todo of
        true -> ok;
        false -> todo
        end.

set_logger(Logger) ->
    Old_logger = todo,
    Default_logger = todo,
    free_logger(Old_logger),
    ok.

get_logger() ->
    Default_logger.

get_level() ->
    'Logger.get_level'(Default_logger).

set_level(Level) ->
    'Logger.set_level'(Default_logger, Level),
    ok.

fatal(S) ->
    'Logger.fatal'(Default_logger, S),
    exit(1),
    ok.

error(S) ->
    'Logger.error'(Default_logger, S),
    ok.

warn(S) ->
    'Logger.warn'(Default_logger, S),
    ok.

info(S) ->
    'Logger.info'(Default_logger, S),
    ok.

debug(S) ->
    'Logger.debug'(Default_logger, S),
    ok.

set_always_flush(Should_flush_on_every_message) ->
    'Logger.set_always_flush'(Default_logger, Should_flush_on_every_message),
    ok.

'Log.get_level'(L) ->
    maps:get(level, L).

'Log.set_level'(L, Level) ->

'Log.set_full_logpath'(L, Full_log_path) ->
    Rlog_file = real_path(Full_log_path),
    'Log.set_output_label'(L, file_name(Rlog_file)),
    'Log.set_output_path'(L, dir(Rlog_file)),
    ok.

'Log.set_output_label'(L, Label) ->

'Log.set_output_path'(L, Output_file_path) ->
    case maps:get(is_opened, maps:get(ofile, L)) of
        true -> 'File.close'(maps:get(ofile, L));
        false -> ok
    end,
    Ofile = open_append(maps:get(output_file_name, L)),

'Log.set_output_stream'(L, Stream) ->

'Log.log_to_console_too'(L) ->
    case maps:get(output_target, L) /= file of
        true -> erlang:error({panic, <<"log_to_console_too should be called *after* .set_output_path">>});
        false -> ok
    end,

'Log.flush'(L) ->
    'File.flush'(maps:get(ofile, L)),
    ok.

'Log.close'(L) ->
    'File.close'(maps:get(ofile, L)),
    ok.

'Log.reopen'(L) ->
    'Log.flush'(L),
    case maps:get(output_target, L) == file orelse maps:get(output_target, L) == both of
        true -> 'File.reopen'(maps:get(ofile, L), maps:get(output_file_name, L), <<"ab">>);
        false -> ok
    end,
    ok.

'Log.log_file'(L, S, Level) ->
    Timestamp = 'Log.time_format'(L, case maps:get(local_time, L) of
        true -> now();
        false -> utc()
    end),
    E = tag_to_file(Level, maps:get(short_tag, L)),
    % TODO: unhandled stmt type
    case maps:get(always_flush, L) of
        true -> 'Log.flush'(L);
        false -> ok
    end.

'Log.log_stream'(L, S, Level) ->
    Timestamp = 'Log.time_format'(L, case maps:get(local_time, L) of
        true -> now();
        false -> utc()
    end),
    Tag = tag_to_console(Level, maps:get(short_tag, L)),
    Msg = <<(Timestamp)/binary, " [", (Tag)/binary, "] ", (S)/binary, "\\n">>,
    Arr = binary_to_list(Msg),
    'Writer.write'(maps:get(output_stream, L), Arr),
    case maps:get(always_flush, L) of
        true -> case maps:get(output_stream, L) is todo of
            true -> case maps:get(fd, maps:get(output_stream, L)) of
                1 -> flush_stdout();
                2 -> flush_stderr();
                _ -> ok
            end;
            false -> ok
        end;
        false -> ok
    end.

'Log.send_output'(L, S, Level) ->
    case maps:get(output_target, L) == file orelse maps:get(output_target, L) == both of
        true -> 'Log.log_file'(L, S, Level);
        false -> ok
    end,
    case maps:get(output_target, L) == console orelse maps:get(output_target, L) == both of
        true -> 'Log.log_stream'(L, S, Level);
        false -> ok
    end.

'Log.fatal'(L, S) ->
    case todo >= todo of
        true -> begin
            'Log.send_output'(L, S, fatal),
            'File.close'(maps:get(ofile, L))
        end;
        false -> ok
    end,
    erlang:error({panic, <<(<<(maps:get(output_label, L))/binary, (<<": ">>)/binary>>)/binary, (S)/binary>>}),
    ok.

'Log.error'(L, S) ->
    case todo < todo of
        true -> ok;
        false -> 'Log.send_output'(L, S, error)
        end.

'Log.warn'(L, S) ->
    case todo < todo of
        true -> ok;
        false -> 'Log.send_output'(L, S, warn)
        end.

'Log.info'(L, S) ->
    case todo < todo of
        true -> ok;
        false -> 'Log.send_output'(L, S, info)
        end.

'Log.debug'(L, S) ->
    case todo < todo of
        true -> ok;
        false -> 'Log.send_output'(L, S, debug)
        end.

'Log.free'(F) ->
    % TODO: unhandled stmt type
        ok.

'Log.time_format'(L, T) ->
    case maps:get(time_format, L) of
        tf_rfc3339_micro -> 'Time.format_rfc3339_micro'(T);
        tf_ss_micro -> 'Time.format_ss_micro'(T);
        tf_default -> 'Time.format'(T);
        tf_ss -> 'Time.format_ss'(T);
        tf_ss_milli -> 'Time.format_ss_milli'(T);
        tf_ss_nano -> 'Time.format_ss_nano'(T);
        tf_rfc3339 -> 'Time.format_rfc3339'(T);
        tf_rfc3339_nano -> 'Time.format_rfc3339_nano'(T);
        tf_hhmm -> 'Time.hhmm'(T);
        tf_hhmmss -> 'Time.hhmmss'(T);
        tf_hhmm12 -> 'Time.hhmm12'(T);
        tf_ymmdd -> 'Time.ymmdd'(T);
        tf_ddmmy -> 'Time.ddmmy'(T);
        tf_md -> 'Time.md'(T);
        tf_custom_format -> 'Time.custom_format'(T, maps:get(custom_time_format, L))
    end.

'Log.set_time_format'(L, F) ->

'Log.set_always_flush'(L, Should_flush_every_time) ->

'Log.get_time_format'(L) ->
    maps:get(time_format, L).

'Log.set_custom_time_format'(L, F) ->

'Log.get_custom_time_format'(L) ->
    maps:get(custom_time_format, L).

'Log.set_short_tag'(L, Enabled) ->

'Log.get_short_tag'(L) ->
    maps:get(short_tag, L).

'Log.set_local_time'(L, Enabled) ->

'Log.get_local_time'(L) ->
    maps:get(local_time, L).

use_stdout() ->
    L = #{{vbeam, type} => 'ThreadSafeLog'},
    'ThreadSafeLog.set_output_stream'(L, stdout()),
    set_logger(L),
    ok.

new_thread_safe_log() ->
    Slevel = todo,
    Level = level_from_tag(string:uppercase(Slevel)),
    X = #{level => Level, {vbeam, type} => 'ThreadSafeLog'},
    X.

'ThreadSafeLog.free'(X) ->
    % TODO: unhandled stmt type
        ok.

'ThreadSafeLog.set_level'(X, Level) ->
    case todo of
        true -> ok;
        false -> begin
            'Mutex.lock'(maps:get(mu, X)),
            'Log.set_level'(maps:get(Log, X), Level),
            'Mutex.unlock'(maps:get(mu, X)),
            ok
        end
        end.

'ThreadSafeLog.set_always_flush'(X, Should_flush) ->
    case todo of
        true -> ok;
        false -> begin
            'Mutex.lock'(maps:get(mu, X)),
            'Log.set_always_flush'(maps:get(Log, X), Should_flush),
            'Mutex.unlock'(maps:get(mu, X)),
            ok
        end
        end.

'ThreadSafeLog.debug'(X, S) ->
    case todo of
        true -> ok;
        false -> begin
            'Mutex.lock'(maps:get(mu, X)),
            'Log.debug'(maps:get(Log, X), S),
            'Mutex.unlock'(maps:get(mu, X)),
            ok
        end
        end.

'ThreadSafeLog.info'(X, S) ->
    case todo of
        true -> ok;
        false -> begin
            'Mutex.lock'(maps:get(mu, X)),
            'Log.info'(maps:get(Log, X), S),
            'Mutex.unlock'(maps:get(mu, X)),
            ok
        end
        end.

'ThreadSafeLog.warn'(X, S) ->
    case todo of
        true -> ok;
        false -> begin
            'Mutex.lock'(maps:get(mu, X)),
            'Log.warn'(maps:get(Log, X), S),
            'Mutex.unlock'(maps:get(mu, X)),
            ok
        end
        end.

'ThreadSafeLog.error'(X, S) ->
    case todo of
        true -> ok;
        false -> begin
            'Mutex.lock'(maps:get(mu, X)),
            'Log.error'(maps:get(Log, X), S),
            'Mutex.unlock'(maps:get(mu, X)),
            ok
        end
        end.

'ThreadSafeLog.fatal'(X, S) ->
    case todo of
        true -> erlang:error({panic, S});
        false -> ok
    end,
    'Mutex.lock'(maps:get(mu, X)),
    % TODO: unhandled stmt type
    'Log.fatal'(maps:get(Log, X), S),
    ok.

'Level__static__from'(Input) ->
    error(<<"invalid value">>).

'LogTarget__static__from'(Input) ->
    error(<<"invalid value">>).

'TimeFormat__static__from'(Input) ->
    error(<<"invalid value">>).
