-module('v.os').
-export([args_after/1, args_before/1, start_new_command/1, 'Command.start'/1, 'Command.read_line'/1, 'Command.close'/1, fd_close/1, fd_write/2, fd_slurp/1, fd_read/2, fd_is_pending/1, is_abs_path/1, abs_path/1, norm_path/1, existing_path/1, clean_path/1, to_slash/1, from_slash/1, win_volume_len/1, is_slash/1, is_unc_path/1, has_drive_letter/1, starts_w_slash_slash/1, is_drive_rooted/1, is_normal_path/1, is_curr_dir_ref/3, windows_volume/1, stdin/0, stdout/0, stderr/0, 'File.close'/1, 'File.write'/2, 'File.writeln'/2, 'File.write_string'/2, 'File.write_ptr'/3, 'File.write_full_buffer'/3, 'File.flush'/1, 'File.seek'/3, 'File.tell'/1, 'File.read'/2, 'File.read_bytes_with_newline'/2, 'File.read_into_ptr'/3, 'File.eof'/1, error_file_not_opened/0, getwd/0, chdir/1, exists/1, is_dir/1, is_link/1, is_executable/1, is_writable/1, is_readable/1, mkdir/2, rmdir/1, ls/1, read_file/1, read_bytes/1, write_bytes/2, rm/1, cp/3, rename/2, rename_dir/2, open/1, create/1, vfopen/2, stat/1, lstat/1, file_size/1, file_last_mod_unix/1, truncate/2, getenv/1, getenv_opt/1, setenv/3, unsetenv/1, environ/0, real_path/1, symlink/2, readlink/1, link/2, executable/0, getpid/0, getppid/0, getuid/0, geteuid/0, getgid/0, getegid/0, system/1, execute/1, fork/0, wait/0, hostname/0, loginname/0, uname/0, flush/0, get_raw_line/0, get_raw_stdin/0, fileno/1, is_atty/1, chmod/2, chown/3, glob/1, posix_get_error_msg/1, get_error_msg/1, last_error/0, error_posix/1, page_size/0, disk_usage/1, native_glob_pattern/2, kind_of_existing_path/1, input_password/1, execve/3, open_file/3, open_append/1, 'File.reopen'/3, signal_opt/2, signal_ignore/1, is_main_thread/0, signal_ignore_internal/1, 'Result.free'/1, executable_fallback/0, cp_all/3, mv_by_cp/3, mv/3, read_lines/1, write_lines/2, sigint_to_signal_name/1, rmdir_all/1, is_dir_empty/1, file_ext/1, dir/1, base/1, file_name/1, split_path/1, input_opt/1, input/1, get_line/0, get_lines/0, get_lines_joined/0, get_raw_lines/0, get_raw_lines_joined/0, get_trimmed_lines/0, user_os/0, user_names/0, home_dir/0, expand_tilde_to_home/1, write_file/2, 'ExecutableNotFoundError.msg'/1, error_failed_to_find_executable/0, find_abs_path_of_executable/1, exists_in_system_path/1, is_file/1, join_path/2, join_path_single/2, normalize_path_in_builder/1, walk_ext/3, impl_walk_ext/4, walk/2, walk_with_context/3, log/1, mkdir_all/2, create_folder_when_it_does_not_exist/1, xdg_home_folder/2, cache_dir/0, data_dir/0, state_dir/0, local_bin_dir/0, temp_dir/0, vtmp_dir/0, default_vmodules_path/0, vmodules_dir/0, vmodules_paths/0, resource_abs_path/1, execute_or_panic/1, execute_or_exit/1, execute_opt/1, quoted_path/1, config_dir/0, 'Process.signal_kill'/1, 'Process.signal_term'/1, 'Process.signal_pgkill'/1, 'Process.signal_stop'/1, 'Process.signal_continue'/1, 'Process.wait'/1, 'Process.close'/1, 'Process.free'/1, 'Process._spawn'/1, 'Process.is_alive'/1, 'Process.set_redirect_stdio'/1, 'Process.stdin_write'/2, 'Process.stdout_slurp'/1, 'Process.stderr_slurp'/1, 'Process.stdout_read'/1, 'Process.stderr_read'/1, 'Process.pipe_read'/2, 'Process.is_pending'/2, 'Process.run'/1, new_process/1, 'Process.set_args'/2, 'Process.set_work_folder'/2, 'Process.set_environment'/2, sleep_ms/1, 'SeekMode__static__from'/1, 'ChildProcessPipeKind__static__from'/1, 'ProcessState__static__from'/1, 'Signal__static__from'/1]).

args_after(Cut_word) ->
    case length(init:get_plain_arguments()) == 0 of
        true -> [];
        false -> begin
            Cargs = [],
            case (not lists:member(Cut_word, init:get_plain_arguments())) of
                true -> ok;
                false -> begin
                    Found = false,
                    Cargs bsl lists:nth(1, init:get_plain_arguments()),
                    lists:foreach(fun(A) ->
                        case A == Cut_word of
                            true -> begin
                                Found1 = true,
                                % TODO: unhandled stmt type
                            end;
                            false -> ok
                        end,
                        case not Found1 of
                            true -> ok;
                            false -> ok
                        end,
                        Cargs bsl A,
                        ok
                    end, lists:nth(todo + 1, init:get_plain_arguments())),
                end
            end,
            Cargs
        end
        end.

args_before(Cut_word) ->
    case length(init:get_plain_arguments()) == 0 of
        true -> [];
        false -> begin
            Cargs = [],
            case (not lists:member(Cut_word, init:get_plain_arguments())) of
                true -> ok;
                false -> begin
                    Cargs bsl lists:nth(1, init:get_plain_arguments()),
                    lists:foreach(fun(A) ->
                        case A == Cut_word of
                            true -> ok;
                            false -> ok
                        end,
                        Cargs bsl A,
                        ok
                    end, lists:nth(todo + 1, init:get_plain_arguments())),
                end
            end,
            Cargs
        end
        end.

start_new_command(Cmd) ->
    Res = #{path => Cmd, {vbeam, type} => 'Command'},
    'Command.start'(Res),
    Res.

'Command.start'(C) ->
    ok.

'Command.read_line'(C) ->
    <<"">>.

'Command.close'(C) ->
    ok.

fd_close(Fd) ->
    case Fd == -1 of
        true -> 0;
        false -> 0
        end.

fd_write(Fd, S) ->
    case Fd == -1 of
        true -> ok;
        false -> ok
        end.

fd_slurp(Fd) ->
    Res = [],
    case Fd == -1 of
        true -> Res;
        false -> Res
        end.

fd_read(Fd, Maxbytes) ->
    case Fd == -1 of
        true -> <<"">>;
        false -> <<"">>
        end.

fd_is_pending(Fd) ->
    false.

is_abs_path(Path) ->
    case Path == <<"">> of
        true -> false;
        false -> begin
            % TODO: unhandled stmt type
            lists:nth(1, Path) == todo
        end
        end.

abs_path(Path) ->
    Wd = getwd(),
    case Path == <<"">> of
        true -> Wd;
        false -> begin
            Npath = norm_path(Path),
            case Npath == <<".">> of
                true -> Wd;
                false -> 
                    case not is_abs_path(Npath) of
                        true -> norm_path('Builder.str'(Sb));
                        false -> Npath
                                        end
                                        end
        end
        end.

norm_path(Path) ->
    case Path == <<"">> of
        true -> <<".">>;
        false -> begin
            Rooted = is_abs_path(Path),
            Volume_len = win_volume_len(Path),
            Volume = lists:nth(todo + 1, Path),
            case Volume_len /= 0 andalso case binary:match(Volume, <<"/">>) of nomatch -> false; _ -> true end of
                true -> ok;
                false -> ok
            end,
            Cpath = clean_path(lists:nth(todo + 1, Path)),
            case Cpath == <<"">> andalso Volume_len == 0 of
                true -> <<".">>;
                false -> begin
                    Spath = binary:split(Cpath, <<"/">>, [global]),
                    case (not lists:member(<<"..">>, Spath)) of
                        true -> case Volume_len /= 0 of
                            true -> <<(Volume)/binary, (Cpath)/binary>>;
                            false -> Cpath
                        end;
                        false -> begin
                            Spath_len = length(Spath),
                            Sb = new_builder(length(Cpath)),
                            case Rooted of
                                true -> 'Builder.write_string'(Sb, <<"/">>);
                                false -> ok
                            end,
                            New_path = [],
                            Backlink_count = 0,
                            % TODO: unhandled stmt type
                            case Backlink_count /= 0 andalso not Rooted of
                                true -> ok;
                                false -> ok
                            end,
                            'Builder.write_string'(Sb, iolist_to_binary(lists:join(<<"/">>, New_path))),
                            Res = 'Builder.str'(Sb),
                            case length(Res) == 0 of
                                true -> <<"/">>;
                                false -> 
                                    case Volume_len /= 0 of
                                        true -> <<(Volume)/binary, (Res)/binary>>;
                                        false -> Res
                                                                        end
                                                                                        end
                        end
                                        end
                end
                        end
        end
        end.

existing_path(Path) ->
    Err = error(<<"path does not exist">>),
    case Path == <<"">> of
        true -> Err;
        false -> 
            case exists(Path) of
                true -> Path;
                false -> begin
                    Volume_len = 0,
                    % TODO: unhandled stmt type
                    case Volume_len > 0 andalso is_slash(lists:nth(Volume_len - 1 + 1, Path)) of
                        true -> todo;
                        false -> ok
                    end,
                    Sc = new(lists:nth(todo + 1, Path)),
                    Recent_path = lists:nth(todo + 1, Path),
                    % TODO: unhandled stmt type
                    Err
                end
                        end
                end.

clean_path(Path) ->
    case Path == <<"">> of
        true -> <<"">>;
        false -> begin
            Sb = new_builder(length(Path)),
            Sc = new(Path),
            % TODO: unhandled stmt type
            Res = 'Builder.str'(Sb),
            case length(Res) > 1 andalso is_slash(lists:nth(length(Res) - 1 + 1, Res)) of
                true -> lists:nth(todo + 1, Res);
                false -> Res
                        end
        end
        end.

to_slash(Path) ->
    .

from_slash(Path) ->
    .

win_volume_len(Path) ->
    case not Windows of
        true -> 0;
        false -> begin
            Plen = length(Path),
            case Plen < 2 of
                true -> 0;
                false -> 
                    case has_drive_letter(Path) of
                        true -> 2;
                        false -> begin
                            case Plen >= 5 andalso starts_w_slash_slash(Path) andalso not is_slash(lists:nth(3, Path)) of
                                true -> ok;
                                false -> ok
                            end,
                            0
                        end
                                        end
                                        end
        end
        end.

is_slash(B) ->
    % TODO: unhandled stmt type
    B == todo.

is_unc_path(Path) ->
    win_volume_len(Path) >= 5 andalso starts_w_slash_slash(Path).

has_drive_letter(Path) ->
    length(Path) >= 2 andalso 'u8.is_letter'(lists:nth(1, Path)) andalso lists:nth(2, Path) == todo.

starts_w_slash_slash(Path) ->
    length(Path) >= 2 andalso is_slash(lists:nth(1, Path)) andalso is_slash(lists:nth(2, Path)).

is_drive_rooted(Path) ->
    length(Path) >= 3 andalso has_drive_letter(Path) andalso is_slash(lists:nth(3, Path)).

is_normal_path(Path) ->
    Plen = length(Path),
    case Plen == 0 of
        true -> false;
        false -> (Plen == 1 andalso is_slash(lists:nth(1, Path))) orelse (Plen >= 2 andalso is_slash(lists:nth(1, Path)) andalso not is_slash(lists:nth(2, Path)))
        end.

is_curr_dir_ref(Byte_one, Byte_two, Byte_three) ->
    case todo /= todo of
        true -> false;
        false -> (Byte_one < 0 orelse is_slash(todo)) andalso (Byte_three < 0 orelse is_slash(todo))
        end.

windows_volume(Path) ->
    Volume_len = win_volume_len(Path),
    case Volume_len == 0 of
        true -> <<"">>;
        false -> lists:nth(todo + 1, Path)
        end.

stdin() ->
    #{fd => 0, cfile => todo, is_opened => true, {vbeam, type} => 'File'}.

stdout() ->
    #{fd => 1, cfile => todo, is_opened => true, {vbeam, type} => 'File'}.

stderr() ->
    #{fd => 2, cfile => todo, is_opened => true, {vbeam, type} => 'File'}.

'File.close'(F) ->
    case not maps:get(is_opened, F) of
        true -> ok;
        false -> 
                end.

'File.write'(F, Buf) ->
    case not maps:get(is_opened, F) of
        true -> error_file_not_opened();
        false -> length(Buf)
        end.

'File.writeln'(F, S) ->
    case not maps:get(is_opened, F) of
        true -> error_file_not_opened();
        false -> length(S) + 1
        end.

'File.write_string'(F, S) ->
    case not maps:get(is_opened, F) of
        true -> error_file_not_opened();
        false -> length(S)
        end.

'File.write_ptr'(F, Data, Size) ->
    Size.

'File.write_full_buffer'(F, Buffer, Buffer_len) ->
    ok.

'File.flush'(F) ->
    ok.

'File.seek'(F, Pos, Mode) ->
    case not maps:get(is_opened, F) of
        true -> error_file_not_opened();
        false -> ok
        end.

'File.tell'(F) ->
    case not maps:get(is_opened, F) of
        true -> error_file_not_opened();
        false -> 0
        end.

'File.read'(F, Buf) ->
    case not maps:get(is_opened, F) of
        true -> error_file_not_opened();
        false -> 0
        end.

'File.read_bytes_with_newline'(F, Buf) ->
    case not maps:get(is_opened, F) of
        true -> error_file_not_opened();
        false -> 
            case length(Buf) == 0 of
                true -> error(<<(todo)/binary, (<<": `buf.len` == 0">>)/binary>>);
                false -> 0
                        end
                end.

'File.read_into_ptr'(F, P, Max) ->
    case not maps:get(is_opened, F) of
        true -> error_file_not_opened();
        false -> 0
        end.

'File.eof'(F) ->
    true.

error_file_not_opened() ->
    error(<<"file not opened">>).

getwd() ->
    <<"/tmp">>.

chdir(Path) ->
    case Path == <<"">> of
        true -> error(<<"empty path">>);
        false -> ok
        end.

exists(Path) ->
    false.

is_dir(Path) ->
    false.

is_link(Path) ->
    false.

is_executable(Path) ->
    false.

is_writable(Path) ->
    false.

is_readable(Path) ->
    false.

mkdir(Path, Params) ->
    case Path == <<".">> of
        true -> ok;
        false -> ok
        end.

rmdir(Path) ->
    ok.

ls(Path) ->
    case Path == <<"">> of
        true -> error(<<"ls() expects a folder, not an empty string">>);
        false -> []
        end.

read_file(Path) ->
    error(<<"read_file not implemented for BEAM">>).

read_bytes(Path) ->
    Content = read_file(Path),
    binary_to_list(Content).

write_bytes(Path, Bytes) ->
    ok.

rm(Path) ->
    ok.

cp(Src, Dst, Config) ->
    ok.

rename(Src, Dst) ->
    ok.

rename_dir(Src, Dst) ->
    rename(Src, Dst),
    ok.

open(Path) ->
    error(<<"open not implemented for BEAM">>).

create(Path) ->
    error(<<"create not implemented for BEAM">>).

vfopen(Path, Mode) ->
    error(<<"vfopen not implemented for BEAM">>).

stat(Path) ->
    error(<<"stat not implemented for BEAM">>).

lstat(Path) ->
    error(<<"lstat not implemented for BEAM">>).

file_size(Path) ->
    0.

file_last_mod_unix(Path) ->
    0.

truncate(Path, Len) ->
    ok.

getenv(Key) ->
    getenv_opt(Key).

getenv_opt(Key) ->
    todo.

setenv(Name, Value, Overwrite) ->
    0.

unsetenv(Name) ->
    0.

environ() ->
    #{}.

real_path(Fpath) ->
    Fpath.

symlink(Target, Link_name) ->
    ok.

readlink(Path) ->
    error(<<"readlink not implemented for BEAM">>).

link(Origin, Target) ->
    ok.

executable() ->
    <<"">>.

getpid() ->
    0.

getppid() ->
    0.

getuid() ->
    0.

geteuid() ->
    0.

getgid() ->
    0.

getegid() ->
    0.

system(Cmd) ->
    0.

execute(Cmd) ->
    #{exit_code => 0, output => <<"">>, {vbeam, type} => 'Result'}.

fork() ->
    -1.

wait() ->
    -1.

hostname() ->
    <<"">>.

loginname() ->
    getenv(<<"USER">>).

uname() ->
    #{sysname => <<"BEAM">>, nodename => <<"">>, release => <<"">>, version => <<"">>, machine => <<"">>, {vbeam, type} => 'Uname'}.

flush() ->
    ok.

get_raw_line() ->
    <<"">>.

get_raw_stdin() ->
    [].

fileno(Cfile) ->
    -1.

is_atty(Fd) ->
    0.

chmod(Path, Mode) ->
    ok.

chown(Path, Owner, Group) ->
    ok.

glob(Patterns) ->
    [].

posix_get_error_msg(Code) ->
    <<"error code: ", (integer_to_binary(Code))/binary>>.

get_error_msg(Code) ->
    posix_get_error_msg(Code).

last_error() ->
    error(<<"unknown error">>).

error_posix(E) ->
    Message = case maps:get(msg, E) == <<"">> of
        true -> <<"posix error">>;
        false -> maps:get(msg, E)
    end,
    error_with_code(Message, maps:get(code, E)).

page_size() ->
    4096.

disk_usage(Path) ->
    #{total => 0, available => 0, used => 0, {vbeam, type} => 'DiskUsage'}.

native_glob_pattern(Pattern, Matches) ->
    ok.

kind_of_existing_path(Path) ->
    #{{vbeam, type} => 'PathKind'}.

input_password(Prompt) ->
    error(<<"input_password not implemented for BEAM">>).

execve(Cmdpath, Cmdargs, Envs) ->
    error(<<"execve not implemented for BEAM">>).

open_file(Path, Mode, Options) ->
    error(<<"open_file not implemented for BEAM">>).

open_append(Path) ->
    #{fd => -1, cfile => todo, is_opened => true, {vbeam, type} => 'File'}.

'File.reopen'(F, Path, Mode) ->
    'File.close'(F),
    ok.

signal_opt(Signum, Handler) ->
    Handler.

signal_ignore(Args) ->
    ok.

is_main_thread() ->
    true.

signal_ignore_internal(Args) ->
    ok.

'Result.free'(Result) ->
    todo,
    ok.

executable_fallback() ->
    case length(init:get_plain_arguments()) == 0 of
        true -> <<"">>;
        false -> begin
            Exepath = lists:nth(1, init:get_plain_arguments()),
            % TODO: unhandled stmt type
            case not is_abs_path(Exepath) of
                true -> begin
                    Other_separator = ,
                    Rexepath = binary:replace(Exepath, Other_separator, <<"/">>, [global]),
                    case case binary:match(Rexepath, <<"/">>) of nomatch -> false; _ -> true end of
                        true -> ok;
                        false -> begin
                            Foundpath = find_abs_path_of_executable(Exepath),
                            case Foundpath /= <<"">> of
                                true -> ok;
                                false -> ok
                            end
                        end
                    end
                end;
                false -> ok
            end,
            Exepath1 = real_path(Exepath),
            Exepath1
        end
        end.

cp_all(Src, Dst, Overwrite) ->
    Source_path = real_path(Src),
    Dest_path = real_path(Dst),
    case not exists(Source_path) of
        true -> error(<<"Source path doesn't exist">>);
        false -> 
            case not is_dir(Source_path) of
                true -> ok;
                false -> begin
                    case not exists(Dest_path) of
                        true -> mkdir(Dest_path, #{{vbeam, type} => 'MkdirParams'});
                        false -> ok
                    end,
                    case not is_dir(Dest_path) of
                        true -> error(<<"Destination path is not a valid directory">>);
                        false -> begin
                            Files = ls(Source_path),
                            lists:foreach(fun(File) ->
                                Sp = join_path_single(Source_path, File),
                                Dp = join_path_single(Dest_path, File),
                                case is_dir(Sp) of
                                    true -> case not exists(Dp) of
                                        true -> mkdir(Dp, #{{vbeam, type} => 'MkdirParams'});
                                        false -> ok
                                    end;
                                    false -> ok
                                end,
                                cp_all(Sp, Dp, Overwrite),
                                ok
                            end, Files),
                            ok
                        end
                                        end
                end
                        end
                end.

mv_by_cp(Source, Target, Opts) ->
    cp_all(Source, Target, maps:get(overwrite, Opts)),
    case is_dir(Source) of
        true -> ok;
        false -> begin
            rm(Source),
            ok
        end
        end.

mv(Source, Target, Opts) ->
    case not maps:get(overwrite, Opts) andalso exists(Target) of
        true -> error(<<"target path already exist">>);
        false -> begin
            rename(Source, Target),
            ok
        end
        end.

read_lines(Path) ->
    Buf = read_file(Path),
    Res = binary:split(Buf, <<"\n">>, [global]),
    todo,
    Res.

write_lines(Path, Lines) ->
    F = create(Path),
    % TODO: unhandled stmt type
    lists:foreach(fun(Line) ->
        'File.writeln'(F, Line),
        ok
    end, Lines),
    ok.

sigint_to_signal_name(Si) ->
    case Si of
        1 -> <<"SIGHUP">>;
        2 -> <<"SIGINT">>;
        3 -> <<"SIGQUIT">>;
        4 -> <<"SIGILL">>;
        6 -> <<"SIGABRT">>;
        8 -> <<"SIGFPE">>;
        9 -> <<"SIGKILL">>;
        11 -> <<"SIGSEGV">>;
        13 -> <<"SIGPIPE">>;
        14 -> <<"SIGALRM">>;
        15 -> <<"SIGTERM">>;
        _ -> ok
    end,
    % TODO: unhandled stmt type
    <<"unknown">>.

rmdir_all(Path) ->
    Err_msg = <<"">>,
    Err_code = -1,
    Items = ls(Path),
    lists:foreach(fun(Item) ->
        Fullpath = join_path_single(Path, Item),
        case is_dir(Fullpath) andalso not is_link(Fullpath) of
            true -> rmdir_all(Fullpath);
            false -> rm(Fullpath)
        end,
        ok
    end, Items),
    rmdir(Path),
    case Err_msg /= <<"">> of
        true -> error_with_code(Err_msg, Err_code);
        false -> ok
        end.

is_dir_empty(Path) ->
    Items = ls(Path),
    Res = length(Items) == 0,
    todo,
    Res.

file_ext(Opath) ->
    case length(Opath) < 3 of
        true -> <<"">>;
        false -> begin
            Path = file_name(Opath),
            Pos = 'string.last_index_u8'(Path, todo),
            case Pos == -1 of
                true -> <<"">>;
                false -> 
                    case Pos + 1 >= length(Path) orelse Pos == 0 of
                        true -> <<"">>;
                        false -> lists:nth(todo + 1, Path)
                                        end
                                        end
        end
        end.

dir(Path) ->
    case Path == <<"">> of
        true -> <<".">>;
        false -> begin
            Detected_path_separator = case case binary:match(Path, <<"/">>) of nomatch -> false; _ -> true end of
                true -> <<"/">>;
                false -> <<"\\\\">>
            end,
            Pos = 'string.last_index'(Path, Detected_path_separator),
            case Pos == 0 of
                true -> Detected_path_separator;
                false -> lists:nth(todo + 1, Path)
                        end
        end
        end.

base(Path) ->
    case Path == <<"">> of
        true -> <<".">>;
        false -> begin
            Detected_path_separator = case case binary:match(Path, <<"/">>) of nomatch -> false; _ -> true end of
                true -> <<"/">>;
                false -> <<"\\\\">>
            end,
            case Path == Detected_path_separator of
                true -> Detected_path_separator;
                false -> 
                    case case binary:longest_common_suffix([Path, Detected_path_separator]) of 0 -> false; _ -> true end of
                        true -> lists:nth(todo + 1, Path2);
                        false -> begin
                            Pos = 'string.last_index'(Path, Detected_path_separator),
                            lists:nth(todo + 1, Path)
                        end
                                        end
                                        end
        end
        end.

file_name(Path) ->
    Detected_path_separator = case case binary:match(Path, <<"/">>) of nomatch -> false; _ -> true end of
        true -> <<"/">>;
        false -> <<"\\\\">>
    end,
    'string.all_after_last'(Path, Detected_path_separator).

split_path(Path) ->
    case Path == <<"">> of
        true -> <<".">>;
        false -> case Path == <<".">> of
            true -> <<".">>;
            false -> case Path == <<"..">> of
                true -> <<"..">>;
                false -> ok
            end
        end
    end,
    Detected_path_separator = case case binary:match(Path, <<"/">>) of nomatch -> false; _ -> true end of
        true -> <<"/">>;
        false -> <<"\\\\">>
    end,
    case Path == Detected_path_separator of
        true -> Detected_path_separator;
        false -> 
            case case binary:longest_common_suffix([Path, Detected_path_separator]) of 0 -> false; _ -> true end of
                true -> lists:nth(todo + 1, Path);
                false -> begin
                    Dir = <<".">>,
                    Pos = 'string.last_index'(Path, Detected_path_separator),
                    case Pos == -1 of
                        true -> ok;
                        false -> case Pos == 0 of
                            true -> ok;
                            false -> ok
                        end
                    end,
                    File_name = 'string.all_after_last'(Path, Detected_path_separator),
                    Pos_ext = 'string.last_index_u8'(File_name, todo),
                    case Pos_ext == -1 orelse Pos_ext == 0 orelse Pos_ext + 1 >= length(File_name) of
                        true -> Dir;
                        false -> Dir
                                        end
                end
                        end
                end.

input_opt(Prompt) ->
    io:format("~s", [Prompt]),
    flush(),
    Res = get_raw_line(),
    case length(Res) > 0 of
        true -> 'string.trim_right'(Res, <<"\\r\\n">>);
        false -> todo
        end.

input(Prompt) ->
    Res = input_opt(Prompt),
    Res.

get_line() ->
    Str = get_raw_line(),
    % TODO: unhandled stmt type
    'string.trim_right'(Str, <<"\\n">>).

get_lines() ->
    Line = <<"">>,
    Inputstr = [],
    % TODO: unhandled stmt type
    Inputstr.

get_lines_joined() ->
    iolist_to_binary(lists:join(<<"">>, get_lines())).

get_raw_lines() ->
    Line = <<"">>,
    Lines = [],
    % TODO: unhandled stmt type
    Lines.

get_raw_lines_joined() ->
    iolist_to_binary(lists:join(<<"">>, get_raw_lines())).

get_trimmed_lines() ->
    Lines = [],
    % TODO: unhandled stmt type
    Lines.

user_os() ->
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    case getenv(<<"TERMUX_VERSION">>) /= <<"">> of
        true -> <<"termux">>;
        false -> <<"unknown">>
        end.

user_names() ->

home_dir() ->

expand_tilde_to_home(Path) ->
    case Path == <<"~">> of
        true -> 'string.trim_right'(Hdir, <<"/">>);
        false -> begin
            Source = <<(<<"~">>)/binary, (<<"/">>)/binary>>,
            case case string:prefix(Path, Source) of nomatch -> false; _ -> true end of
                true -> Result;
                false -> Path
                        end
        end
        end.

write_file(Path, Text) ->
    F = create(Path),
    todo,
    'File.close'(F),
    ok.

'ExecutableNotFoundError.msg'(Err) ->
    <<"os: failed to find executable">>.

error_failed_to_find_executable() ->
    #{{vbeam, type} => 'ExecutableNotFoundError'}.

find_abs_path_of_executable(Exe_name) ->
    case Exe_name == <<"">> of
        true -> error(<<"expected non empty `exe_name`">>);
        false -> begin
            lists:foreach(fun(Suffix) ->
                Fexepath = <<(Exe_name)/binary, (Suffix)/binary>>,
                case is_abs_path(Fexepath) of
                    true -> Fexepath;
                    false -> ok
                end,
                Res = <<"">>,
                Path = getenv(<<"PATH">>),
                Paths = binary:split(Path, <<":">>, [global]),
                lists:foreach(fun(P) ->
                    Found_abs_path = join_path_single(P, Fexepath),
                    % TODO: unhandled stmt type
                    case is_file(Found_abs_path) andalso is_executable(Found_abs_path) of
                        true -> begin
                            Res1 = Found_abs_path,
                            % TODO: unhandled stmt type
                        end;
                        false -> ok
                    end,
                    ok
                end, Paths),
                case length(Res1) > 0 of
                    true -> abs_path(Res1);
                    false -> ok
                end,
                ok
            end, [<<"">>]),
            error_failed_to_find_executable()
        end
        end.

exists_in_system_path(Prog) ->
    find_abs_path_of_executable(Prog),
    true.

is_file(Path) ->
    exists(Path) andalso not is_dir(Path).

join_path(Base, Dirs) ->
    Sb = new_builder(length(Base) + length(Dirs) * 50),
    % TODO: unhandled stmt type
    Sbase = 'string.trim_right'(Base, <<"\\\\/">>),
    % TODO: unhandled stmt type
    'Builder.write_string'(Sb, Sbase),
    lists:foreach(fun(D) ->
        case D /= <<"">> of
            true -> begin
                'Builder.write_string'(Sb, <<"/">>),
                'Builder.write_string'(Sb, D)
            end;
            false -> ok
        end,
        ok
    end, Dirs),
    normalize_path_in_builder(Sb),
    Res = 'Builder.str'(Sb),
    case Base == <<"">> of
        true -> ok;
        false -> ok
    end,
    Res.

join_path_single(Base, Elem) ->
    Sb = new_builder(length(Base) + length(Elem) + 1),
    % TODO: unhandled stmt type
    Sbase = 'string.trim_right'(Base, <<"\\\\/">>),
    % TODO: unhandled stmt type
    'Builder.write_string'(Sb, Sbase),
    case Elem /= <<"">> of
        true -> begin
            'Builder.write_string'(Sb, <<"/">>),
            'Builder.write_string'(Sb, Elem)
        end;
        false -> ok
    end,
    normalize_path_in_builder(Sb),
    Res = 'Builder.str'(Sb),
    case Base == <<"">> of
        true -> ok;
        false -> ok
    end,
    Res.

normalize_path_in_builder(Sb) ->
    Fs = todo,
    Rs = todo,
    % TODO: unhandled stmt type
    lists:foreach(fun(Idx) ->
        % TODO: unhandled stmt type
        ok
    end, lists:seq(0, length(Sb) - 1)),
    lists:foreach(fun(Idx) ->
        case lists:nth(Idx + 1, Sb) == Rs andalso lists:nth(Idx + 1 + 1, Sb) == todo andalso lists:nth(Idx + 2 + 1, Sb) == Rs of
            true -> ok;
            false -> ok
        end.
        case lists:nth(Idx + 1, Sb) == Rs andalso lists:nth(Idx + 1 + 1, Sb) == Rs of
            true -> ok;
            false -> ok
        end.
        ok
    end, lists:seq(0, length(Sb) - 3 - 1)),
        ok.

walk_ext(Path, Ext, Opts) ->
    Res = [],
    impl_walk_ext(Path, Ext, Res, Opts),
    Res.

impl_walk_ext(Path, Ext, Out, Opts) ->
    case not is_dir(Path) of
        true -> ok;
        false -> begin
            Files = ls(Path),
            Separator = case case binary:longest_common_suffix([Path, <<"/">>]) of 0 -> false; _ -> true end of
                true -> <<"">>;
                false -> <<"/">>
            end,
            lists:foreach(fun(File) ->
                case not maps:get(hidden, Opts) andalso case string:prefix(File, <<".">>) of nomatch -> false; _ -> true end of
                    true -> ok;
                    false -> ok
                end
                P = <<(<<(Path)/binary, (Separator)/binary>>)/binary, (File)/binary>>,
                case is_dir(P) andalso not is_link(P) of
                    true -> impl_walk_ext(P, Ext, Out, Opts);
                    false -> case case binary:longest_common_suffix([File, Ext]) of 0 -> false; _ -> true end of
                        true -> Out bsl P;
                        false -> ok
                    end
                end
                ok
            end, Files),
                        ok
        end
        end.

walk(Path, F) ->
    case Path == <<"">> of
        true -> ok;
        false -> 
            case not is_dir(Path) of
                true -> ok;
                false -> begin
                    Remaining = [],
                    Clean_path = 'string.trim_right'(Path, <<"/">>),
                    % TODO: unhandled stmt type
                                        ok
                end
                        end
                end.

walk_with_context(Path, Context, Fcb) ->
    case Path == <<"">> of
        true -> ok;
        false -> 
            case not is_dir(Path) of
                true -> ok;
                false -> begin
                    Remaining = [],
                    Clean_path = 'string.trim_right'(Path, <<"/">>),
                    Loops = 0,
                    % TODO: unhandled stmt type
                                        ok
                end
                        end
                end.

log(S) ->
    vbeam_io:println(<<(<<"os.log: ">>)/binary, (S)/binary>>),
    ok.

mkdir_all(Opath, Params) ->
    case exists(Opath) of
        true -> error(<<"path `", (Opath)/binary, "` already exists, and is not a folder">>);
        false -> begin
            Other_separator = ,
            Path = binary:replace(Opath, Other_separator, <<"/">>, [global]),
            P = case case string:prefix(Path, <<"/">>) of nomatch -> false; _ -> true end of
                true -> <<"/">>;
                false -> <<"">>
            end,
            Path_parts = binary:split('string.trim_left'(Path, <<"/">>), <<"/">>, [global]),
            lists:foreach(fun(Subdir) ->
                P1 = <<(Subdir)/binary, (<<"/">>)/binary>>,
                case exists(P1) andalso is_dir(P1) of
                    true -> ok;
                    false -> ok
                end,
                mkdir(P1, Params),
                ok
            end, Path_parts),
            ok
        end
        end.

create_folder_when_it_does_not_exist(Path) ->
    case is_dir(Path) orelse is_link(Path) of
        true -> ok;
        false -> begin
            Error_msg = <<"">>,
            lists:foreach(fun(_) ->
                mkdir_all(Path, #{mode => 8#700, {vbeam, type} => 'MkdirParams'}),
                % TODO: unhandled stmt type
                ok
            end, lists:seq(0, 10 - 1)),
            case is_dir(Path) orelse is_link(Path) of
                true -> ok;
                false -> erlang:error({panic, Error_msg})
                        end
        end
        end.

xdg_home_folder(Ename, Lpath) ->
    Xdg_folder = getenv(Ename),
    Dir = case Xdg_folder /= <<"">> of
        true -> Xdg_folder;
        false -> join_path_single(home_dir(), Lpath)
    end,
    create_folder_when_it_does_not_exist(Dir),
    Dir.

cache_dir() ->
    xdg_home_folder(<<"XDG_CACHE_HOME">>, <<".cache">>).

data_dir() ->
    xdg_home_folder(<<"XDG_DATA_HOME">>, <<".local/share">>).

state_dir() ->
    xdg_home_folder(<<"XDG_STATE_HOME">>, <<".local/state">>).

local_bin_dir() ->
    xdg_home_folder(<<"LOCAL_BIN_DIR">>, <<".local/bin">>).

temp_dir() ->
    Path = getenv(<<"TMPDIR">>),
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    % TODO: unhandled stmt type
    case Path == <<"">> of
        true -> ok;
        false -> ok
    end,
    Path.

vtmp_dir() ->
    Vtmp = getenv(<<"VTMP">>),
    case length(Vtmp) > 0 of
        true -> Vtmp;
        false -> begin
            Uid = getuid(),
            Vtmp1 = join_path_single(temp_dir(), <<"v_", (integer_to_binary(Uid))/binary>>),
            create_folder_when_it_does_not_exist(Vtmp1),
            setenv(<<"VTMP">>, Vtmp1, true),
            Vtmp1
        end
        end.

default_vmodules_path() ->
    Hdir = home_dir(),
    Res = join_path_single(Hdir, <<".vmodules">>),
    Res.

vmodules_dir() ->
    Paths = vmodules_paths(),
    case length(Paths) > 0 of
        true -> lists:nth(1, Paths);
        false -> default_vmodules_path()
        end.

vmodules_paths() ->
    Path = getenv(<<"VMODULES">>),
    case Path == <<"">> of
        true -> ok;
        false -> ok
    end,
    % TODO: unhandled stmt type
    Splitted = binary:split(Path, <<":">>, [global]),
    % TODO: unhandled stmt type
    List = [],
    {Si, Trimmed} = lists:foldl(fun(I, {SiAcc, TrimmedAcc}) ->
        SiOut = lists:nth(I + 1, Splitted),
        TrimmedOut = 'string.trim_right'(Si, <<"/">>),
        List bsl Trimmed,
        {SiOut, TrimmedOut}
    end, {Si, Trimmed}, lists:seq(0, length(Splitted) - 1)),
    List.

resource_abs_path(Path) ->
    Exe = executable(),
    Dexe = dir(Exe),
    Base_path = real_path(Dexe),
    Vresource = getenv(<<"V_RESOURCE_PATH">>),
    case length(Vresource) /= 0 of
        true -> begin
            todo,
            Base_path1 = Vresource,
        end;
        false -> ok
    end,
    Fp = join_path_single(Base_path1, Path),
    Res = real_path(Fp),
    % TODO: unhandled stmt type
    Res.

execute_or_panic(Cmd) ->
    Res = execute(Cmd),
    case maps:get(exit_code, Res) /= 0 of
        true -> begin
            io:format(standard_error, "~s~n", [<<"failed    cmd: ", (Cmd)/binary>>]),
            io:format(standard_error, "~s~n", [<<"failed   code: ", (integer_to_binary(maps:get(exit_code, Res)))/binary>>]),
            erlang:error({panic, maps:get(output, Res)})
        end;
        false -> ok
    end,
    Res.

execute_or_exit(Cmd) ->
    Res = execute(Cmd),
    case maps:get(exit_code, Res) /= 0 of
        true -> begin
            io:format(standard_error, "~s~n", [<<"failed    cmd: ", (Cmd)/binary>>]),
            io:format(standard_error, "~s~n", [<<"failed   code: ", (integer_to_binary(maps:get(exit_code, Res)))/binary>>]),
            io:format(standard_error, "~s~n", [maps:get(output, Res)]),
            exit(1)
        end;
        false -> ok
    end,
    Res.

execute_opt(Cmd) ->
    Res = execute(Cmd),
    case maps:get(exit_code, Res) /= 0 of
        true -> error(maps:get(output, Res));
        false -> Res
        end.

quoted_path(Path) ->

config_dir() ->
    error(<<"Cannot find config directory">>).

'Process.signal_kill'(P) ->
    case (not lists:member(maps:get(status, P), [running, stopped])) of
        true -> ok;
        false -> 
                end.

'Process.signal_term'(P) ->
    case (not lists:member(maps:get(status, P), [running, stopped])) of
        true -> ok;
        false -> ok
        end.

'Process.signal_pgkill'(P) ->
    case (not lists:member(maps:get(status, P), [running, stopped])) of
        true -> ok;
        false -> ok
        end.

'Process.signal_stop'(P) ->
    case maps:get(status, P) /= running of
        true -> ok;
        false -> 
                end.

'Process.signal_continue'(P) ->
    case maps:get(status, P) /= stopped of
        true -> ok;
        false -> 
                end.

'Process.wait'(P) ->
    case maps:get(status, P) == not_started of
        true -> 'Process._spawn'(P);
        false -> ok
    end,
    case (not lists:member(maps:get(status, P), [running, stopped])) of
        true -> ok;
        false -> begin
        end
        end.

'Process.close'(P) ->
    case lists:member(maps:get(status, P), [not_started, closed]) of
        true -> ok;
        false -> 
                end.

'Process.free'(P) ->
    'Process.close'(P),
    ok.

'Process._spawn'(P) ->
    case not maps:get(env_is_custom, P) of
        true -> begin
            Current_environment = environ(),
            lists:foreach(fun(V) ->
                maps:get(env, P) bsl <<(K)/binary, "=", (V)/binary>>,
                ok
            end, Current_environment),
        end;
        false -> ok
    end,
    0.

'Process.is_alive'(P) ->
    lists:member(maps:get(status, P), [running, stopped]).

'Process.set_redirect_stdio'(P) ->

'Process.stdin_write'(P, S) ->
    ok.

'Process.stdout_slurp'(P) ->
    <<"">>.

'Process.stderr_slurp'(P) ->
    <<"">>.

'Process.stdout_read'(P) ->
    <<"">>.

'Process.stderr_read'(P) ->
    <<"">>.

'Process.pipe_read'(P, Pkind) ->
    todo.

'Process.is_pending'(P, Pkind) ->
    false.

'Process.run'(P) ->
    case maps:get(status, P) /= not_started of
        true -> ok;
        false -> 'Process._spawn'(P)
        end.

new_process(Filename) ->
    #{filename => Filename, stdio_fd => [-1, -1, -1], {vbeam, type} => 'Process'}.

'Process.set_args'(P, Pargs) ->
    case maps:get(status, P) /= not_started of
        true -> ok;
        false -> begin
            ok
        end
        end.

'Process.set_work_folder'(P, Path) ->
    case maps:get(status, P) /= not_started of
        true -> ok;
        false -> begin
            ok
        end
        end.

'Process.set_environment'(P, Envs) ->
    case maps:get(status, P) /= not_started of
        true -> ok;
        false -> begin
            lists:foreach(fun(V) ->
                maps:get(env, P) bsl <<(K)/binary, "=", (V)/binary>>,
                ok
            end, Envs),
            ok
        end
        end.

sleep_ms(Ms) ->
    ok.

'SeekMode__static__from'(Input) ->
    error(<<"invalid value">>).

'ChildProcessPipeKind__static__from'(Input) ->
    error(<<"invalid value">>).

'ProcessState__static__from'(Input) ->
    error(<<"invalid value">>).

'Signal__static__from'(Input) ->
    error(<<"invalid value">>).
