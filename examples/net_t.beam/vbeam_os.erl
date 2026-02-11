%% vbeam_os - OS and system operations for V language BEAM backend
%% Provides OS-level functionality

-module(vbeam_os).
-export([arguments/0, getenv/1, getenv/2, setenv/2,
         exit/1, system/1,
         getcwd/0, chdir/1,
         exists/1, is_dir/1, is_file/1,
         read_file/1, write_file/2,
         mkdir/1, mkdir_all/1, rmdir/1,
         ls/1]).

%% Get command line arguments (returns list of binaries)
arguments() ->
    [list_to_binary(Arg) || Arg <- init:get_plain_arguments()].

%% Get environment variable (returns empty binary if not set)
getenv(Name) when is_binary(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> <<>>;
        Value -> list_to_binary(Value)
    end.

%% Get environment variable with default
getenv(Name, Default) when is_binary(Name), is_binary(Default) ->
    case os:getenv(binary_to_list(Name)) of
        false -> Default;
        Value -> list_to_binary(Value)
    end.

%% Set environment variable
setenv(Name, Value) when is_binary(Name), is_binary(Value) ->
    os:putenv(binary_to_list(Name), binary_to_list(Value)),
    ok.

%% Exit with status code
exit(Code) when is_integer(Code) ->
    halt(Code).

%% Execute system command (returns {ExitCode, Output})
system(Cmd) when is_binary(Cmd) ->
    Port = open_port({spawn, binary_to_list(Cmd)}, [exit_status, binary, stderr_to_stdout]),
    collect_port_output(Port, <<>>).

collect_port_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port_output(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, Status}} ->
            {Status, Acc}
    after 60000 ->
        port_close(Port),
        {-1, Acc}
    end.

%% Get current working directory
getcwd() ->
    {ok, Dir} = file:get_cwd(),
    list_to_binary(Dir).

%% Change directory
chdir(Path) when is_binary(Path) ->
    case file:set_cwd(binary_to_list(Path)) of
        ok -> ok;
        {error, Reason} -> error({chdir_failed, Reason})
    end.

%% Check if path exists
exists(Path) when is_binary(Path) ->
    filelib:is_file(binary_to_list(Path)) orelse filelib:is_dir(binary_to_list(Path)).

%% Check if path is a directory
is_dir(Path) when is_binary(Path) ->
    filelib:is_dir(binary_to_list(Path)).

%% Check if path is a regular file
is_file(Path) when is_binary(Path) ->
    filelib:is_regular(binary_to_list(Path)).

%% Read entire file contents
read_file(Path) when is_binary(Path) ->
    case file:read_file(binary_to_list(Path)) of
        {ok, Content} -> Content;
        {error, Reason} -> error({read_file_failed, Path, Reason})
    end.

%% Write content to file
write_file(Path, Content) when is_binary(Path), is_binary(Content) ->
    case file:write_file(binary_to_list(Path), Content) of
        ok -> ok;
        {error, Reason} -> error({write_file_failed, Path, Reason})
    end.

%% Create directory
mkdir(Path) when is_binary(Path) ->
    case file:make_dir(binary_to_list(Path)) of
        ok -> ok;
        {error, eexist} -> ok;  %% Already exists is OK
        {error, Reason} -> error({mkdir_failed, Path, Reason})
    end.

%% Create directory and all parents
mkdir_all(Path) when is_binary(Path) ->
    case filelib:ensure_dir(binary_to_list(Path) ++ "/") of
        ok -> mkdir(Path);
        {error, Reason} -> error({mkdir_all_failed, Path, Reason})
    end.

%% Remove directory
rmdir(Path) when is_binary(Path) ->
    case file:del_dir(binary_to_list(Path)) of
        ok -> ok;
        {error, Reason} -> error({rmdir_failed, Path, Reason})
    end.

%% List directory contents
ls(Path) when is_binary(Path) ->
    case file:list_dir(binary_to_list(Path)) of
        {ok, Files} -> [list_to_binary(F) || F <- Files];
        {error, Reason} -> error({ls_failed, Path, Reason})
    end.
