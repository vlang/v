%% vbeam_zstd - Zstandard compression via port to zstd CLI
%%
%% Wraps the `zstd` command-line tool via Erlang ports for compress/decompress.
%% Also provides streaming compress/decompress via persistent port connections.
%%
%% Prerequisites: `zstd` must be installed and on $PATH.
%% Check with: vbeam_zstd:is_available().
%%
%% Static API:
%%   compress(Data) -> {ok, Compressed} | {error, Reason}
%%   compress(Data, Level) -> {ok, Compressed} | {error, Reason}
%%   decompress(Data) -> {ok, Decompressed} | {error, Reason}
%%   decompress(Data, Opts) -> {ok, Decompressed} | {error, Reason}
%%
%% Streaming API:
%%   compress_stream_start() -> {ok, Port} | {error, Reason}
%%   compress_stream_start(Level) -> {ok, Port} | {error, Reason}
%%   compress_stream_write(Port, Data) -> Acc
%%   compress_stream_end(Port) -> Acc
%%   decompress_stream_start() -> {ok, Port} | {error, Reason}
%%   decompress_stream_write(Port, Data) -> Acc
%%   decompress_stream_end(Port) -> Acc
-module(vbeam_zstd).
-export([compress/1, compress/2, decompress/1, decompress/2]).
-export([compress_stream_start/0, compress_stream_start/1,
         compress_stream_write/2, compress_stream_end/1]).
-export([decompress_stream_start/0, decompress_stream_write/2,
         decompress_stream_end/1]).
-export([is_available/0]).

%% @doc Check if the zstd CLI tool is available on $PATH.
-spec is_available() -> boolean().
is_available() ->
    case os:find_executable("zstd") of
        false -> false;
        _ -> true
    end.

%% @doc Compress data with default level (3).
-spec compress(binary()) -> {ok, binary()} | {error, term()}.
compress(Data) ->
    compress(Data, 3).

%% @doc Compress data with specified compression level (1-22).
-spec compress(binary(), integer()) -> {ok, binary()} | {error, term()}.
compress(Data, Level) when is_binary(Data), is_integer(Level) ->
    case is_available() of
        false ->
            {error, zstd_not_installed};
        true ->
            TmpIn = tmp_file("zstd_in_"),
            TmpOut = TmpIn ++ ".zst",
            try
                ok = file:write_file(TmpIn, Data),
                Cmd = lists:flatten(io_lib:format(
                    "zstd -~B -f ~s -o ~s 2>/dev/null",
                    [Level, TmpIn, TmpOut])),
                os:cmd(Cmd),
                case file:read_file(TmpOut) of
                    {ok, Compressed} -> {ok, Compressed};
                    {error, _} -> {error, compression_failed}
                end
            after
                file:delete(TmpIn),
                file:delete(TmpOut)
            end
    end;
compress(Data, Level) when is_list(Data), is_integer(Level) ->
    compress(list_to_binary(Data), Level).

%% @doc Decompress zstd-compressed data.
-spec decompress(binary()) -> {ok, binary()} | {error, term()}.
decompress(Data) ->
    decompress(Data, #{}).

%% @doc Decompress zstd-compressed data with options.
-spec decompress(binary(), map()) -> {ok, binary()} | {error, term()}.
decompress(Data, _Opts) when is_binary(Data) ->
    case is_available() of
        false ->
            {error, zstd_not_installed};
        true ->
            TmpIn = tmp_file("zstd_dec_"),
            TmpOut = TmpIn ++ ".raw",
            try
                ok = file:write_file(TmpIn, Data),
                Cmd = lists:flatten(io_lib:format(
                    "zstd -d -f ~s -o ~s 2>/dev/null",
                    [TmpIn, TmpOut])),
                os:cmd(Cmd),
                case file:read_file(TmpOut) of
                    {ok, Decompressed} -> {ok, Decompressed};
                    {error, _} -> {error, decompression_failed}
                end
            after
                file:delete(TmpIn),
                file:delete(TmpOut)
            end
    end;
decompress(Data, Opts) when is_list(Data) ->
    decompress(list_to_binary(Data), Opts).

%% --- Streaming API ---

%% @doc Start a streaming compression port with default level (3).
-spec compress_stream_start() -> {ok, port()} | {error, term()}.
compress_stream_start() ->
    compress_stream_start(3).

%% @doc Start a streaming compression port with specified level.
-spec compress_stream_start(integer()) -> {ok, port()} | {error, term()}.
compress_stream_start(Level) ->
    case is_available() of
        false ->
            {error, zstd_not_installed};
        true ->
            Cmd = lists:flatten(io_lib:format("zstd -~B -c", [Level])),
            Port = open_port({spawn, Cmd},
                             [binary, stream, use_stdio, exit_status]),
            {ok, Port}
    end.

%% @doc Write data to a streaming compression port.
%% Returns any compressed output available so far.
-spec compress_stream_write(port(), binary()) -> binary().
compress_stream_write(Port, Data) ->
    port_command(Port, Data),
    collect_port_data(Port, <<>>).

%% @doc Close a streaming compression port and collect remaining output.
-spec compress_stream_end(port()) -> binary().
compress_stream_end(Port) ->
    port_close(Port),
    collect_final_data(<<>>).

%% @doc Start a streaming decompression port.
-spec decompress_stream_start() -> {ok, port()} | {error, term()}.
decompress_stream_start() ->
    case is_available() of
        false ->
            {error, zstd_not_installed};
        true ->
            Port = open_port({spawn, "zstd -d -c"},
                             [binary, stream, use_stdio, exit_status]),
            {ok, Port}
    end.

%% @doc Write data to a streaming decompression port.
%% Returns any decompressed output available so far.
-spec decompress_stream_write(port(), binary()) -> binary().
decompress_stream_write(Port, Data) ->
    port_command(Port, Data),
    collect_port_data(Port, <<>>).

%% @doc Close a streaming decompression port and collect remaining output.
-spec decompress_stream_end(port()) -> binary().
decompress_stream_end(Port) ->
    port_close(Port),
    collect_final_data(<<>>).

%% --- Internal helpers ---

%% Generate a unique temp file path.
-spec tmp_file(string()) -> string().
tmp_file(Prefix) ->
    N = erlang:unique_integer([positive]),
    "/tmp/" ++ Prefix ++ integer_to_list(N).

%% Collect any data available from a port (non-blocking with short timeout).
-spec collect_port_data(port(), binary()) -> binary().
collect_port_data(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port_data(Port, <<Acc/binary, Data/binary>>)
    after 10 ->
        Acc
    end.

%% Collect remaining data after port close (slightly longer timeout).
-spec collect_final_data(binary()) -> binary().
collect_final_data(Acc) ->
    receive
        {_, {data, Data}} ->
            collect_final_data(<<Acc/binary, Data/binary>>)
    after 100 ->
        Acc
    end.
