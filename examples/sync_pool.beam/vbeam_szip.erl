%% vbeam_szip - Zip archive operations via Erlang's built-in zip module
%%
%% Maps V's compress.szip high-level API to Erlang's OTP zip module.
%% The streaming entry-level API (open/read_entry/write_entry) uses
%% port-based unzip for individual entry extraction.
%%
%% High-level API (uses Erlang zip module, no external deps):
%%   extract_zip_to_dir(ZipPath, Dir) -> {ok, Files} | {error, Reason}
%%   zip_files(ZipPath, Files) -> ok | {error, Reason}
%%   zip_folder(ZipPath, FolderPath) -> ok | {error, Reason}
%%   list_entries(ZipPath) -> {ok, Names} | {error, Reason}
%%
%% Streaming API (port-based, for entry-by-entry reading):
%%   open(ZipPath, Mode) -> {ok, Handle} | {error, Reason}
%%   read_entry(Handle) -> {ok, Data, Handle} | eof
%%   close(Handle) -> ok
-module(vbeam_szip).
-export([extract_zip_to_dir/2, zip_files/2, zip_folder/2, list_entries/1]).
-export([open/2, read_entry/1, close/1]).

%% @doc Extract an entire zip archive to a directory.
%% Returns {ok, FileList} with extracted file paths, or {error, Reason}.
-spec extract_zip_to_dir(binary() | string(), binary() | string()) ->
    {ok, [binary()]} | {error, term()}.
extract_zip_to_dir(ZipPath, Dir) ->
    ZipStr = to_list(ZipPath),
    DirStr = to_list(Dir),
    case zip:unzip(ZipStr, [{cwd, DirStr}]) of
        {ok, Files} ->
            {ok, [list_to_binary(F) || F <- Files]};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Create a zip archive from a list of file paths.
%% The files must exist on disk. Returns ok or {error, Reason}.
-spec zip_files(binary() | string(), [binary() | string()]) ->
    ok | {error, term()}.
zip_files(ZipPath, Files) ->
    ZipStr = to_list(ZipPath),
    FilesList = [to_list(F) || F <- Files],
    case zip:zip(ZipStr, FilesList) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Zip an entire folder (recursively) into a zip archive.
%% Filters out directories (only regular files are included).
-spec zip_folder(binary() | string(), binary() | string()) ->
    ok | {error, term()}.
zip_folder(ZipPath, FolderPath) ->
    FolderStr = to_list(FolderPath),
    Pattern = FolderStr ++ "/**",
    AllFiles = filelib:wildcard(Pattern),
    RegularFiles = [F || F <- AllFiles, not filelib:is_dir(F)],
    ZipStr = to_list(ZipPath),
    case zip:zip(ZipStr, RegularFiles) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc List all entries (file names) in a zip archive.
-spec list_entries(binary() | string()) ->
    {ok, [binary()]} | {error, term()}.
list_entries(ZipPath) ->
    ZipStr = to_list(ZipPath),
    case zip:list_dir(ZipStr) of
        {ok, Entries} ->
            Names = [list_to_binary(element(2, E))
                     || E <- Entries, element(1, E) =:= zip_file],
            {ok, Names};
        {error, Reason} ->
            {error, Reason}
    end.

%% --- Streaming API (port-based entry-by-entry reading) ---

%% @doc Open a zip archive for streaming entry-by-entry reading.
%% Returns a handle that can be used with read_entry/1.
-spec open(binary() | string(), term()) ->
    {ok, map()} | {error, term()}.
open(ZipPath, _Mode) ->
    ZipStr = to_list(ZipPath),
    case file:read_file_info(ZipStr) of
        {ok, _} ->
            %% Pre-populate entries list for sequential reading
            case list_entries(ZipPath) of
                {ok, EntryNames} ->
                    {ok, #{path => ZipPath,
                           entries => EntryNames,
                           current => undefined}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Read the next entry from the handle.
%% Returns {ok, Data, UpdatedHandle} or eof when all entries consumed.
-spec read_entry(map()) -> {ok, binary(), map()} | eof.
read_entry(#{path := ZipPath, entries := [Entry | Rest]} = Handle) ->
    Cmd = lists:flatten(io_lib:format(
        "unzip -p ~s \"~s\"",
        [to_list(ZipPath), to_list(Entry)])),
    Data = list_to_binary(os:cmd(Cmd)),
    {ok, Data, Handle#{entries => Rest, current => Entry}};
read_entry(#{entries := []}) ->
    eof.

%% @doc Close a zip handle (no-op, for API symmetry).
-spec close(map()) -> ok.
close(_Handle) ->
    ok.

%% --- Internal helpers ---

%% Convert binary or list to list (for Erlang zip module which expects charlists).
-spec to_list(binary() | string()) -> string().
to_list(Bin) when is_binary(Bin) -> binary_to_list(Bin);
to_list(List) when is_list(List) -> List.
