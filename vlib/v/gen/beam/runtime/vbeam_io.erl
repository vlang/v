%% vbeam_io - I/O operations for V language BEAM backend
%% Provides println, print, eprintln, eprint for V programs

-module(vbeam_io).
-export([println/1, print/1, eprintln/1, eprint/1, read_line/0, input/1]).

%% Print with newline to stdout
%% Accepts binary (Erlang representation of V string)
-spec println(binary() | term()) -> ok.
println(Bin) when is_binary(Bin) ->
    io:format("~s~n", [Bin]),
    ok;
println(Term) ->
    %% For non-binary terms, convert to string representation
    io:format("~p~n", [Term]),
    ok.

%% Print without newline to stdout
-spec print(binary() | term()) -> ok.
print(Bin) when is_binary(Bin) ->
    io:format("~s", [Bin]),
    ok;
print(Term) ->
    io:format("~p", [Term]),
    ok.

%% Print with newline to stderr
-spec eprintln(binary() | term()) -> ok.
eprintln(Bin) when is_binary(Bin) ->
    io:format(standard_error, "~s~n", [Bin]),
    ok;
eprintln(Term) ->
    io:format(standard_error, "~p~n", [Term]),
    ok.

%% Print without newline to stderr
-spec eprint(binary() | term()) -> ok.
eprint(Bin) when is_binary(Bin) ->
    io:format(standard_error, "~s", [Bin]),
    ok;
eprint(Term) ->
    io:format(standard_error, "~p", [Term]),
    ok.

%% Read a line from stdin
%% Returns binary (without trailing newline)
-spec read_line() -> binary().
read_line() ->
    case io:get_line("") of
        eof -> <<>>;
        {error, _} -> <<>>;
        Line when is_list(Line) ->
            %% Remove trailing newline and convert to binary
            Trimmed = string:trim(Line, trailing, "\n"),
            list_to_binary(Trimmed)
    end.

%% Print prompt and read line (like Python's input())
%% Returns binary (without trailing newline)
-spec input(binary() | string()) -> binary().
input(Prompt) when is_binary(Prompt) ->
    case io:get_line(binary_to_list(Prompt)) of
        eof -> <<>>;
        {error, _} -> <<>>;
        Line when is_list(Line) ->
            Trimmed = string:trim(Line, trailing, "\n"),
            list_to_binary(Trimmed)
    end;
input(Prompt) when is_list(Prompt) ->
    case io:get_line(Prompt) of
        eof -> <<>>;
        {error, _} -> <<>>;
        Line when is_list(Line) ->
            Trimmed = string:trim(Line, trailing, "\n"),
            list_to_binary(Trimmed)
    end.
