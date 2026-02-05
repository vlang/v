%% vbeam_io - I/O operations for V language BEAM backend
%% Provides println, print, eprintln, eprint for V programs

-module(vbeam_io).
-export([println/1, print/1, eprintln/1, eprint/1]).

%% Print with newline to stdout
%% Accepts binary (Erlang representation of V string)
println(Bin) when is_binary(Bin) ->
    io:format("~s~n", [Bin]),
    ok;
println(Term) ->
    %% For non-binary terms, convert to string representation
    io:format("~p~n", [Term]),
    ok.

%% Print without newline to stdout
print(Bin) when is_binary(Bin) ->
    io:format("~s", [Bin]),
    ok;
print(Term) ->
    io:format("~p", [Term]),
    ok.

%% Print with newline to stderr
eprintln(Bin) when is_binary(Bin) ->
    io:format(standard_error, "~s~n", [Bin]),
    ok;
eprintln(Term) ->
    io:format(standard_error, "~p~n", [Term]),
    ok.

%% Print without newline to stderr
eprint(Bin) when is_binary(Bin) ->
    io:format(standard_error, "~s", [Bin]),
    ok;
eprint(Term) ->
    io:format(standard_error, "~p", [Term]),
    ok.
