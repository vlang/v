%% vbeam_panic - Panic and error handling for V language BEAM backend
%% Provides panic and assert functionality

-module(vbeam_panic).
-export([panic/1, assert/1, assert/2]).

%% Panic - aborts execution with error message
%% V's panic() equivalent
-spec panic(term()) -> no_return().
panic(Message) when is_binary(Message) ->
    io:format(standard_error, "panic: ~s~n", [Message]),
    %% Get stack trace for debugging using OTP 21+ syntax
    try
        throw(panic)
    catch
        _:_:Stacktrace ->
            print_stacktrace(Stacktrace)
    end,
    halt(1);
panic(Message) when is_list(Message) ->
    panic(list_to_binary(Message));
panic(Message) ->
    panic(vbeam_conv:to_binary(Message)).

%% Assert - panic if condition is false
-spec assert(boolean()) -> ok.
assert(true) ->
    ok;
assert(false) ->
    panic(<<"assertion failed">>).

%% Assert with custom message
-spec assert(boolean(), term()) -> ok.
assert(true, _Message) ->
    ok;
assert(false, Message) ->
    panic(<<"assertion failed: ", (vbeam_conv:to_binary(Message))/binary>>).

%% Print stacktrace in readable format
print_stacktrace([]) ->
    ok;
print_stacktrace([{Module, Function, Arity, Location}|Rest]) when is_integer(Arity) ->
    File = proplists:get_value(file, Location, "unknown"),
    Line = proplists:get_value(line, Location, 0),
    io:format(standard_error, "  at ~s:~s/~p (~s:~p)~n",
              [Module, Function, Arity, File, Line]),
    print_stacktrace(Rest);
print_stacktrace([{Module, Function, Args, Location}|Rest]) when is_list(Args) ->
    File = proplists:get_value(file, Location, "unknown"),
    Line = proplists:get_value(line, Location, 0),
    io:format(standard_error, "  at ~s:~s/~p (~s:~p)~n",
              [Module, Function, length(Args), File, Line]),
    print_stacktrace(Rest);
print_stacktrace([_|Rest]) ->
    print_stacktrace(Rest).
