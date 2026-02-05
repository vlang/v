-module('v.main').
-export([should_not_return/1, main/0]).

should_not_return(Logger) ->
    'Log.fatal'(Logger, <<(todo)/binary, ": yikes!">>),
    ok.

main() ->
    My_log = #{{vbeam, type} => 'Log'},
    should_not_return(My_log),
    ok.
