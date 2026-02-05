-module('v.no_main').
-export([puts/1, hello/0]).

puts(Const_msg) ->
    ok.

hello() ->
    puts(<<"Hello world">>),
    0.
