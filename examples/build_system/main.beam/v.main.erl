-module('v.main').
-export([main/0]).

main() ->
    vbeam_io:println(<<"Hello, ", (lists:nth(2, 'v.os':'arguments'()))/binary, "!">>),
    ok.
