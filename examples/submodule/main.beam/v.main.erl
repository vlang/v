-module('v.main').
-export([main/0]).

main() ->
    vbeam_io:println(integer_to_binary(add_xy(2, 3))),
    vbeam_io:println(integer_to_binary(sub_xy(10, 7))),
    ok.
