-module('v.main').
-export([main/0]).

main() ->
    Original_password = input_password(<<"Enter your password : ">>),
    Repeated_password = input_password(<<"Confirm password : ">>),
    case Original_password == Repeated_password of
        true -> vbeam_io:println(<<"Password confirmed! You entered: ", (Original_password)/binary, " .">>);
        false -> io:format("~s~n", [<<"Passwords do not match .">>])
    end.
