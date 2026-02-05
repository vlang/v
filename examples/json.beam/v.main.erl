-module('v.main').
-export([main/0, 'User.can_register'/1, 'User.register'/1]).

main() ->
    S = <<"[{ \"name\":\"Frodo\", \"age\":25}, {\"name\":\"Bobby\", \"age\":10}]">>,
    Users = decode(todo, S),
    lists:foreach(fun(User) ->
        vbeam_io:println(<<(maps:get(name, User))/binary, ": ", (integer_to_binary(maps:get(age, User)))/binary>>),
        ok
    end, Users),
    io:format("~s~n", [<<"">>]),
    lists:foreach(fun(User) ->
        vbeam_io:println(<<(integer_to_binary(I))/binary, ") ", (maps:get(name, User))/binary>>),
        case !'User.can_register'(User) of
            true -> vbeam_io:println(<<"Cannot register ", (maps:get(name, User))/binary, ", they are too young">>);
            false -> begin
                'User.register'(lists:nth(I + 1, Users)),
                vbeam_io:println(<<(maps:get(name, User))/binary, " is registered">>)
            end
        end,
        ok
    end, Users),
    io:format("~s~n", [<<"">>]),
    vbeam_io:println(encode(Users)),
    ok.

'User.can_register'(U) ->
    maps:get(age, U) >= 16.

'User.register'(U) ->
