-module('v.main').
-export([main/0, decode/1, get_string/2, get_int/2]).

main() ->
    Data = <<"name=Alice\\nage=18">>,
    User = decode(Data),
    vbeam_io:println(User),
    ok.

decode(Data) ->
    Result = #{{vbeam, type} => 'T'},
    Result.

get_string(Data, Field_name) ->
    lists:foreach(fun(Line) ->
        Key_val = binary:split(Line, <<"=">>, [global]),
        case lists:nth(1, Key_val) == Field_name of
            true -> lists:nth(2, Key_val);
            false -> ok
        end,
        ok
    end, binary:split(Data, <<"\n">>, [global])),
    <<"">>.

get_int(Data, Field) ->
    binary_to_integer(get_string(Data, Field)).
