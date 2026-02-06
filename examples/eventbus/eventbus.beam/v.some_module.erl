-module('v.some_module').
-export([do_work/0, get_subscriber/0]).

do_work() ->
    Duration = #{hours => 10, {vbeam, type} => 'Duration'},
    lists:foreach(fun(I) ->
        io:format("~s~n", [<<"working...">>]),
        case I == 5 of
            true -> begin
                Event_metadata = #{message => <<(<<"Iteration ">>)/binary, (integer_to_binary(I))/binary>>, {vbeam, type} => 'EventMetadata'},
                'EventBus[string].publish'(new(), <<"event_foo">>, Duration, Event_metadata),
                'EventBus[string].publish'(new(), <<"event_bar">>, Duration, Event_metadata)
            end;
            false -> ok
        end,
        ok
    end, lists:seq(0, 10 - 1)),
    'EventBus[string].publish'(new(), <<"event_baz">>, #{hours => 42, {vbeam, type} => 'Duration'}, #{message => <<"Additional data at the end.">>, {vbeam, type} => 'EventMetadata'}),
    ok.

get_subscriber() ->
    *maps:get(subscriber, new()).
