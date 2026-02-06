-module('v.main').
-export([main/0, on_foo/3, on_bar/3, on_baz/3]).

main() ->
    Sub = get_subscriber(),
    R = #{{vbeam, type} => 'Receiver'},
    'Subscriber[string].subscribe_method'(Sub, <<"event_foo">>, Main.on_foo, R),
    'Subscriber[string].subscribe'(Sub, <<"event_bar">>, Main.on_bar),
    'Subscriber[string].subscribe'(Sub, <<"event_baz">>, Main.on_baz),
    vbeam_io:println(<<(<<"Receiver ok: ">>)/binary, (atom_to_binary(maps:get(ok, R)))/binary>>),
    do_work(),
    vbeam_io:println(<<(<<"Receiver ok: ">>)/binary, (atom_to_binary(maps:get(ok, R)))/binary>>),
    ok.

on_foo(Receiver, E, _sender) ->
    vbeam_io:println(<<(<<"on_foo :: ">>)/binary, (maps:get(message, E))/binary>>),
    ok.

on_bar(_receiver, E, _sender) ->
    vbeam_io:println(<<(<<"on_bar :: ">>)/binary, (maps:get(message, E))/binary>>),
    ok.

on_baz(_receiver, _event, D) ->
    vbeam_io:println(<<(<<"on_baz :: ">>)/binary, (integer_to_binary(maps:get(hours, D)))/binary>>),
    ok.
