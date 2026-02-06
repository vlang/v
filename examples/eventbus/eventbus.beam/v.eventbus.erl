-module('v.eventbus').
-export(['EventBus__static__new'/0, new/0, 'EventBus.publish'/4, 'EventBus.clear_all'/1, 'EventBus.has_subscriber'/2, 'Publisher.publish'/4, 'Publisher.clear_all'/1, 'Subscriber.subscribe'/3, 'Subscriber.subscribe_method'/4, 'Subscriber.unsubscribe_method'/3, 'Subscriber.unsubscribe_receiver'/2, 'Subscriber.subscribe_once'/3, 'Subscriber.is_subscribed'/2, 'Subscriber.is_subscribed_method'/3, 'Subscriber.unsubscribe'/3, 'Registry.check_subscriber'/2]).

'EventBus__static__new'() ->
    Registry = #{events => [], {vbeam, type} => 'Registry'},
    #{ => Registry,  => #{ => Registry, {vbeam, type} => 'Publisher'},  => #{ => Registry, {vbeam, type} => 'Subscriber'}, {vbeam, type} => 'EventBus'}.

new() ->
    Registry = #{events => [], {vbeam, type} => 'Registry'},
    #{registry => Registry, publisher => #{registry => Registry, {vbeam, type} => 'Publisher'}, subscriber => #{registry => Registry, {vbeam, type} => 'Subscriber'}, {vbeam, type} => 'EventBus'}.

'EventBus.publish'(Eb, Name, Sender, Args) ->
    Publisher = maps:get(publisher, Eb),
    'Publisher.publish'(Publisher, Name, Sender, Args),
    ok.

'EventBus.clear_all'(Eb) ->
    Publisher = maps:get(publisher, Eb),
    'Publisher.clear_all'(Publisher),
    ok.

'EventBus.has_subscriber'(Eb, Name) ->
    'Registry.check_subscriber'(maps:get(registry, Eb), Name).

'Publisher.publish'(Pb, Name, Sender, Args) ->
    Invalid = 0,
    Handled_receivers = todo,
    J = 0,
    Found_onces = 0,
    lists:foreach(fun(Event) ->
        case maps:get(name, Event) == Name of
            true -> begin
                case maps:get(once, Event) of
                    true -> todo;
                    false -> ok
                end,
                case lists:member(maps:get(receiver, Event), Handled_receivers) of
                    true -> ok;
                    false -> ok
                end,
                'EventHandler.handler'(Event, maps:get(receiver, Event), Args, Sender),
                J1 = (J + 1) rem 20,
            end;
            false -> ok
        end,
        ok
    end, maps:get(events, maps:get(registry, Pb))),
    case Found_onces > 0 of
        true -> ok;
        false -> ok
    end.

'Publisher.clear_all'(P) ->
    'EventHandler.clear'(maps:get(events, maps:get(registry, P))),
    ok.

'Subscriber.subscribe'(S, Name, Handler) ->
    maps:get(events, maps:get(registry, S)) bsl #{name => Name, handler => Handler, {vbeam, type} => 'EventHandler'},
    ok.

'Subscriber.subscribe_method'(S, Name, Handler, Receiver) ->
    maps:get(events, maps:get(registry, S)) bsl #{name => Name, handler => Handler, receiver => Receiver, {vbeam, type} => 'EventHandler'},
    ok.

'Subscriber.unsubscribe_method'(S, Name, Receiver) ->

'Subscriber.unsubscribe_receiver'(S, Receiver) ->

'Subscriber.subscribe_once'(S, Name, Handler) ->
    maps:get(events, maps:get(registry, S)) bsl #{name => Name, handler => Handler, once => true, {vbeam, type} => 'EventHandler'},
    ok.

'Subscriber.is_subscribed'(S, Name) ->
    'Registry.check_subscriber'(maps:get(registry, S), Name).

'Subscriber.is_subscribed_method'(S, Name, Receiver) ->
    'EventHandler.any'(maps:get(events, maps:get(registry, S)), maps:get(name, It) == Name andalso maps:get(receiver, It) == Receiver).

'Subscriber.unsubscribe'(S, Name, Handler) ->

'Registry.check_subscriber'(R, Name) ->
    'EventHandler.any'(maps:get(events, R), maps:get(name, It) == Name).
