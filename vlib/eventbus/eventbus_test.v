@[has_globals]
module main

import eventbus

struct EventData {
	data string
	n    int
}

struct FakeReceiver {
	ok bool
}

fn test_eventbus_string() {
	ev_data := &EventData{'hello', 1}
	mut eb := eventbus.new[string]()
	eb.subscriber.subscribe_once('on_test', on_test_without_receiver)
	assert eb.has_subscriber('on_test')
	assert !eb.has_subscriber('not_exist')
	assert eb.subscriber.is_subscribed('on_test')
	eprintln('> publishing to on_test 1')
	reset_ncalls()
	eb.publish('on_test', eb, ev_data)
	assert calls_of_on_test_without_receiver == 1
	reset_ncalls()
	assert !eb.has_subscriber('on_test')
	assert !eb.subscriber.is_subscribed('on_test')
	// subscribe 3 times more:
	eb.subscriber.subscribe('on_test', on_test_without_receiver)
	eb.subscriber.subscribe('on_test', on_test_without_receiver)
	eb.subscriber.subscribe('on_test', on_test_without_receiver)
	assert eb.has_subscriber('on_test')
	assert eb.subscriber.is_subscribed('on_test')
	eprintln('> publishing to on_test 2')
	reset_ncalls()
	eb.publish('on_test', eb, &EventData{'hello', 2})
	assert calls_of_on_test_without_receiver == 1, 'even though we had more than 1 subscription, to the same event, the callback should be called just once because of the deduplication'
	reset_ncalls()
	eb.clear_all()
	assert !eb.has_subscriber('on_test')
	assert !eb.subscriber.is_subscribed('on_test')
	// check that unsubscription actually remove the handler:
	reset_ncalls()
	eb.publish('on_test', eb, &EventData{'hello', 3})
	assert calls_of_on_test_without_receiver == 0
	reset_ncalls()
}

enum Events {
	event_1
	event_2
	event_3
}

fn test_eventbus_enum() {
	reset_ncalls()
	ev_data := &EventData{'hello', 1}
	mut eb := eventbus.EventBus.new[Events]()
	eprintln('> subscribing once on_test for Events enum bus ...')
	eb.subscriber.subscribe_once(Events.event_1, on_test_without_receiver)
	assert eb.has_subscriber(Events.event_1)
	assert !eb.has_subscriber(Events.event_2)
	assert eb.subscriber.is_subscribed(Events.event_1)
	eprintln('> publishing even_1 ...')
	eb.publish(Events.event_1, eb, ev_data)
	assert !eb.has_subscriber(Events.event_1)
	assert !eb.subscriber.is_subscribed(Events.event_1)
	eb.subscriber.subscribe(Events.event_1, on_test_without_receiver)
	assert eb.has_subscriber(Events.event_1)
	assert eb.subscriber.is_subscribed(Events.event_1)
	eb.clear_all()
	assert !eb.has_subscriber(Events.event_1)
	assert !eb.subscriber.is_subscribed(Events.event_1)

	assert calls_of_on_test_without_receiver == 1
	assert call_of_on_test_with_receiver == 0
}

fn test_eventbus_int() {
	reset_ncalls()
	ev_data := &EventData{'hello', 1}
	mut eb := eventbus.EventBus.new[int]()
	eprintln('> subscribing once on_test for int bus ...')
	eb.subscriber.subscribe_once(9999, on_test_without_receiver)
	assert eb.has_subscriber(9999)
	assert !eb.has_subscriber(1111)
	assert eb.subscriber.is_subscribed(9999)
	eb.publish(9999, eb, ev_data)
	assert !eb.has_subscriber(9999)
	assert !eb.subscriber.is_subscribed(9999)
	eb.subscriber.subscribe(9999, on_test_without_receiver)
	assert eb.has_subscriber(9999)
	assert eb.subscriber.is_subscribed(9999)
	eb.clear_all()
	assert !eb.has_subscriber(9999)
	assert !eb.subscriber.is_subscribed(9999)

	assert calls_of_on_test_without_receiver == 1
	assert call_of_on_test_with_receiver == 0
}

fn test_subscribe_method() {
	reset_ncalls()
	// given
	mut eb := eventbus.new[string]()
	r := FakeReceiver{}
	assert !eb.subscriber.is_subscribed_method('on_test_with_receiver', r)
	// when
	eb.subscriber.subscribe_method('on_test_with_receiver', on_test_with_receiver, r)
	// then
	assert eb.subscriber.is_subscribed_method('on_test_with_receiver', r)

	assert calls_of_on_test_without_receiver == 0
	assert call_of_on_test_with_receiver == 0
}

fn test_unsubscribe_method() {
	reset_ncalls()
	// given
	mut eb := eventbus.new[string]()
	r := FakeReceiver{}
	r2 := FakeReceiver{}
	// when
	eb.subscriber.subscribe_method('on_test_with_receiver', on_test_with_receiver, r)
	eb.subscriber.subscribe_method('on_test_with_receiver', on_test_with_receiver, r2)
	eb.subscriber.unsubscribe_method('on_test_with_receiver', r)
	// then
	assert !eb.subscriber.is_subscribed_method('on_test_with_receiver', r)
	assert eb.subscriber.is_subscribed_method('on_test_with_receiver', r2)

	assert calls_of_on_test_without_receiver == 0
	assert call_of_on_test_with_receiver == 0
}

fn test_publish() {
	reset_ncalls()
	// given
	ev_data := &EventData{'hello', 1}
	mut eb := eventbus.new[string]()
	r1 := FakeReceiver{}
	r2 := FakeReceiver{}
	// when
	eb.subscriber.subscribe_once('on_test', on_test_without_receiver)
	eprintln('> publishing once 1 ...')
	eb.publish('on_test', eb, ev_data)
	// then
	assert !eb.subscriber.is_subscribed('on_test')
	assert calls_of_on_test_without_receiver == 1

	eb.subscriber.subscribe_method('on_test', on_test_with_receiver, r1) // subscribe r1 3 times
	eb.subscriber.subscribe_method('on_test', on_test_with_receiver, r1)
	eb.subscriber.subscribe_method('on_test', on_test_with_receiver, r1)
	eb.subscriber.subscribe_method('on_test', on_test_with_receiver, r2) // subscribe r2 3 times
	eb.subscriber.subscribe_method('on_test', on_test_with_receiver, r2)
	eb.subscriber.subscribe_method('on_test', on_test_with_receiver, r2)
	eb.publish('on_test', eb, &EventData{'hello', 123})

	assert calls_of_on_test_without_receiver == 1 // this should not change, since the subscription was done through subscribe_once.
	assert call_of_on_test_with_receiver == 2 // make sure that even though we subscribed several times, only 2 calls are made (once per receiver)
}

fn test_publish_with_receiver() {
	reset_ncalls()
	// given
	mut eb := eventbus.new[string]()
	ev_data := &EventData{'hello', 1}
	r := FakeReceiver{}
	// when
	eb.subscriber.subscribe_method('on_test_with_receiver', on_test_with_receiver, r)
	eb.publish('on_test_with_receiver', eb, ev_data)

	assert calls_of_on_test_without_receiver == 0
	assert call_of_on_test_with_receiver == 1
}

fn test_unsubscribe_reveiver() {
	reset_ncalls()
	// given
	mut eb := eventbus.new[string]()
	r := &FakeReceiver{}
	// when
	eb.subscriber.subscribe_method('on_test_with_receiver', on_test_with_receiver, r)
	eb.subscriber.subscribe_method('on_test', on_test_without_receiver, r)
	eb.subscriber.unsubscribe_receiver(r)
	assert !eb.subscriber.is_subscribed_method('on_test_with_receiver', r)
	assert !eb.subscriber.is_subscribed_method('on_test', r)

	assert calls_of_on_test_without_receiver == 0
	assert call_of_on_test_with_receiver == 0
}

fn on_test_without_receiver(receiver voidptr, ev &EventData, sender voidptr) {
	calls_of_on_test_without_receiver++
	eprintln('>> ${@LOCATION:-80s} | receiver: ${voidptr(receiver):16} | sender: ${sender:16} | ev.data: ${ev.data} | ev.n: ${ev.n}')
	assert receiver == 0
	assert sender != 0
	assert ev.data == 'hello'
}

fn on_test_with_receiver(receiver &FakeReceiver, ev &EventData, sender voidptr) {
	call_of_on_test_with_receiver++
	eprintln('>> ${@LOCATION:-80s} | receiver: ${voidptr(receiver):16} | sender: ${sender:16} | ev.data: ${ev.data} | ev.n: ${ev.n}')
	assert receiver.ok == false
	assert sender != 0
	assert ev.data == 'hello'
}

__global calls_of_on_test_without_receiver = 0
__global call_of_on_test_with_receiver = 0

fn reset_ncalls() {
	calls_of_on_test_without_receiver = 0
	call_of_on_test_with_receiver = 0
}
