import eventbus

struct EventData {
	data string
}

struct FakeReceiver {
	ok bool
}

fn test_eventbus_string() {
	ev_data := &EventData{'hello'}
	mut eb := eventbus.new[string]()
	eb.subscriber.subscribe_once('on_test', on_test)
	assert eb.has_subscriber('on_test')
	assert !eb.has_subscriber('not_exist')
	assert eb.subscriber.is_subscribed('on_test')
	eb.publish('on_test', eb, ev_data)
	assert !eb.has_subscriber('on_test')
	assert !eb.subscriber.is_subscribed('on_test')
	eb.subscriber.subscribe('on_test', on_test)
	assert eb.has_subscriber('on_test')
	assert eb.subscriber.is_subscribed('on_test')
	eb.clear_all()
	assert !eb.has_subscriber('on_test')
	assert !eb.subscriber.is_subscribed('on_test')
}

enum Events {
	event_1
	event_2
	event_3
}

fn test_eventbus_enum() {
	ev_data := &EventData{'hello'}
	mut eb := eventbus.EventBus.new[Events]()
	eb.subscriber.subscribe_once(Events.event_1, on_test)
	assert eb.has_subscriber(Events.event_1)
	assert !eb.has_subscriber(Events.event_2)
	assert eb.subscriber.is_subscribed(Events.event_1)
	eb.publish(Events.event_1, eb, ev_data)
	assert !eb.has_subscriber(Events.event_1)
	assert !eb.subscriber.is_subscribed(Events.event_1)
	eb.subscriber.subscribe(Events.event_1, on_test)
	assert eb.has_subscriber(Events.event_1)
	assert eb.subscriber.is_subscribed(Events.event_1)
	eb.clear_all()
	assert !eb.has_subscriber(Events.event_1)
	assert !eb.subscriber.is_subscribed(Events.event_1)
}

fn test_eventbus_int() {
	ev_data := &EventData{'hello'}
	mut eb := eventbus.EventBus.new[int]()
	eb.subscriber.subscribe_once(9999, on_test)
	assert eb.has_subscriber(9999)
	assert !eb.has_subscriber(1111)
	assert eb.subscriber.is_subscribed(9999)
	eb.publish(9999, eb, ev_data)
	assert !eb.has_subscriber(9999)
	assert !eb.subscriber.is_subscribed(9999)
	eb.subscriber.subscribe(9999, on_test)
	assert eb.has_subscriber(9999)
	assert eb.subscriber.is_subscribed(9999)
	eb.clear_all()
	assert !eb.has_subscriber(9999)
	assert !eb.subscriber.is_subscribed(9999)
}

fn test_subscribe_method() {
	// Does not really test subscribe_method idinvidually though
	// given
	mut eb := eventbus.new[string]()
	r := FakeReceiver{}

	assert !eb.subscriber.is_subscribed_method('on_test_with_receiver', r)
	// when
	eb.subscriber.subscribe_method('on_test_with_receiver', on_test_with_receiver, r)

	// then
	assert eb.subscriber.is_subscribed_method('on_test_with_receiver', r)
}

fn test_unsubscribe_method() {
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
}

fn test_publish() {
	// given
	ev_data := &EventData{'hello'}
	mut eb := eventbus.new[string]()

	// when
	eb.subscriber.subscribe_once('on_test', on_test)
	eb.subscriber.subscribe_once('on_test', on_test)
	eb.publish('on_test', eb, ev_data)

	// then
	assert !eb.subscriber.is_subscribed('on_test')
}

fn test_publish_with_receiver() {
	// given
	mut eb := eventbus.new[string]()
	ev_data := &EventData{'hello'}
	r := FakeReceiver{}

	// when
	eb.subscriber.subscribe_method('on_test_with_receiver', on_test_with_receiver, r)
	eb.publish('on_test_with_receiver', eb, ev_data)

	// then asserts are in on_test_with_receiver, don't know how to be sure
	// that it has been properly called...
}

fn test_unsubscribe_reveiver() {
	// given
	mut eb := eventbus.new[string]()
	r := &FakeReceiver{}

	// when
	eb.subscriber.subscribe_method('on_test_with_receiver', on_test_with_receiver, r)
	eb.subscriber.subscribe_method('on_test', on_test, r)
	eb.subscriber.unsubscribe_receiver(r)
	assert !eb.subscriber.is_subscribed_method('on_test_with_receiver', r)
	assert !eb.subscriber.is_subscribed_method('on_test', r)
}

fn on_test(receiver voidptr, ev &EventData, sender voidptr) {
	assert receiver == 0
	assert sender != 0
	assert ev.data == 'hello'
}

fn on_test_with_receiver(receiver &FakeReceiver, ev &EventData, sender voidptr) {
	assert receiver.ok == false
	assert sender != 0
	assert ev.data == 'hello'
}
