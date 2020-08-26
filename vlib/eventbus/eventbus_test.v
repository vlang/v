import eventbus

struct EventData {
	data	string
}

fn test_eventbus(){
	ev_data := &EventData{'hello'}
	mut eb := eventbus.new()
	eb.subscriber.subscribe_once("on_test", on_test)

	assert eb.has_subscriber("on_test")
	assert eb.subscriber.is_subscribed("on_test")

	eb.publish("on_test", eb, ev_data)

	assert !eb.has_subscriber("on_test")
	assert !eb.subscriber.is_subscribed("on_test")

	eb.subscriber.subscribe("on_test", on_test)

	assert eb.has_subscriber("on_test")
	assert eb.subscriber.is_subscribed("on_test")

	eb.clear_all()

	assert !eb.has_subscriber("on_test")
	assert !eb.subscriber.is_subscribed("on_test")
}

fn on_test(receiver voidptr, ev &EventData, sender voidptr) {
	assert receiver == 0
	assert sender != 0
	assert ev.data == "hello"
}
