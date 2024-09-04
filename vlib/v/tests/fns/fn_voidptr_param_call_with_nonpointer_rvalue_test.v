import eventbus

struct MyMessage {
	msg string
}

fn test_fn_call_with_nonpointer_rvalue() {
	eb := eventbus.new[string]()
	mut subscriber := eb.subscriber

	subscriber.subscribe('my_publish', subscriber_method)

	do_something(eb)
	assert true
}

fn subscriber_method(receiver voidptr, ev &MyMessage, sender voidptr) {
	println(ev)
}

fn do_something[T](eb &eventbus.EventBus[T]) {
	eb.publish('my_publish', eb, MyMessage{ msg: 'this is my message' })
}
