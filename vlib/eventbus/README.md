# Event Bus

A module to provide eventing capabilities using pub/sub.

## API

1. `new()` - create a new `EventBus`

### Structs:

**EventBus:**

1. `publish(name string, sender voidptr, args voidptr)` - publish an event with provided
    Params & name
2. `clear_all()` - clear all subscribers
3. `has_subscriber(name string)` - check if a subscriber to an event exists

**Subscriber:**

1. `subscribe(name string, handler EventHandlerFn)` - subscribe to an event
2. `subscribe_once(name string, handler EventHandlerFn)` - subscribe only once to an event
3. `subscribe_method(name string, handler EventHandlerFn, receiver voidptr)` - subscribe to
    an event and also set the `receiver` as a parameter.
    Since it's not yet possible to send methods as parameters, this is a workaround.
4. `is_subscribed(name string)` - check if we are subscribed to an event
5. `unsubscribe(name string)` - unsubscribe from an event

**Event Handler Signature:**

The function given to `subscribe`, `subscribe_method` and `subscribe_once` must match this:

```v oksyntax
fn cb(receiver voidptr, args voidptr, sender voidptr) {
}

// Since V can map structs to voidptr, this also works
struct ClickEvent {
	x int
	y int
}

// Example case where publisher sends ClickEvent as args.
fn on_press(receiver voidptr, e &ClickEvent, sender voidptr) {
	println(e.x)
	// your code here...
}
```

## Usage

For **usage across modules**
[check the example](https://github.com/vlang/v/tree/master/examples/eventbus).

_Note: As a general rule, you will need to **subscribe before publishing**._

**main.v**

```v oksyntax
module main

import eventbus

// initialize it globally
const (
	eb = eventbus.new()
)

fn main() {
	// get a mutable reference to the subscriber
	mut sub := eb.subscriber
	// subscribe to the 'error' event
	sub.subscribe('error', on_error)
	// start the work
	do_work()
}

// the event handler
fn on_error(receiver voidptr, e &Error, work &Work) {
	println('error occured on ${work.hours}. Error: ${e.message}')
}
```

**work.v**

```v oksyntax
module main

import eventbus

const eb = eventbus.new()

struct Work {
	hours int
}

struct AnError {
	message string
}

fn do_work() {
	work := Work{20}
	// get a mutable Params instance & put some data into it
	error := &AnError{'Error: no internet connection.'}
	// publish the event
	eb.publish('error', work, error)
}
```

### Notes:

1. Each `EventBus` instance has it's own registry (i.e. there is no global event registry
    so you can't just subscribe to an event wherever you are.
2. Each `EventBus` has a `Subscriber` instance which will need to be either exposed or you can make
    small public helper functions specific to your module like (`onPress`, `onError`) and etc.
3. The `eventbus` module has some helpers to ease getting/setting of Params
    (since V doesn't support empty interfaces yet or reflection) so use them (see usage above).

**The rationale behind separating Subscriber & Publisher:**

This is mainly for security because if publisher & subscriber are both passed around,
a client can easily publish events acting as the server.
So a client should only be able to use the Subscriber methods.
