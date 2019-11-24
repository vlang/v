# Event Bus

A module to provide eventing capabilities using pub/sub.

## API

1. `new()` - create a new `EventBus`

### Structs:

**EventBus:**

1. `publish(string, Params)` - publish an event with provided Params & name
2. `clear_all()` - clear all subscribers
3. `has_subscriber(string)` - check if a subscriber to an event exists

**Subscriber:**

1. `subscribe(string, fn(Params))` - subscribe to an event
2. `subscribe_once(string, fn(Params))` - subscribe only once to an event
3. `is_subscribed(string)` - check if we are subscribed to an event
4. `unsubscribe(string)` - unsubscribe from an event

**Event Handler Signature:**

The function given to `subscribe` and `subscribe_once` must match this:

```v
fn(Params){

}
// Example
fn onPress(p Params){
    //your code here...
}
```

## Usage

For **usage across modules** [check the example](https://github.com/vlang/v/tree/master/examples/eventbus).

_Note: As a general rule, you will need to **subscribe before emitting**._

**main.v**

```v
module main
import eventbus

// initialize it globally
const (
    eb = eventbus.new()
)

fn main(){
    // get a mutable reference to the subscriber
	mut sub := eb.subscriber
    // subscribe to the 'error' event
	sub.subscribe("error", on_error)
    // start the work
	do_work()
}

// the event handler
fn on_error(p eventbus.Params) {
	println(p.get_string("error"))
}
```

**work.v**

```v
module main

import (
	eventbus
)

fn do_work(){
    // get a mutable Params instance & put some data into it
	mut params := eventbus.Params{}
    params.put_string("error", "Error: no internet connection.")
    // publish the event
    eb.publish("error", params)
}
```

### How to use `Params`:

```v
mut params := eventbus.Params{}
params.put_string("string", "vevent")
params.put_int("int", 20)
params.put_bool("bo", true)
//the array  & map currently needs to set like this
eventbus.put_array(mut params, "array", [1,2,3])
eventbus.put_map(mut params, "map", "", {"hello": "world"})

params.get_string("string") == "vevent"
params.get_int("int") == 20
params.get_bool("bo") == true
m := params.get_string_map("map")
//the array currently needs to gotten like this
arr := eventbus.get_array(params, "array", 0)

//you can also pass around custom type arrays & maps (it's a little crude but works):
struct Example{}
custom_map := {"example": Example{}}
eventbus.put_map(mut params, "custom_map", Example{}, custom_map)
//and get it like this
eventbus.get_map(params, "custom_map", {"":Example{}}

//For arrays:
eventbus.put_array(mut params, "array", [Example{}])
eventbus.get_array(params, "custom_array", Example{})
```

### Notes:

1. Each `EventBus` instance has it's own registry (i.e. there is no global event registry so you can't just subscribe to an event wherever you are.
2. Each `EventBus` has a `Subscriber` instance which will need to be either exposed or you can make small public helper functions specific to your module like (`onPress`, `onError`) and etc.
3. The `eventbus` module has some helpers to ease getting/setting of Params (since V doesn't support empty interfaces yet or reflection) so use them (see usage above).

**The rationale behind separating Subscriber & Emitter:**

This is mainly for security because the if emitter & subscriber are both passed around, a client can easily emit events acting as the server. So a client should only be able to use the Subscriber methods.
