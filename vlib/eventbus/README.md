# Event Bus

A module to provide eventing capabilities using pub/sub.

## API

1. `new()` - create a new `EventBus`

### Structs:

**EventBus:**

1. `publish(string, voidptr, Params)` - publish an event with provided Params & name
2. `clear_all()` - clear all subscribers
3. `has_subscriber(string)` - check if a subscriber to an event exists

**Subscriber:**

1. `subscribe(string, fn(voidptr, Params))` - subscribe to an event
2. `subscribe_once(string, fn(voidptr, Params))` - subscribe only once to an event
3. `is_subscribed(string)` - check if we are subscribed to an event
4. `unsubscribe(string)` - unsubscribe from an event

**Event Handler Signature:**

The function given to `subscribe` and `subscribe_once` must match this:

```v
fn(voidptr, Params){

}
// Example
fn onPress(sender voidptr, p Params){
    //your code here...
}
```

## Usage

For **usage across modules** [check the example](https://github.com/vlang/v/tree/master/examples/eventbus).

_Note: As a general rule, you will need to **subscribe before publishing**._

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
fn on_error(sender voidptr, p eventbus.Params) {
    //cast the sender to the real type
    //you can also make this mutable if required.
    work := *(*Work(sender)) //a little verbose but works

    error := p.get_string("error")
	println('error occured on ${work.hours}. Error: ${error}')
}
```

**work.v**

```v
module main

import (
	eventbus
)

struct Work{
    hours int
}

fn do_work(){
    work := Work{20}
    // get a mutable Params instance & put some data into it
	mut params := eventbus.Params{}
    params.put_string("error", "Error: no internet connection.")
    // publish the event
    eb.publish("error", work, params)
}
```

### How to use `Params`:

```v
mut params := eventbus.Params{}

params.put_string("string", "some_string")
params.put_int("int", 20)
params.put_bool("bool", true)

// add maps and arrays of any type like this
arr := [1,2,3]
params.put_array("array", arr)
mp :=  {"hello": "world"}
params.put_map("map", mp)

//get and use the params like this
assert params.get_string("string") == "some_string"
assert params.get_int("int") == 20
assert params.get_bool("bool") == true

g_arr := params.get_array("array", 0)
assert g_arr[0] == 1

g_m := params.get_map("map", "")
assert g_m["hello"] == "world"
```

#### Caution when putting arrays:

Currently putting arrays and maps directly as parameters in `put_array` doesn't work, so make a variable first and use that.

### Notes:

1. Each `EventBus` instance has it's own registry (i.e. there is no global event registry so you can't just subscribe to an event wherever you are.
2. Each `EventBus` has a `Subscriber` instance which will need to be either exposed or you can make small public helper functions specific to your module like (`onPress`, `onError`) and etc.
3. The `eventbus` module has some helpers to ease getting/setting of Params (since V doesn't support empty interfaces yet or reflection) so use them (see usage above).

**The rationale behind separating Subscriber & Publisher:**

This is mainly for security because the if publisher & subscriber are both passed around, a client can easily publish events acting as the server. So a client should only be able to use the Subscriber methods.
