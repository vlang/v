module main

import consumer
import foreign

fn twice(value int) int { return value * 2 }

fn main() {
	assert foreign.marker() == 41
	assert consumer.local_array() == [3, 7]
	assert consumer.parameter([5, 8]) == 13
	assert consumer.loop_binding() == 6
	closure := consumer.closure()
	assert closure([19]) == 19
	assert consumer.callback(twice) == 8
	item := consumer.Item{ count: 23 }
	assert item.value() == 23
	assert foreign.marker() == 41
	println('foreign globals preserve local, parameter, loop, closure, callback and receiver bindings')
}
