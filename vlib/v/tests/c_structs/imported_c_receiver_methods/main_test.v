module main

import bridge

type InheritedCounter = bridge.Counter

type MiddleCounter = InheritedCounter

type OuterCounter = MiddleCounter

fn (c MiddleCounter) read() int { return c.value + 400 }

fn test_methods_on_c_struct_fields_and_local_copies() {
	h := bridge.make_holder()
	assert h.value.read() == 17
	assert h.value.@union(2) == 19
	assert h.value.@select[int](1) == 17
	assert h.value.convert[int](1) == 17
	assert h.value.read_again() == 17
	alias_value := bridge.Counter(h.value)
	assert alias_value.read() == 117
	assert alias_value.convert[int](1) == 217
	assert alias_value.alias_only[int](1) == 317
	inherited := InheritedCounter(h.value)
	assert inherited.read() == 117
	assert inherited.@union(2) == 119
	assert inherited.convert[int](1) == 217
	assert inherited.alias_only[int](1) == 317
	outer := OuterCounter(h.value)
	assert outer.read() == 417
	outer_ref := &outer
	assert outer_ref.read() == 417
}

fn call_bound_reader(reader fn () int) int {
	return reader()
}

fn test_imported_c_receiver_method_values() {
	read := bridge.make_holder().value.read
	assert read() == 17
	assert call_bound_reader(bridge.make_holder().value.read) == 17
	value := bridge.make_holder().value
	escaped := value.@union
	assert escaped(2) == 19
	inherited := InheritedCounter(value)
	alias_read := inherited.read
	alias_escaped := inherited.@union
	assert alias_read() == 117
	assert alias_escaped(2) == 119
	outer := OuterCounter(value)
	nearest := outer.read
	assert nearest() == 417
}

fn test_imported_mutable_c_method_value_borrows_its_receiver() {
	mut value := bridge.make_holder().value
	increment := value.increment
	increment()
	assert value.value == 18
}
