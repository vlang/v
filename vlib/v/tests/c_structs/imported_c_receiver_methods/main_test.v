module main

import bridge

type InheritedCounter = bridge.Counter

type MiddleCounter = InheritedCounter

type OuterCounter = MiddleCounter

type CounterRef = &C.Counter

type MiddleCounterRef = CounterRef

type OuterCounterRef = MiddleCounterRef

type AliasedCounterRef = &bridge.Counter

type PointerCounterRef = &C.PointerCounter

fn (c MiddleCounter) read() int { return c.value + 400 }

fn (c MiddleCounterRef) read() int { return c.value + 500 }

fn (c MiddleCounterRef) convert[T](marker T) int { return c.value + 600 }

fn test_pointer_aliases_keep_imported_and_nearest_alias_methods() {
	value := bridge.make_holder().value
	pointer := CounterRef(&value)
	assert pointer.read() == 17
	assert pointer.@union(2) == 19
	assert pointer.convert[int](1) == 17
	assert pointer.same_address(&value)
	assert pointer.same_generic_address[int](&value, 1)
	outer := OuterCounterRef(pointer)
	assert outer.read() == 517
	assert outer.convert[int](1) == 617
	alias_value := bridge.Counter(value)
	alias_pointer := AliasedCounterRef(&alias_value)
	assert alias_pointer.read() == 117
	assert alias_pointer.convert[int](1) == 217
	// These borrowed receivers stay live until the callbacks finish.
	unsafe {
		read := pointer.read
		nearest := outer.read
		assert read() == 17
		assert nearest() == 517
	}
	pointer_value := bridge.make_pointer_counter()
	hex_pointer := PointerCounterRef(&pointer_value)
	assert hex_pointer.hex() == 'pointer counter'
}

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

fn next_counter_index(mut calls []int) int {
	calls[0]++
	return 0
}

fn test_imported_reference_receivers_keep_storage_before_branch_arguments() {
	mut values := [bridge.make_holder().value, bridge.make_holder().value]
	mut calls := [0]
	condition := true
	values[next_counter_index(mut calls)].update(if condition { 3 } else { 5 })
	assert calls[0] == 1
	assert values[0].value == 20
	values[next_counter_index(mut calls)].update(match calls[0] {
		2 { 7 }
		else { 9 }
	})
	assert calls[0] == 2
	assert values[0].value == 27
	assert values[1].value == 17
	// The array is not resized while these addresses are compared.
	first := unsafe { &values[0] }
	second := unsafe { &values[1] }
	assert values[next_counter_index(mut calls)].same_address(if condition {
		first
	} else {
		second
	})
	assert calls[0] == 3
	assert values[next_counter_index(mut calls)].same_address(match calls[0] {
		4 { first }
		else { second }
	})
	assert calls[0] == 4
	assert values[0].@union(if condition { 2 } else { 3 }) == 29
}

fn test_imported_c_iterator_protocol() {
	iterator := bridge.make_iterator(3)
	mut values := []int{}
	for value in iterator { values << value }
	assert values == [1, 2, 3]
}

fn test_imported_hex_methods_respect_pointer_receivers() {
	value := bridge.make_holder().value
	assert value.hex() == 'counter'
	pointer_value := bridge.make_pointer_counter()
	pointer_receiver := &pointer_value
	assert pointer_receiver.hex() == 'pointer counter'
}
