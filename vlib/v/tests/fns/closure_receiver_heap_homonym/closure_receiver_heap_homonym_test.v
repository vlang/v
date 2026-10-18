module main

import homonym as foreign

struct Receiver {
mut:
	n int
}

fn (mut receiver Receiver) change_copy() {
	fn [mut receiver] () {
		mut value_copy := receiver
		assert typeof(value_copy).name == 'Receiver'
		value_copy.n = 9
		assert value_copy.n == 9
	}()
}

type HeapAlias = foreign.Receiver

fn (mut receiver HeapAlias) change_alias() {
	fn [mut receiver] () {
		mut alias := receiver
		alias.n = 10
	}()
}

fn test_imported_heap_homonym_does_not_change_value_capture() {
	mut receiver := Receiver{}
	receiver.change_copy()
	assert receiver.n == 0
	mut heap := &foreign.Receiver{}
	heap.change_alias()
	assert heap.n == 9
}

fn test_imported_heap_alias_capture_retains_reference() {
	mut receiver := &HeapAlias{}
	receiver.change_alias()
	assert receiver.n == 10
}
