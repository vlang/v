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
		// Keep parentheses to exercise non-heap receiver value copies.
		// vfmt off
		mut grouped_copy := ((receiver))
		assert typeof(grouped_copy).name == 'Receiver'
		grouped_copy.n = 12
		assert grouped_copy.n == 12
		fn [mut receiver] () {
			mut nested_copy := (receiver)
			assert typeof(nested_copy).name == 'Receiver'
			nested_copy.n = 13
			assert nested_copy.n == 13
		}()
		// vfmt on
		mut effects := 0
		// Keep mixed wrappers to exercise non-heap receiver value copies.
		// vfmt off
		mut block_copy := (unsafe {
			effects++
			((receiver))
		})
		// vfmt on
		assert effects == 1
		assert typeof(block_copy).name == 'Receiver'
		block_copy.n = 14
		assert block_copy.n == 14
	}()
}

type HeapAlias = foreign.Receiver

fn (mut receiver HeapAlias) change_alias() {
	fn [mut receiver] () {
		mut alias := receiver
		alias.n = 10
	}()
}

fn (mut receiver HeapAlias) change_grouped_alias() {
	fn [mut receiver] () {
		// Keep parentheses to exercise imported heap alias inference.
		// vfmt off
		mut alias := ((receiver))
		// vfmt on
		alias.n = 11
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

fn test_imported_heap_alias_parenthesized_capture_retains_reference() {
	mut receiver := &HeapAlias{}
	receiver.change_grouped_alias()
	assert receiver.n == 11
}

fn (mut receiver HeapAlias) change_block_alias() {
	fn [mut receiver] () {
		// Keep mixed wrappers to exercise imported heap alias inference.
		// vfmt off
		mut alias := (unsafe { ((receiver)) })
		// vfmt on
		alias.n = 12
	}()
}

fn test_imported_heap_alias_value_block_capture_retains_reference() {
	mut receiver := &HeapAlias{}
	receiver.change_block_alias()
	assert receiver.n == 12
}
