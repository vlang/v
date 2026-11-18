import strings

// Preserve intentionally borrowed headers under autofree; the tests free them explicitly.
@[manualfree]
fn borrow_swap_builder(mut target strings.Builder, source []u8) {
	unsafe { vmemcpy(&target, &source, int(sizeof(strings.Builder))) }
}

// swap exchanges storage, so the content and the accumulated capacity travel
// with the buffer, in both directions and without copying any bytes.
fn test_swap_exchanges_content() {
	mut producer := strings.new_builder(16)
	mut consumer := strings.new_builder(4)
	producer.write_string('abcdef')
	consumer.write_string('xy')
	producer_cap := producer.cap
	consumer_cap := consumer.cap
	producer_data := producer.data
	consumer_data := consumer.data

	producer.swap(mut consumer)

	assert consumer.data == producer_data
	assert producer.data == consumer_data
	assert consumer.cap == producer_cap
	assert producer.cap == consumer_cap
	assert consumer.str() == 'abcdef'
	assert producer.str() == 'xy'
}

// A drained destination can take the whole pending buffer without allocating.
fn test_swap_into_empty_builder_is_free() {
	mut producer := strings.new_builder(1024)
	mut consumer := strings.new_builder(0)
	producer.write_string('pending output')
	assert consumer.cap == 0
	producer.swap(mut consumer)
	assert consumer.len == 14
	assert consumer.cap >= 1024
	assert producer.len == 0
}

// Both builders have to stay usable after the exchange, in either role.
fn test_swap_keeps_both_builders_usable() {
	mut a := strings.new_builder(8)
	mut b := strings.new_builder(8)
	a.write_string('alpha')
	b.write_string('bravo')
	a.swap(mut b)
	a.write_string('!')
	b.write_string('?')
	assert a.str() == 'bravo!'
	assert b.str() == 'alpha?'
}

// Swapping a builder with itself must be a no-op, not a double free.
fn test_swap_with_self() {
	mut sb := strings.new_builder(8)
	sb.write_string('abc')
	sb.swap(mut sb)
	assert sb.str() == 'abc'
	// Still usable, still owning a single buffer.
	sb.write_string('def')
	assert sb.str() == 'def'
}

// Repeated swaps must not leak or multiply capacity (alternating two queues).
fn test_swap_repeatedly_between_two_builders() {
	mut a := strings.new_builder(64)
	mut b := strings.new_builder(64)
	a_data := a.data
	b_data := b.data
	for i in 0 .. 100 {
		a.write_string('x')
		a.swap(mut b)
		assert a.data == if i % 2 == 0 { b_data } else { a_data }
		assert b.data == if i % 2 == 0 { a_data } else { b_data }
		assert a.cap == 64
		assert b.cap == 64
		assert b.len == 1
		assert b.str() == 'x'
	}
	assert a.len == 0
}

// Both allocations must still be independently freeable after the exchange.
fn test_swap_then_free() {
	mut a := strings.new_builder(32)
	mut b := strings.new_builder(32)
	a.write_string('one')
	b.write_string('two')
	a.swap(mut b)
	assert a.str() == 'two'
	unsafe { a.free() }
	unsafe { b.free() }
	assert a.len == 0
	assert b.len == 0
}

// swap must preserve the `.noslices` ownership flag, so growth after a swap
// still frees the old block instead of leaking it.
fn test_swap_keeps_noslices_flag() {
	mut a := strings.new_builder(4)
	mut b := strings.new_builder(4)
	a.write_string('1234567890') // forces at least one growth
	b.write_string('z') // a non-empty destination with its own block
	a.swap(mut b)
	assert b.data != unsafe { nil }
	// After the swap, `b` owns the grown block: it must keep growing and
	// freeing it (`.noslices`), and the content must be intact.
	b.write_string('abcdefghijklmnopqrstuvwxyz') // grows the swapped-in block
	assert b.str() == '1234567890abcdefghijklmnopqrstuvwxyz'
	a.write_string('1234567890')
	assert a.str() == 'z1234567890'
}

// Keep the intentional aliases even when this test runs with autofree.
@[manualfree]
fn test_swap_shared_prefix_exchanges_distinct_headers() {
	mut owner := strings.new_builder(32)
	owner.write_string('abcdef')
	mut prefix := strings.new_builder(0)
	borrow_swap_builder(mut prefix, owner[..2])
	owner_flags := owner.flags
	prefix_flags := prefix.flags
	assert owner.data == prefix.data
	assert owner_flags != prefix_flags
	owner.swap(mut prefix)
	assert owner.len == 2
	assert owner.cap == 2
	assert owner.flags == prefix_flags
	assert prefix.len == 6
	assert prefix.cap == 32
	assert prefix.flags == owner_flags
	assert owner.bytestr() == 'ab'
	assert prefix.bytestr() == 'abcdef'
	unsafe {
		owner.free()
		prefix.free()
	}
}

@[manualfree]
fn test_swap_preserves_offsets_and_slice_detachment() {
	mut backing := '012345'.bytes()
	mut a := strings.new_builder(32)
	a.write_string('abcdef')
	a.drop(2)
	mut b := strings.new_builder(0)
	// Borrow the slice header explicitly instead of acquiring an autofree copy.
	borrow_swap_builder(mut b, backing[1..4])
	a_data := a.data
	b_data := b.data
	a_flags := a.flags
	b_flags := b.flags
	a.swap(mut b)
	assert a.data == b_data
	assert a.offset == 1
	assert a.flags == b_flags
	assert b.data == a_data
	assert b.offset == 2
	assert b.flags == a_flags
	a.write_string('x')
	b.write_string('y')
	assert a.str() == '123x'
	assert b.str() == 'cdefy'
	assert backing.bytestr() == '012345'
	unsafe {
		a.free()
		b.free()
		backing.free()
	}
}
