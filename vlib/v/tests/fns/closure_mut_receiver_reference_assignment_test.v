@[heap]
struct CapturedHeapReceiver {
mut:
	n int
}

fn (mut receiver CapturedHeapReceiver) bump(amount int) {
	receiver.n += amount
}

fn (mut receiver CapturedHeapReceiver) setup() {
	fn [mut receiver] () {
		mut alias := receiver
		assert typeof(receiver).name == '&CapturedHeapReceiver'
		assert typeof(alias).name == '&CapturedHeapReceiver'
		alias.bump(1)
		mut explicit := &CapturedHeapReceiver(receiver)
		explicit.bump(10)
		fn [mut alias] () {
			mut nested := alias
			assert typeof(nested).name == '&CapturedHeapReceiver'
			nested.bump(100)
		}()
	}()
}

fn (mut receiver CapturedHeapReceiver) setup_with_parentheses() {
	fn [mut receiver] () {
		// Keep parentheses to exercise receiver inference.
		// vfmt off
		mut alias := (receiver)
		assert typeof(alias).name == '&CapturedHeapReceiver'
		alias.bump(1)
		mut grouped := (((receiver)))
		assert typeof(grouped).name == '&CapturedHeapReceiver'
		grouped.bump(10)
		fn [mut receiver] () {
			mut nested := ((receiver))
			assert typeof(nested).name == '&CapturedHeapReceiver'
			nested.bump(100)
		}()
		fn [mut alias] () {
			mut nested := ((alias))
			assert typeof(nested).name == '&CapturedHeapReceiver'
			nested.bump(1000)
		}()
		// vfmt on
	}()
}

fn (mut receiver CapturedHeapReceiver) setup_without_closure() {
	before := receiver.n
	mut value_copy := receiver
	assert typeof(value_copy).name == 'CapturedHeapReceiver'
	value_copy.bump(5)
	assert value_copy.n == before + 5
	assert receiver.n == before
	// Keep parentheses to exercise value-copy inference outside closures.
	// vfmt off
	mut grouped_copy := ((receiver))
	// vfmt on
	assert typeof(grouped_copy).name == 'CapturedHeapReceiver'
	grouped_copy.bump(10)
	assert grouped_copy.n == before + 10
	assert receiver.n == before
	mut effects := 0
	// Keep the block's parenthesized tail to exercise value-copy inference.
	// vfmt off
	mut block_copy := (unsafe {
		effects++
		((receiver))
	})
	// vfmt on
	assert effects == 1
	assert typeof(block_copy).name == 'CapturedHeapReceiver'
	block_copy.bump(20)
	assert block_copy.n == before + 20
	assert receiver.n == before
}

fn test_mut_receiver_reference_assignment_inside_closures() {
	mut receiver := &CapturedHeapReceiver{}
	receiver.setup()
	assert receiver.n == 111
	receiver.setup_without_closure()
	assert receiver.n == 111
}

fn test_mut_pointer_capture_reference_assignment() {
	mut pointer := &CapturedHeapReceiver{}
	fn [mut pointer] () {
		mut alias := pointer
		assert typeof(alias).name == '&CapturedHeapReceiver'
		alias.bump(3)
	}()
	assert pointer.n == 3
}

fn test_parenthesized_mut_receiver_reference_assignment_inside_closures() {
	mut receiver := &CapturedHeapReceiver{}
	receiver.setup_with_parentheses()
	assert receiver.n == 1111
	receiver.setup_without_closure()
	assert receiver.n == 1111
}

fn (mut receiver CapturedHeapReceiver) setup_with_value_blocks() {
	fn [mut receiver] () {
		mut alias := unsafe { receiver }
		assert typeof(alias).name == '&CapturedHeapReceiver'
		alias.bump(1)
		mut effects := 0
		mut with_effects := unsafe {
			effects++
			receiver.bump(2)
			receiver
		}
		assert effects == 1
		assert typeof(with_effects).name == '&CapturedHeapReceiver'
		with_effects.bump(10)
		// Keep mixed parentheses and blocks to exercise receiver inference.
		// vfmt off
		mut mixed := (unsafe {
			effects++
			((receiver))
		})
		// vfmt on
		assert effects == 2
		assert typeof(mixed).name == '&CapturedHeapReceiver'
		mixed.bump(100)
		fn [mut receiver] () {
			mut nested := unsafe { receiver }
			assert typeof(nested).name == '&CapturedHeapReceiver'
			nested.bump(1000)
		}()
		mut explicit := unsafe { &CapturedHeapReceiver(receiver) }
		assert typeof(explicit).name == '&CapturedHeapReceiver'
		explicit.bump(10000)
		address := unsafe { usize(receiver) }
		assert address == unsafe { usize(explicit) }
	}()
}

fn test_mut_receiver_reference_assignment_through_value_blocks() {
	mut receiver := &CapturedHeapReceiver{}
	receiver.setup_with_value_blocks()
	assert receiver.n == 11113
}
