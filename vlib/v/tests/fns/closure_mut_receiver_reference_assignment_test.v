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

fn (mut receiver CapturedHeapReceiver) setup_without_closure() {
	before := receiver.n
	mut value_copy := receiver
	assert typeof(value_copy).name == 'CapturedHeapReceiver'
	value_copy.bump(5)
	assert value_copy.n == before + 5
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
