fn take[T](ch chan T) ?T {
	mut value := T{}
	if ch.try_pop(mut value) == .success {
		return value
	}
	return none
}

fn put[T](ch chan T, value T) bool {
	return ch.try_push(value) == .success
}

struct Box[T] {
	ch chan T
}

fn (b Box[T]) take() ?T {
	mut value := T{}
	if b.ch.try_pop(mut value) == .success {
		return value
	}
	return none
}

fn test_generic_channel_try_push_and_try_pop() {
	ch := chan int{cap: 1}
	assert put(ch, 7)
	assert !put(ch, 8)
	assert take(ch)? == 7
	assert take(ch) == none
	box := Box[string]{ ch: chan string{cap: 1} }
	assert put(box.ch, 'hello')
	assert box.take()? == 'hello'
}
