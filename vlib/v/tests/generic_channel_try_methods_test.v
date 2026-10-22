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

fn pop_into[T](ch chan T, mut value T) bool {
	return ch.try_pop(mut value) == .success
}

fn pop_address[T](ch chan T, mut value T) bool {
	return ch.try_pop(mut &value) == .success
}

type ChannelNumber = int

struct ChannelHolder[T] {
mut:
	value T
}

fn pop_field_and_index[T](ch chan T, first T, second T) {
	mut holder := ChannelHolder[T]{}
	assert ch.try_push(first) == .success
	assert ch.try_pop(mut holder.value) == .success
	assert holder.value == first
	mut values := [T{}]
	assert ch.try_push(second) == .success
	assert ch.try_pop(mut values[0]) == .success
	assert values[0] == second
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

fn test_generic_channel_try_alias_and_pointer_storage() {
	ch := chan ChannelNumber{cap: 1}
	assert put(ch, ChannelNumber(12))
	mut value := ChannelNumber(0)
	assert pop_into(ch, mut value)
	assert value == 12
	assert put(ch, ChannelNumber(13))
	assert pop_address(ch, mut value)
	assert value == 13
	pop_field_and_index(ch, ChannelNumber(14), ChannelNumber(15))
	mut pointed := 23
	pointers := chan &int{cap: 1}
	assert put(pointers, &pointed)
	mut destination := &int(unsafe { nil })
	mut storage := &destination
	assert pop_into(pointers, mut storage)
	assert destination == &pointed
}
