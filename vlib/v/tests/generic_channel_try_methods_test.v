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

@[unsafe]
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

type CustomTryChannel = chan int

fn (ch CustomTryChannel) try_push(value int) bool { return value == 7 }

fn (ch CustomTryChannel) try_pop(mut values []int) bool {
	values << 42
	return true
}

fn test_channel_alias_uses_declared_try_methods() {
	ch := CustomTryChannel(chan int{})
	assert ch.try_push(7)
	mut values := []int{}
	assert ch.try_pop(mut values)
	assert values == [42]
}

fn push_void_pointer[T](ch chan T, value &int) {
	assert ch.try_push(value) == .success
}

fn pop_shared[T](ch chan T, shared value T) {
	lock value {
		assert ch.try_pop(mut value) == .success
	}
}

fn test_generic_channel_try_void_pointer_and_shared_storage() {
	pointers := chan voidptr{cap: 1}
	pointed := 42
	push_void_pointer(pointers, &pointed)
	pointer := <-pointers
	assert pointer == voidptr(&pointed)
	ch := chan int{cap: 1}
	ch <- 23
	shared value := 0
	pop_shared(ch, shared value)
	rlock value {
		assert value == 23
	}
}

type ChannelValue = int | string

fn test_generic_channel_try_full_sum_type_storage() {
	ch := chan ChannelValue{cap: 1}
	assert put(ch, ChannelValue('hello'))
	assert take(ch)? == ChannelValue('hello')
	assert put(ch, ChannelValue(42))
	assert take(ch)? == ChannelValue(42)
}
