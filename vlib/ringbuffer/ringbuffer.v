module ringbuffer

// Written by flopetautschnig (floscodes) (c) 2022

pub struct RingBuffer<T> {
	size u64
mut:
	content []T
}

// new - creates an empty ringbuffer
pub fn new<T>(s u64) RingBuffer<T> {
	return RingBuffer<T>{
		content: []T{}
		size: s
	}
}

// push - adds an element to the ringbuffer
pub fn (mut rb RingBuffer<T>) push<T>(element T) ? {
	if rb.content.len < rb.size {
		rb.content.prepend(element)
	} else {
		return error('Buffer overflow')
	}
}

// pop - returns the oldest element and deletes it from the ringbuffer
pub fn (mut rb RingBuffer<T>) pop() ?T {
	if rb.content.len > 0 {
		return rb.content.pop()
	} else {
		return error('Buffer is empty')
	}
}

// clear - emptys the ringbuffer
pub fn (mut rb RingBuffer<T>) clear() {
	rb.content = []T{}
}

// capacity - returns the capacity of the ringbuffer
pub fn (rb RingBuffer<T>) capacity() u64 {
	return rb.size
}

// is_empty - checks if the ringbuffer is empty
pub fn (rb RingBuffer<T>) is_empty() bool {
	if rb.content.len < 1 {
		return true
	} else {
		return false
	}
}

// is_full - checks if the ringbuffer is full
pub fn (rb RingBuffer<T>) is_full() bool {
	if rb.content.len < rb.size {
		return false
	} else {
		return true
	}
}

// remaining -  returns the remaining capacity of the ringbuffer
pub fn (rb RingBuffer<T>) remaining() u64 {
	return rb.size - u64(rb.content.len)
}
