// Written by flopetautschnig (floscodes) (c) 2022

module datatypes

pub struct RingBuffer<T> {
mut:
	content []T
}

// new - creates an empty ringbuffer
pub fn new_ringbuffer<T>(s int) RingBuffer<T> {
	return RingBuffer<T>{
		content: []T{cap: s}
	}
}

// push - adds an element to the ringbuffer ensuring that the buffer will never grow
pub fn (mut rb RingBuffer<T>) push(element T) ? {
	if rb.content.len == rb.content.cap {
		eprintln('Error: buffer overflow\n\nCannot push value $element to buffer because buffer has only a capacity of $rb.content.cap')
	} else {
		rb.content.insert(0, element)
	}
}

// pop - returns the oldest element and deletes it from the ringbuffer
pub fn (mut rb RingBuffer<T>) pop() ?T {
	if rb.content.len == 0 {
		eprintln('Error: buffer is empty')
	}
	return rb.content.pop()
}

// clear - emptys the ringbuffer
pub fn (mut rb RingBuffer<T>) clear() {
	rb.content = []T{len: 0, cap: rb.content.cap}
}

// capacity - returns the capacity of the ringbuffer
pub fn (rb RingBuffer<T>) capacity() int {
	return rb.content.cap
}

// is_empty - checks if the ringbuffer is empty
pub fn (rb RingBuffer<T>) is_empty() bool {
	if rb.content.len == 0 {
		return true
	} else {
		return false
	}
}

// is_full - checks if the ringbuffer is full
pub fn (rb RingBuffer<T>) is_full() bool {
	if rb.content.len < rb.content.cap {
		return false
	} else {
		return true
	}
}

// remaining -  returns the remaining capacity of the ringbuffer
pub fn (rb RingBuffer<T>) remaining() int {
	return rb.content.cap - rb.content.len
}
