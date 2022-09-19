// Written by flopetautschnig (floscodes) (c) 2022

module datatypes

pub struct RingBuffer<T> {
mut:
	reader  int // index of the tail where data is going to be read
	writer  int // index of the head where data is going to be written
	content []T
}

// new_ringbuffer - creates an empty ringbuffer
pub fn new_ringbuffer<T>(s int) RingBuffer<T> {
	return RingBuffer<T>{
		reader: 0
		writer: 0
		content: []T{len: s + 1, cap: s + 1}
	} // increasing custom set size by one element in order to make ring flow possible, so that writer cannot equal reader before reader-index has been read.
}

// push - adds an element to the ringbuffer
pub fn (mut rb RingBuffer<T>) push(element T) {
	if rb.is_full() {
		eprintln('Buffer overflow')
	} else {
		rb.content[rb.writer] = element
		rb.writer = rb.writer + 1
		if rb.writer > rb.content.len - 1 {
			rb.writer = 0
		}
	}
}

// pop - returns the oldest element of the buffer
pub fn (mut rb RingBuffer<T>) pop() T {
	mut v := rb.content[rb.reader]
	if rb.is_empty() {
		eprintln('Buffer is empty')
	} else {
		rb.reader = rb.reader + 1
		if rb.reader > rb.content.len - 1 {
			rb.reader = 0
		}
	}
	return v
}

// is_empty - checks if the ringbuffer is empty
pub fn (rb RingBuffer<T>) is_empty() bool {
	if rb.reader == rb.writer {
		return true // if reader equals writer it means that no value to read has been written before. It follows that the buffer is empty.
	} else {
		return false
	}
}

// is_full - checks if the ringbuffer is full
pub fn (rb RingBuffer<T>) is_full() bool {
	if rb.writer + 1 == rb.reader {
		return true
	} else if rb.writer == rb.content.len - 1 && rb.reader == 0 {
		return true
	} else {
		return false
	}
}

// capacity - returns the capacity of the ringbuffer
pub fn (rb RingBuffer<T>) capacity() int {
	return rb.content.cap - 1 // reduce by one because of the extra element explained in function `new_ringbuffer()`
}

// clear - emptys the ringbuffer
pub fn (mut rb RingBuffer<T>) clear() {
	rb = RingBuffer<T>{
		reader: 0
		writer: 0
		content: []T{len: rb.content.len, cap: rb.content.cap}
	}
}
