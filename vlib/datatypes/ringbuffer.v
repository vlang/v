// Written by flopetautschnig (floscodes) (c) 2022

module datatypes

// RingBuffer represents a ring buffer also known as a circular buffer.
pub struct RingBuffer[T] {
mut:
	reader  int // index of the tail where data is going to be read
	writer  int // index of the head where data is going to be written
	content []T
}

// new_ringbuffer creates an empty ring buffer of size `s`.
pub fn new_ringbuffer[T](s int) RingBuffer[T] {
	return RingBuffer[T]{
		content: []T{len: s + 1, cap: s + 1}
	} // increasing custom set size by one element in order to make ring flow possible, so that writer cannot equal reader before reader-index has been read.
}

// push adds an element to the ring buffer.
pub fn (mut rb RingBuffer[T]) push(element T) ! {
	if rb.is_full() {
		return error('Buffer overflow')
	} else {
		rb.content[rb.writer] = element
		rb.move_writer()
	}
}

// pop returns the oldest element in the buffer.
pub fn (mut rb RingBuffer[T]) pop() !T {
	mut v := rb.content[rb.reader]
	if rb.is_empty() {
		return error('Buffer is empty')
	} else {
		rb.move_reader()
	}
	return v
}

// push_many pushes an array to the buffer.
pub fn (mut rb RingBuffer[T]) push_many(elements []T) ! {
	for v in elements {
		rb.push(v) or { return err }
	}
}

// pop_many returns `n` elements of the buffer starting with the oldest one.
pub fn (mut rb RingBuffer[T]) pop_many(n u64) ![]T {
	mut elements := []T{}
	for _ in 0 .. n {
		elements << rb.pop() or { return err }
	}
	return elements
}

// is_empty returns `true` if the ring buffer is empty, `false` otherwise.
pub fn (rb RingBuffer[T]) is_empty() bool {
	return rb.reader == rb.writer // if reader equals writer it means that no value to read has been written before. It follows that the buffer is empty.
}

// is_full returns `true` if the ring buffer is full, `false` otherwise.
pub fn (rb RingBuffer[T]) is_full() bool {
	if rb.writer + 1 == rb.reader {
		return true
	} else if rb.writer == rb.content.len - 1 && rb.reader == 0 {
		return true
	} else {
		return false
	}
}

// capacity returns the capacity of the ring buffer.
pub fn (rb RingBuffer[T]) capacity() int {
	return rb.content.cap - 1 // reduce by one because of the extra element explained in function `new_ringbuffer()`
}

// clear empties the ring buffer and all pushed elements.
pub fn (mut rb RingBuffer[T]) clear() {
	rb = RingBuffer[T]{
		content: []T{len: rb.content.len, cap: rb.content.cap}
	}
}

// occupied returns the occupied capacity of the buffer.
pub fn (rb RingBuffer[T]) occupied() int {
	mut reader := rb.reader
	mut v := 0
	if rb.is_empty() {
		return v
	}
	for {
		reader++
		if reader > rb.content.len - 1 {
			reader = 0
		}
		v++
		if reader == rb.writer {
			break
		}
	}
	return v
}

// remaining returns the remaining capacity of the buffer.
pub fn (rb RingBuffer[T]) remaining() int {
	return rb.capacity() - rb.occupied()
}

// head an tail-pointer move methods
// these methods are not public, they are just an eneasement for handling the pointer-movement process.
fn (mut rb RingBuffer[T]) move_reader() {
	rb.reader++
	if rb.reader > rb.content.len - 1 {
		rb.reader = 0
	}
}

fn (mut rb RingBuffer[T]) move_writer() {
	rb.writer++
	if rb.writer > rb.content.len - 1 {
		rb.writer = 0
	}
}
