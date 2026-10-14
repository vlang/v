module datatypes

// Deque is a double-ended queue: both ends support push and pop in amortized
// O(1). It is ring-buffer backed, so unlike Queue it is also usable as a
// stack or a palindrome buffer. Like Queue, it requires external
// synchronization when shared between threads.
@[heap]
pub struct Deque[T] {
mut:
	buf  []T
	head int // index of the front element
	len  int
}

// new_deque creates an empty Deque.
pub fn new_deque[T]() Deque[T] {
	return Deque[T]{}
}

// len returns the number of elements.
pub fn (d &Deque[T]) len() int {
	return d.len
}

// is_empty reports whether the deque holds no elements.
pub fn (d &Deque[T]) is_empty() bool {
	return d.len == 0
}

// cap returns the current ring capacity (for tests and tuning).
pub fn (d &Deque[T]) cap() int {
	return d.buf.len
}

// grow doubles the ring capacity, preserving front-to-back order.
fn (mut d Deque[T]) grow() {
	mut new_cap := if d.buf.len == 0 { 4 } else { d.buf.len * 2 }
	mut grown := []T{len: new_cap}
	for i in 0 .. d.len {
		grown[i] = d.buf[(d.head + i) % d.buf.len]
	}
	d.buf = grown
	d.head = 0
}

// push_back appends item at the back.
pub fn (mut d Deque[T]) push_back(item T) {
	if d.buf.len == 0 || d.len == d.buf.len {
		d.grow()
	}
	d.buf[(d.head + d.len) % d.buf.len] = item
	d.len++
}

// push_front prepends item at the front.
pub fn (mut d Deque[T]) push_front(item T) {
	if d.buf.len == 0 || d.len == d.buf.len {
		d.grow()
	}
	d.head = (d.head - 1 + d.buf.len) % d.buf.len
	d.buf[d.head] = item
	d.len++
}

// pop_front removes and returns the front element, or an error if empty.
// The vacated slot no longer retains the removed value.
pub fn (mut d Deque[T]) pop_front() !T {
	if d.len == 0 {
		return error('datatypes: pop_front from empty Deque')
	}
	item := d.buf[d.head]
	// A default fixed array clears references without allocating a new T object.
	empty := [1]T{}
	d.buf[d.head] = empty[0]
	d.head = (d.head + 1) % d.buf.len
	d.len--
	return item
}

// pop_back removes and returns the back element, or an error if empty.
// The vacated slot no longer retains the removed value.
pub fn (mut d Deque[T]) pop_back() !T {
	if d.len == 0 {
		return error('datatypes: pop_back from empty Deque')
	}
	d.len--
	index := (d.head + d.len) % d.buf.len
	item := d.buf[index]
	empty := [1]T{}
	d.buf[index] = empty[0]
	return item
}

// peek_front returns the front element without removing it.
pub fn (d &Deque[T]) peek_front() !T {
	if d.len == 0 {
		return error('datatypes: peek_front on empty Deque')
	}
	return d.buf[d.head]
}

// peek_back returns the back element without removing it.
pub fn (d &Deque[T]) peek_back() !T {
	if d.len == 0 {
		return error('datatypes: peek_back on empty Deque')
	}
	return d.buf[(d.head + d.len - 1) % d.buf.len]
}

// array returns the elements in front-to-back order.
pub fn (d &Deque[T]) array() []T {
	mut out := []T{cap: d.len}
	for i in 0 .. d.len {
		out << d.buf[(d.head + i) % d.buf.len]
	}
	return out
}
