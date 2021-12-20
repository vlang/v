module adt

pub struct Queue<T> {
mut:
	elements []T
pub mut:
	len int
}

// is_empty checks if the queue is empty
pub fn (queue Queue<T>) is_empty() bool {
	return queue.len <= 0
}

// peek returns the head of the queue
pub fn (queue Queue<T>) peek() ?T {
	return if !queue.is_empty() { queue.elements.first() } else { error('Queue is empty') }
}

// push adds an element to the tail of the queue
pub fn (mut queue Queue<T>) push(item T) {
	queue.elements << item
	queue.len++
}

// pop removes the element at the head of the queue and returns it
pub fn (mut queue Queue<T>) pop() ?T {
	if !queue.is_empty() {
		queue.len--
		to_return := queue.elements.first()
		queue.elements.delete(0)
		return to_return
	}
	return error('Queue is empty')
}

// str returns a string representation of the queue
pub fn (mut queue Queue<T>) str() string {
	return queue.elements.str()
}
