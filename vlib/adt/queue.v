module adt

pub struct Queue<T> {
mut:
	elements LinkedList<T>
}

// is_empty checks if the queue is empty
pub fn (queue Queue<T>) is_empty() bool {
	return queue.elements.is_empty()
}

// len returns the length of the queue
pub fn (queue Queue<T>) len() int {
	return queue.elements.len()
}

// peek returns the head of the queue
pub fn (queue Queue<T>) peek() ?T {
	return if !queue.is_empty() { queue.elements.first() ? } else { error('Queue is empty') }
}

// push adds an element to the tail of the queue
pub fn (mut queue Queue<T>) push(item T) {
	queue.elements.push(item)
}

// pop removes the element at the head of the queue and returns it
pub fn (mut queue Queue<T>) pop() ?T {
	return if !queue.is_empty() { queue.elements.shift() ? } else { error('Queue is empty') }
}

// str returns a string representation of the queue
pub fn (queue Queue<T>) str() string {
	return queue.elements.str()
}
