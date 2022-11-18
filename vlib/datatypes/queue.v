module datatypes

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

// peek returns the head of the queue (first element added)
pub fn (queue Queue<T>) peek() ?T {
	return queue.elements.first()
}

// last returns the tail of the queue (last element added)
pub fn (queue Queue<T>) last() ?T {
	return queue.elements.last()
}

// index returns the element at the given index of the queue
pub fn (queue Queue<T>) index(idx int) ?T {
	return queue.elements.index(idx)
}

// push adds an element to the tail of the queue
pub fn (mut queue Queue<T>) push(item T) {
	queue.elements.push(item)
}

// pop removes the element at the head of the queue and returns it
pub fn (mut queue Queue<T>) pop() ?T {
	return queue.elements.shift()
}

// str returns a string representation of the queue
pub fn (queue Queue<T>) str() string {
	return queue.elements.str()
}

// array returns a array representation of the queue
pub fn (queue Queue<T>) array() []T {
	return queue.elements.array()
}
