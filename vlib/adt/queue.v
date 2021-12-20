module adt

struct Queue<T> {
mut:
	elements []T
pub mut:
	len int
}

fn (queue Queue<T>) is_empty() bool {
	return queue.len <= 0
}

fn (queue Queue<T>) peek() ?T {
	return if !queue.is_empty() { queue.elements.first() } else { error('Queue is empty') }
}

fn (mut queue Queue<T>) push(item T) {
	queue.elements << item
	queue.len++
}

fn (mut queue Queue<T>) pop() ?T {
	if !queue.is_empty() {
		queue.len--
		to_return := queue.elements.first()
		queue.elements.delete(0)
		return to_return
	}
	return error('Queue is empty')
}

fn (mut queue Queue<T>) str() string {
	return queue.elements.str()
}
