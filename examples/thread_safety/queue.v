import sync

struct ThreadSafeQueue[T] {
mut:
	queue []T
	mutex sync.Mutex
	cond  sync.Cond
}

fn new_queue[T]() &ThreadSafeQueue[T] {
	mutex := sync.new_mutex()
	cond := sync.new_cond()
	return &ThreadSafeQueue[T]{
		queue: []T{}
		mutex: mutex
		cond:  cond
	}
}

fn (mut q ThreadSafeQueue[T]) enqueue(item T) {
	q.mutex.lock()
	defer {
		q.mutex.unlock()
	}
	q.queue << item
	q.cond.signal() // Signal the condition variable
}

// dequeue works in FIFO order
fn (mut q ThreadSafeQueue[T]) dequeue() T {
	q.mutex.lock()
	defer {
		q.mutex.unlock()
	}
	for q.queue.len == 0 {
		q.cond.wait(mut q.mutex)
	}
	data := q.queue[0]
	q.queue.delete(0)
	return data
}

fn (mut q ThreadSafeQueue[T]) destroy() {
	q.mutex.lock()
	q.queue.clear()
	q.mutex.unlock()
	q.cond.destroy()
	q.mutex.destroy()
}

// Producer function example
fn producer(mut queue ThreadSafeQueue[int]) {
	for i in 0 .. 10 {
		queue.enqueue(i)
		println('Produced: ${i}')
	}
}

// Consumer function example
fn consumer(mut queue ThreadSafeQueue[int]) {
	for _ in 0 .. 10 {
		item := queue.dequeue()
		println('Consumed: ${item}')
	}
}

fn main() {
	mut queue := new_queue[int]()

	// Start producer and consumer threads
	producer_thread := spawn producer(mut queue)
	consumer_thread := spawn consumer(mut queue)

	// Wait for threads to complete
	producer_thread.wait()
	consumer_thread.wait()

	// Clean up
	queue.destroy()
}
