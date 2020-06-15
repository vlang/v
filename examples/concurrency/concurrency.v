import sync
import time

// Simulate expensive computing using sleep function
fn expensive_computing(id, duration int, wg &sync.WaitGroup) {
	println('Executing expensive computing task (${id})...')
	time.sleep_ms(duration)
	println('Finish task ${id} on ${duration} ms')
	wg.done()
}

fn main() {
	wg := sync.new_waitgroup()
	wg.add(3)
	go expensive_computing(1, 100, wg)
	go expensive_computing(2, 500, wg)
	go expensive_computing(3, 1000, wg)
	// Join all tasks
	wg.wait()
	println('All jobs finished!')
}
