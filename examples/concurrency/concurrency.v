import time

// Simulate expensive computing using sleep function
fn expensive_computing(id int, duration int) {
	println('Executing expensive computing task ($id)...')
	time.sleep(duration * time.millisecond)
	println('Finish task $id on $duration ms')
}

fn main() {
	mut threads := []thread{}
	threads << spawn expensive_computing(1, 100)
	threads << spawn expensive_computing(2, 500)
	threads << spawn expensive_computing(3, 1000)
	// Join all tasks
	threads.wait()
	println('All jobs finished!')
}
