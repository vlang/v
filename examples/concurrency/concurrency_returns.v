fn expensive_computing(i int) int {
	return i * i
}

fn main() {
	mut threads := []thread int{}
	for i in 1 .. 10 {
		threads << go expensive_computing(i)
	}
	// Join all tasks
	r := threads.wait()
	println('All jobs finished: $r')
}
