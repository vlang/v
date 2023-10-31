interface CanPerformTask {
	task()
}

struct Task1 {}

fn (task1 Task1) task() {
	println('task1')
}

struct Task2 {}

fn (task2 Task2) task() {
	println('task2')
}

fn test_go_call_interface_method() {
	mut tasks := []CanPerformTask{}

	tasks << Task1{}
	tasks << Task2{}

	for task in tasks {
		spawn task.task()
	}

	assert true
}
