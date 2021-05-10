struct ABC {
mut:
	ia []int
}

fn main() {
	mut abc := ABC{}

	defer {
		abc.ia << add()
	}
}

fn add() int {
	return 2
}