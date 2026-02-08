interface Speaker {
	speak()
}

struct Cat {}

fn (c &Cat) speak() {}

struct EventA {
	a u32
}

struct EventB {
	b u32
}

type Event = EventA | EventB

fn some_func() {
	queue := Queue.new[Speaker]()
}

struct Queue[T] {
mut:
	data &T = unsafe { nil }
}

fn Queue.new[T]() Queue[T] {
	return Queue[T]{}
}

fn test_main() {
	some_func()
	assert true
}
