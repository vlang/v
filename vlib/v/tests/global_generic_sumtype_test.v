@[has_globals]
module main

__global queue2 Queue[Event]

type Event = int | u32

struct Queue[T] {
mut:
	data &T
}

fn test_main() {
	assert queue2.data == unsafe { nil }
}
