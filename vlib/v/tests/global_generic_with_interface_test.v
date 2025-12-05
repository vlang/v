@[has_globals]
module main

@[markused]
__global queue1 Queue[Speaker]

@[markused]
__global queue2 Queue[Event]

interface Speaker {
	speak() string
}

struct Cat {}

fn (c &Cat) speak() string {
	return 'meow'
}

struct EventA {
	a u32
}

struct EventB {
	b u32
}

type Event = EventA | EventB

struct Queue[T] {
mut:
	data &T = unsafe { nil }
}

fn test_main() {
	c := Cat{}
	queue1.data = &Speaker(c)
	assert queue1.data.speak() == 'meow'
}
