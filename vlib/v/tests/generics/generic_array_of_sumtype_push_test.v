type SumType = int | string

fn template_fn[T](a []T) []T {
	mut b := []T{}
	b << a
	return b
}

fn test_generic_array_of_sumtype_push() {
	ret1 := template_fn([SumType(1)])
	println(ret1)
	assert ret1.len == 1
	assert ret1[0] == SumType(1)

	ret2 := template_fn([SumType('hello')])
	println(ret2)
	assert ret2.len == 1
	assert ret2[0] == SumType('hello')
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
	data []T
}

fn (mut q Queue[T]) push(val T) {
	q.data << val
}

fn test_generic_queue_sumtype_structs() {
	mut queue := Queue[Event]{}
	queue.push(EventA{ a: 10 })
	assert queue.data.len == 1
	assert queue.data[0] == Event(EventA{
		a: 10
	})
}
