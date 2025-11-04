struct EventA {
	a u32
}

struct EventB {
	b u32
}

type Event = EventA | EventB

fn test_main() {
	some_ptr := voidptr(&EventA{
		a: 1234
	})
	event := unsafe { &Event(some_ptr) }

	d1 := &EventA(event)
	assert d1.a == 1234

	d2 := &EventB(event)
	assert d2.b == 1234
}
