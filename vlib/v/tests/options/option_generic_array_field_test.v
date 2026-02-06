module main

@[heap]
struct FLQueue[T] {
mut:
	inner [8]?T
}

fn (mut f FLQueue[T]) put[T](element T) {
	for index, item in f.inner {
		if item == none {
			f.inner[index] = element
			return
		}
	}
	panic('put: No free slot')
}

fn (mut f FLQueue[T]) get[T]() ?T {
	for item in f.inner {
		if item != none {
			return item
		}
	}
	return none
}

@[heap]
struct Thread {
	stack [1024]u8
}

fn test_main() {
	mut q := FLQueue[Thread]{}
	t := Thread{}
	q.put(t)
	p := q.get()
	println(p)
}
