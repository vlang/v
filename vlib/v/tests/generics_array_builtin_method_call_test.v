struct Container<T> {
mut:
	items []T
}

fn (mut c Container<T>) pop() ?T {
	return c.items.pop()
}

struct Item {
	data     string
	priority int
}

fn test_generic_array_pop_call() {
	mut a1 := Container<int>{
		items: [11, 22]
	}
	println(a1)
	ret1 := a1.pop() or { 0 }
	println(ret1)
	assert ret1 == 22

	item1 := Item{'a', 1}
	item2 := Item{'b', 2}
	mut a2 := Container<Item>{
		items: [item1, item2]
	}
	println(a2)
	ret2 := a2.pop() or { Item{} }
	assert ret2 == item2
}
