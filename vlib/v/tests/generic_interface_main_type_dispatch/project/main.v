import getters

struct Item {
	id int
}

struct ItemGetter {
	value Item
}

fn (getter ItemGetter) get() Item { return getter.value }

struct ArrayGetter {
	value []Item
}

fn (getter ArrayGetter) get() []Item { return getter.value }

struct IntGetter {
	value int
}

fn (getter IntGetter) get() int { return getter.value }

fn main() {
	value := getters.read[Item](ItemGetter{ value: Item{ id: 7 } })
	assert value.id == 7
	values := getters.read_array[Item](ArrayGetter{ value: [Item{ id: 7 }, Item{ id: 9 }] })
	assert values[0].id == 7 && values[1].id == 9
	assert getters.read_scalar[int](IntGetter{ value: 17 }) == 17
}
