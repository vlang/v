import aliases

struct Item {
	id int
}

fn main() {
	values := aliases.values[Item]([Item{ id: 7 }, Item{ id: 9 }])
	assert values[0].id == 7
	assert values[1].id == 9
}
