module main

import dep

struct Item {
	id int
}

fn main() {
	value := dep.first[Item, string](Item{ id: 7 }, 'unused')
	assert value.id == 7
	assert dep.first[Item, string](Item{ id: 9 }, 'unused').id == 9
	assert dep.first[[]Item, string]([Item{ id: 11 }], 'unused')[0].id == 11
}
