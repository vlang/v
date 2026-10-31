struct Item {
	value int
}

struct Record {
	item Item
}

const main = Record{ item: Item{ value: 7 } }
const item = &Item{ value: 42 }

fn test_implicit_main_constant_selector_uses_module_type() {
	assert main.item.value == 42
}
