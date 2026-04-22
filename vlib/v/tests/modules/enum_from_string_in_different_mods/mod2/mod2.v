module mod2

pub enum MyEnum {
	item1
	item2
}

fn foo() {
	if a := MyEnum.from_string('item2') {
		assert a == MyEnum.item2
	}
	assert false
}
