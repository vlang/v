module main

fn main() {
	mut t := TestStruct{}
	t.list[TestEnum.one] = 1
	assert t.list[TestEnum.one] == 1
}
