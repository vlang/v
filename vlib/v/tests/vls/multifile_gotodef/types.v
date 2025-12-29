module main

pub struct MyStruct {
	value int
	name  string
}

pub enum MyEnum {
	first
	second
	third
}

pub fn (ms MyStruct) get_value() int {
	return ms.value
}
