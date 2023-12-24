module main

import mod { MyEnum, MyStruct }

fn main() {
	_ := MyEnum.from_string('item1')
	_ := MyStruct.from_string('item1')
}
