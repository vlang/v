module main

import amod { MyEnum }

fn main() {
	_ = MyEnum.from_string('item1')
	_ = amod.MyEnum.from_string('item1')
	_ = amod.MyStruct.from_string('item1')
}
