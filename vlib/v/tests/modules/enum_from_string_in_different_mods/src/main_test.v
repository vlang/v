module main

import mod { MyEnum }
import mod2

fn test_main() {
	item1 := MyEnum.from_string('item1')?
	assert item1 == MyEnum.item1
	item2 := mod.MyEnum.from_string('item2')?
	assert item2 == MyEnum.item2
	item2_2 := mod2.MyEnum.from_string('item2')?
	assert item2_2 == mod2.MyEnum.item2
}
