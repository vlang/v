module another_module

pub struct SomeStruct {
pub mut:
	x int
	y int
	z int
}

pub fn (s SomeStruct) some_method() int {
	return 999 + s.x + s.y + s.z
}
