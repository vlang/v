const enum_value = $if linux { 1 } $else { 2 }

pub enum Test {
	a = enum_value
}

fn test_enum_with_comptime_const() {
	println(Test.a)
	assert true
}
