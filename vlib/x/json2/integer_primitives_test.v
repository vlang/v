import x.json2

struct IntegerValues {
mut:
	ux8  u8
	ux16 u16
	ux32 u32
	ux64 u64
	sx8  i8
	sx16 i16
	sx32 int
	sx64 i64
}

fn test_all_primitive_integer_types_are_encodable_and_decodable() {
	f := IntegerValues{1, 2, 3, 4, -1, -2, -3, -4}
	s := json2.encode[IntegerValues](f)
	assert s == '{"ux8":1,"ux16":2,"ux32":3,"ux64":4,"sx8":-1,"sx16":-2,"sx32":-3,"sx64":-4}'
	x := json2.decode[IntegerValues](s)!
	assert x == f
	println('done')
}
