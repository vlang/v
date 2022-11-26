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

pub fn (mut x IntegerValues) from_json(f json2.Any) {
	fm := f.as_map()
	if v := fm['ux8'] {
		x.ux8 = u8(v.u64())
	}
	if v := fm['ux16'] {
		x.ux16 = u16(v.u64())
	}
	if v := fm['ux32'] {
		x.ux32 = u32(v.u64())
	}
	if v := fm['ux64'] {
		x.ux64 = v.u64()
	}
	//
	if v := fm['sx8'] {
		x.sx8 = i8(v.i64())
	}
	if v := fm['sx16'] {
		x.sx16 = i16(v.i64())
	}
	if v := fm['sx32'] {
		x.sx32 = int(v.i64())
	}
	if v := fm['sx64'] {
		x.sx64 = v.i64()
	}
}

fn test_all_primitive_integer_types_are_encodable_and_decodable() {
	f := IntegerValues{1, 2, 3, 4, -1, -2, -3, -4}
	s := json2.encode[IntegerValues](f)
	dump(s)
	assert s == '{"ux8":1,"ux16":2,"ux32":3,"ux64":4,"sx8":-1,"sx16":-2,"sx32":-3,"sx64":-4}'
	x := json2.decode[IntegerValues](s)!
	dump(x)
	assert x == f
	println('done')
}
