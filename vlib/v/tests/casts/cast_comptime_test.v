struct Document0 {
	a int
	b f64
	c string
	d bool
	e u8
	f u32
}

type Any = []Any
	| bool
	| f32
	| f64
	| i16
	| i64
	| i8
	| int
	| map[string]Any
	| string
	| u16
	| u32
	| u64
	| u8

fn raw_encode[T](data T) !map[string]Any {
	mut res := map[string]Any{}
	$for field in T.fields {
		x := data.$(field.name)
		res[field.name] = Any(x)
	}
	return res
}

fn test_main() {
	d := Document0{
		a: 1
		b: 1.1
		c: 'qwerty'
		d: false
		e: u8(0)
		f: u32(0)
	}
	map_data := raw_encode(d)!
	assert map_data['a']! as int == d.a
	assert map_data['b']! as f64 == d.b
	assert map_data['c']! as string == d.c
	assert map_data['d']! as bool == d.d
	assert map_data['e']! as u8 == d.e
	assert map_data['f']! as u32 == d.f
}
