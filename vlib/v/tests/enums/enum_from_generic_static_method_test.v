enum MyEnum {
	abc
	def
	lock
	xyz
	if
}

@[flag]
enum MyFlaggedEnum {
	abc
	def
	xyz
}

/*
fn dump_enum_values[R,T](list []T) {
	for input in list {
		// TODO: R.from() should work, when R is an enum, but it does not right now
		x := R.from(input) or {
			eprintln('>>>> error input: `${input}` | err: `${err}`')
			continue
		}
		eprintln('> input: ${input} | x: ${x}')
	}
	assert true
}
fn test_enums_conversion_using_from() {
	dump_enum_values[MyEnum,string](['abc', 'bbb', 'xyz', 'if', 'def', 'zzz'])
	dump_enum_values[MyFlaggedEnum,int]([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
}
*/

fn test_enum_from_string() {
	x := MyEnum.from('def')!
	dump(x)
	assert x == .def
	y := MyFlaggedEnum.from('xyz')!
	dump(y)
	assert y == .xyz
	assert MyEnum.from('if')! == MyEnum.if
	assert MyEnum.from('lock')! == MyEnum.lock
	if z := MyEnum.from('unknown') {
		assert false
	} else {
		assert err.msg() == 'invalid value'
	}
}

fn test_enum_from_integer() {
	x := MyEnum.from(3)!
	dump(x)
	assert x == .xyz
	y := MyFlaggedEnum.from(4)!
	dump(y)
	assert y == .xyz
	if z := MyFlaggedEnum.from(9999) {
		assert false
	} else {
		assert err.msg() == 'invalid value'
	}
}

@[flag]
enum Test {
	first
	second
	third
}

fn test_flagged_enum_from_0_and_empty_string() {
	z := Test.zero()
	dump(z)

	x := Test.from(0)!
	dump(x)
	assert x == z

	y := Test.from('')!
	dump(y)
	assert y == z
}
