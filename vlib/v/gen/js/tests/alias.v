type Type0 = string
type Type1 = int | string
type Type2 = string | int
type Type3 = Type0 | int
type Type4 = Type3 | Type1 | f32
type Type5 = Type4 | bool

struct Foo {
	field_0 Type0
	field_1 Type1
	field_2 Type2
	field_3 Type3
	field_4 Type4
	field_5 Type5
}

fn basic_assertion() {
	foo := Type1('')
	assert foo == Type1('')
}

fn struct_with_default_values() {
	foo := Foo{}

	assert foo.field_0 == ''

	if foo.field_1 is int {
		assert foo.field_1 == 0
	} else {
		assert false
	}

	if foo.field_2 is string {
		assert foo.field_2 == ''
	} else {
		assert false
	}

	if foo.field_3 is Type0 {
		assert foo.field_3 == ''
		assert foo.field_3 == Type0('')
	} else {
		assert false
	}

	// TODO: uncomment until the C backend is improved
	// if foo.field_4 is Type3 {
	// 	assert foo.field_4 == Type3(Type0(''))
	// } else {
	// 	assert false
	// }

	// TODO: uncomment until the C backend is improved
	// if foo.field_5 is Type4 {
	// 	assert foo.field_4 == Type4(Type3(Type0('')))
	// } else {
	// 	assert false
	// }
}

fn struct_with_values() {
	// test 0
	f0 := Foo{
		field_0: 'hello'
		field_1: 'world'
		field_2: 100
		field_3: 200
		field_4: f32(3.14)
		field_5: true
	}

	assert f0.field_0 == 'hello'

	if f0.field_1 is string {
		assert f0.field_1 == 'world'
	} else {
		assert false
	}

	if f0.field_2 is int {
		assert f0.field_2 == 100
	} else {
		assert false
	}

	if f0.field_3 is int {
		assert f0.field_3 == 200
	} else {
		assert false
	}

	if f0.field_4 is f32 {
		assert f0.field_4 == 3.14
	} else {
		assert false
	}

	if f0.field_5 is bool {
		assert f0.field_5
	} else {
		assert false
	}

	// test 1
	f1 := Foo{
		field_4: Type3(100)
		field_5: Type4(Type3(Type0('hello')))
	}

	if f1.field_4 is Type3 {
		assert f1.field_4 == Type3(100)
	} else {
		assert false
	}

	if f1.field_5 is Type4 {
		assert f1.field_5 == Type4(Type3(Type0('hello')))
	} else {
		assert false
	}
}

fn main() {
	basic_assertion()
	struct_with_default_values()
	struct_with_values()
}
