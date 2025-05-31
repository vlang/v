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
	assert foo.field_1 is int && foo.field_1 == 0
	assert foo.field_2 is string && foo.field_2 == ''
	assert foo.field_3 is Type0 && foo.field_3 == ''
	assert foo.field_3 is Type0 && foo.field_3 == Type0('')

	// TODO: uncomment until the C backend is improved
	// assert foo.field_4 is Type3 && foo.field_4 == Type3(Type0(''))
	// assert foo.field_5 is Type4 && foo.field_4 == Type4(Type3(Type0('')))
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
	assert f0.field_1 is string && f0.field_1 == 'world'
	assert f0.field_2 is int && f0.field_2 == 100
	assert f0.field_3 is int && f0.field_3 == 200
	assert f0.field_4 is f32 && f0.field_4 == 3.14
	assert f0.field_5 is bool && f0.field_5

	// test 1
	f1 := Foo{
		field_4: Type3(100)
		field_5: Type4(Type3(Type0('hello')))
	}
	assert f1.field_4 is Type3 && f1.field_4 == Type3(100)
	assert f1.field_5 is Type4 && f1.field_5 == Type4(Type3(Type0('hello')))
}

fn main() {
	basic_assertion()
	struct_with_default_values()
	struct_with_values()
}
