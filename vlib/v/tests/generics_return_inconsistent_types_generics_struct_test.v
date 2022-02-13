// test generics function that return generics struct
pub struct Optional<T> {
mut:
	value T
	some  bool
	typ   string
}

pub fn new_some<T, B>(value T, b B) Optional<T> {
	return Optional<T>{
		value: value
		some: true
		typ: typeof(b).name
	}
}

pub fn some<T>(opt Optional<T>) bool {
	return opt.some
}

pub fn get_value<T>(opt Optional<T>) T {
	return opt.value
}

pub fn get_typ<T>(opt Optional<T>) string {
	return opt.typ
}

pub fn set<T, B>(mut opt Optional<T>, value T, b B) {
	opt.value = value
	opt.some = true
	opt.typ = typeof(b).name
}

fn test_inconsistent_types_generics_fn_return_generics_struct() {
	mut o := new_some<int, string>(23, 'aaa')
	println(some<int>(o))
	assert some<int>(o) == true

	println(get_value<int>(o))
	assert get_value<int>(o) == 23

	set<int, string>(mut o, 42, 'aaa')
	println(get_value<int>(o))
	assert get_value<int>(o) == 42

	println(get_typ<int>(o))
	assert get_typ<int>(o) == 'string'
}

// test generics method that return generics struct
pub struct Foo {
	foo int
}

pub fn (f Foo) new_some<T, B>(value T, b B) Optional<T> {
	return Optional<T>{
		value: value
		some: true
		typ: typeof(b).name
	}
}

pub fn (f Foo) some<T>(opt Optional<T>) bool {
	return opt.some
}

pub fn (f Foo) get_value<T>(opt Optional<T>) T {
	return opt.value
}

pub fn (f Foo) get_typ<T>(opt Optional<T>) string {
	return opt.typ
}

pub fn (f Foo) set<T, B>(mut opt Optional<T>, value T, b B) {
	opt.value = value
	opt.some = true
	opt.typ = typeof(b).name
}

fn test_inconstent_types_generics_method_return_generics_struct() {
	foo := Foo{}
	mut o := foo.new_some<int, string>(23, 'aaa')
	println(foo.some<int>(o))
	assert foo.some<int>(o) == true

	println(foo.get_value<int>(o))
	assert foo.get_value<int>(o) == 23

	foo.set<int, string>(mut o, 42, 'aaa')
	println(foo.get_value<int>(o))
	assert foo.get_value<int>(o) == 42

	println(foo.get_typ<int>(o))
	assert foo.get_typ<int>(o) == 'string'
}
