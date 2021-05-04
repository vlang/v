// test generics function that return generics struct
pub struct Optional<T> {
mut:
	value T
	some  bool
}

pub fn new_some<T>(value T) Optional<T> {
	return {
		value: value
		some: true
	}
}

pub fn some<T>(opt Optional<T>) bool {
	return opt.some
}

pub fn get<T>(opt Optional<T>) T {
	return opt.value
}

pub fn set<T>(mut opt Optional<T>, value T) {
	opt.value = value
	opt.some = true
}

fn test_generics_fn_return_generics_struct() {
	mut o := new_some<int>(23)
	println(some<int>(o))
	assert some<int>(o) == true
	set<int>(mut o, 42)
	println(get<int>(o))
	assert get<int>(o) == 42
}

// test generics method that return generics struct
pub struct Foo {
	foo int
}

pub fn (f Foo) new_some<T>(value T) Optional<T> {
	return {
		value: value
		some: true
	}
}

pub fn (f Foo) some<T>(opt Optional<T>) bool {
	return opt.some
}

pub fn (f Foo) get<T>(opt Optional<T>) T {
	return opt.value
}

pub fn (f Foo) set<T>(mut opt Optional<T>, value T) {
	opt.value = value
	opt.some = true
}

fn test_generics_method_return_generics_struct() {
	foo := Foo{}
	mut o := foo.new_some<int>(23)
	println(foo.some<int>(o))
	assert foo.some<int>(o) == true
	foo.set<int>(mut o, 42)
	println(foo.get<int>(o))
	assert foo.get<int>(o) == 42
}

// test genrics struct str()
pub struct ArrayIterator<T> {
	data []T
mut:
	index int
}

pub fn iter<T>(arr []T) ArrayIterator<T> {
	return ArrayIterator{
		data: arr
		index: 11
	}
}

fn test_generics_with_generics_struct_string() {
	data := ['foo', 'bar']
	it := iter<string>(data)
	println(it)
	ret := '$it'
	assert ret.contains('ArrayIterator<string>{')
	assert ret.contains("data: ['foo', 'bar']")
	assert ret.contains('index: 11')
}

fn test_generics_struct_insts_to_concrete() {
	ai := ArrayIterator<int>{
		data: [11, 22]
		index: 22
	}
	println(ai)
	ret := '$ai'
	assert ret.contains('ArrayIterator<int>{')
	assert ret.contains('data: [11, 22]')
	assert ret.contains('index: 22')
}

struct Iterator<T> {
	data []T
}

pub fn (mut i Iterator<T>) next<T>() ?T {
	return i.data[0]
}

pub fn iter_data<T>(data []T) Iterator<T> {
	return Iterator{
		data: data
	}
}

fn test_generics_return_generic_struct_from_fn() {
	mut it := iter_data<int>([1, 2, 3])
	println(it.next())
	assert '$it.next()' == 'Option(1)'
}
