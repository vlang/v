pub struct Optional<T> {
mut:
	value T
	some bool
}

pub fn new_some<T>(value T) Optional<T> {
	return {value: value, some: true}
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

fn test_generics_return_generics_struct() {
	mut o := new_some<int>(23)
	println(some<int>(o))
	assert some<int>(o) == true
	set<int>(mut o, 42)
	println(get<int>(o))
	assert get<int>(o) == 42
}
