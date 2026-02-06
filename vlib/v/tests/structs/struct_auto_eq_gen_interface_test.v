interface IExample {
	thing() bool
}

struct Foo {}

fn (n Foo) thing() bool {
	return true
}

struct Test {
	a IExample
}

fn new() Test {
	return Test{Foo{}}
}

fn test_struct_auto_eq_gen_interface_case() {
	w1 := new()
	w2 := new()
	assert w1 == w2
}

// For https://github.com/vlang/v/issues/16074
pub interface Logger {
mut:
	info(s string)
}

struct LogContainer {
	name string
mut:
	logger &Logger = unsafe { nil }
}

fn test_comparing_struct_with_pointers_to_interface_values_autogenerates_working_eq_method() {
	assert LogContainer{
		name: 'abc'
	} == LogContainer{
		name: 'abc'
	}
}
