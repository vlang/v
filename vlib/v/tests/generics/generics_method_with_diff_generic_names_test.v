// for issue: 20363
// The generic name used by the methods and receiver is different from the generic name defined by the struct.
struct Foo[T] {
	field T
}

fn new_foo[U](arg U) Foo[U] {
	return Foo[U]{arg}
}

fn test_using_diff_generic_names() {
	foo := new_foo(1)
	foo.method_1('hello')
	foo.method_2('hello')
}

fn (f Foo[U]) method_1(arg string) {
	assert arg == 'hello'
}

fn (f Foo[U]) method_2[U](arg string) {
	assert arg == 'hello'
}

// for issue: 20365
// The generic name used by the `str()` method and receiver is different from the generic name defined by the struct.
fn (f Foo[U]) str[U]() string {
	return 'hello'
}

fn test_str_override() {
	foo := new_foo(1)
	assert '${foo}' == 'hello'
}
