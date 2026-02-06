struct Client {}

fn add_handler[T](handler fn (mut Client, T)) string {
	return typeof(handler).name
}

fn on_message(mut client Client, event string) {
	println(event)
}

fn test_generics_fn_typeof_name() {
	ret := add_handler[string](on_message)
	println(ret)
	assert ret == 'fn (mut Client, string)'
}

// test no paras generics fn typeof name
struct Test1 {}

struct Test2 {}

fn print_type[T]() string {
	name := T.name
	println(name)
	return name
}

fn test_no_paras_generics_fn_typeof_name() {
	mut ret := print_type[Test1]()
	assert ret == 'Test1'

	ret = print_type[Test2]()
	assert ret == 'Test2'

	ret = print_type[int]()
	assert ret == 'int'

	ret = print_type[f32]()
	assert ret == 'f32'

	ret = print_type[bool]()
	assert ret == 'bool'
}

// test generic method receiver typeof name
struct Num[T] {
	num T
}

fn (num Num[T]) test(v T) {
	println(typeof(num).name)
	assert typeof(num).name == 'Num[int]'
	println(typeof(v).name)
	assert typeof(v).name == 'int'
}

fn test_generic_method_receiver_typeof_name() {
	num := Num[int]{3}
	num.test(100)
}
