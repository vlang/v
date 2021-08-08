fn test_fn_mut_arg_of_array() {
	mut a := App{}
	a.data << 1
	a.do_something()
	assert a.data.len == 2
}

struct App {
pub mut:
	data []int
}

fn (mut a App) do_something() {
	assert a.data.len == 1
	mut p := Proc{}
	p.make_a(mut a.data)
	assert a.data.len == 2
}

struct Proc {}

fn (mut p Proc) make_a(mut data []int) {
	data << 2
}
