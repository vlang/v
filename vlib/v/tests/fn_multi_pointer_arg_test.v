

fn multi_pointer_arg(mut e &int) {
	assert typeof(e).name == '&&int'
}

fn multi_pointer_arg_test() {
	mut i := 100
	mut pi := &i
	multi_pointer_arg(mut &pi)
}