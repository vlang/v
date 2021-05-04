struct Client {}

fn add_handler<T>(handler fn (mut Client, T)) string {
	return typeof(handler).name
}

fn on_message(mut client Client, event string) {
	println(event)
}

fn test_generics_fn_typeof_name() {
	ret := add_handler<string>(on_message)
	println(ret)
	assert ret == 'fn (mut Client, string)'
}
