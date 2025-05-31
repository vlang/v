struct MyStruct {
	text string
}

struct App {}

fn pre_send[T, N](app T, params N) {
	send(params, app)
}

fn send[T, N](params T, app N) { // app now is second argument
	println(params)
}

fn test_main() {
	params := MyStruct{'hello'}
	app := App{}

	pre_send(app, params)
	assert true
}
