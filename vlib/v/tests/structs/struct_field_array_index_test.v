struct App {
mut:
	buffer []string
}

fn test_struct_field_array_index() {
	mut app := &App{
		buffer: []string{len: 2}
	}

	app.buffer[0] += 'hello'
	app.buffer[1] += 'world'

	println(app)

	assert app.buffer == ['hello', 'world']
}
