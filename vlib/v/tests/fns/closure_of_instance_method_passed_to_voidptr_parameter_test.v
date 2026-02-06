type AudioCallback = fn (buffer voidptr, frames u32)

@[heap]
struct App {
mut:
	id int
}

fn (mut app App) callback(buffer voidptr, frames u32) {
	assert voidptr(app) != unsafe { nil }
	assert buffer != unsafe { nil }
	assert frames == 256

	assert app.id == 12345
	app.id = 98765

	unsafe {
		p := &u8(buffer)
		assert p[0] == 59
		assert p[1] == 58
		assert p[2] == 51
		assert p[3] == 52
		p[0] = 12
		p[1] = 22
		p[2] = 32
		p[3] = 42
	}
}

fn set_audio_callback(p voidptr) {
	println(p)
	cb := AudioCallback(p)
	mut buf := [1024]u8{}
	buf[0] = 59
	buf[1] = 58
	buf[2] = 51
	buf[3] = 52
	cb(&buf[0], 256)
	assert buf[0] == 12
	assert buf[1] == 22
	assert buf[2] == 32
	assert buf[3] == 42
}

fn test_creating_a_closure_from_instance_method_through_explicit_cast_works() {
	mut app := &App{
		id: 12345
	}
	audio_closure := AudioCallback(app.callback)
	set_audio_callback(audio_closure)
	assert app.id == 98765
}

fn test_creating_a_closure_from_instance_method_passed_to_voidptr_fn_parameter_works() {
	mut app := &App{
		id: 12345
	}
	// TODO: the code below should work, but does not yet, because it just passes the fn address
	// of the method implementation itself, without creating a closure to wrap the instance.
	// set_audio_callback(app.callback)
	// assert app.id == 98765
}
