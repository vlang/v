struct ReproError {
	message string
}

fn (err ReproError) msg() string {
	return err.message
}

fn (err ReproError) code() int {
	return 0
}

interface Something {
	create() !
}

struct App {
mut:
	something ?&Something
}

fn (mut app App) get_something() !&Something {
	mut something := app.something or {
		return ReproError{
			message: 'no instance of something'
		}
	}

	return something
}

fn test_result_error_can_be_returned_from_option_interface_or_block() {
	mut app := App{}
	app.get_something() or {
		assert err.msg() == 'no instance of something'
		return
	}
	assert false
}
