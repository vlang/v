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

interface MessagePayload {
	msg() string
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

fn ierror_payload() !IError {
	return ReproError{
		message: 'ierror payload'
	}
}

fn message_payload() !MessagePayload {
	return ReproError{
		message: 'message payload'
	}
}

fn test_result_error_can_be_returned_from_option_interface_or_block() {
	mut app := App{}
	app.get_something() or {
		assert err.msg() == 'no instance of something'
		return
	}
	assert false
}

fn test_result_interface_payload_is_checked_before_ierror_boxing() {
	ierror_value := ierror_payload() or {
		assert false, 'IError payload was returned as an error: ${err}'
		return
	}
	assert ierror_value.msg() == 'ierror payload'

	message_value := message_payload() or {
		assert false, 'interface payload was returned as an error: ${err}'
		return
	}
	assert message_value.msg() == 'message payload'
}
