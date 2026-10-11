import callbacks

fn error_callback(path string, err IError) int {
	assert path == 'entry'
	assert err.msg() == 'unavailable'
	return 42
}

fn test_imported_callback_accepts_builtin_ierror() {
	assert callbacks.invoke(error_callback) == 42
	assert callbacks.invoke(fn (path string, err IError) int {
		assert path == 'entry'
		assert err.msg() == 'unavailable'
		return 24
	}) == 24
	assert callbacks.invoke_inline(fn (path string, err IError) int {
		assert path == 'inline'
		assert err.msg() == 'missing'
		return 17
	}) == 17
}
