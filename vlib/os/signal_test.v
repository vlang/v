import os

fn former_handler(signal os.Signal) {
	exit(0)
}

fn default_handler(signal os.Signal) {
	exit(0)
}

fn test_signal_opt() {
	os.signal_opt(.int, default_handler) or { assert false }
}

fn test_signal_opt_invalid_argument() {
	// Can't register a signal on SIGKILL
	if _ := os.signal_opt(.kill, default_handler) {
		assert false
	}
	os.signal_opt(.kill, default_handler) or {
		assert err.msg == 'Invalid argument'
		assert err.code == 22
	}
}

fn test_signal_opt_return_former_handler() {
	func1 := os.signal_opt(.term, former_handler) or { panic('unexpected error') }
	assert isnil(func1)
	func2 := os.signal_opt(.term, default_handler) or { panic('unexpected error') }
	assert func2 == former_handler
}
