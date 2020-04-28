import dl

fn test_dl() {
	$if linux {
		run_test_linux()
		return
	}
	$if windows {
		run_test_windows()
		return
	} $else {
		eprint('currently not implemented on this platform')
	}
}

fn run_test_linux() {
	// ensure a not-existing dl won't be loaded
	h := dl.open('not-existing-dynamic-link-library', dl.RTLD_NOW)
	// println('handle = $h')
	assert h == 0
}

fn run_test_windows() {
	// ensure a not-existing dl won't be loaded
	h := dl.open('not-existing-dynamic-link-library', dl.RTLD_NOW)
	// println('handle = $h')
	assert h == 0
}
