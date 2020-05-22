import dl

fn test_dl() {
	$if linux {
		run_test_invalid_lib_linux()
		return
	}
	$if windows {
		run_test_invalid_lib_windows()
		run_test_valid_lib_windows()
		run_test_invalid_sym_windows()
		run_test_valid_sym_windows()
		return
	} $else {
		eprint('currently not implemented on this platform')
	}
}

fn run_test_invalid_lib_linux() {
	// ensure a not-existing dl won't be loaded
	h := dl.open('not-existing-dynamic-link-library', dl.rtld_now)
	assert h == 0
}

fn run_test_invalid_lib_windows() {
	// ensure a not-existing dl won't be loaded
	h := dl.open('not-existing-dynamic-link-library', dl.rtld_now)
	assert h == 0
}

fn run_test_valid_lib_windows() {
	h := dl.open('shell32', dl.rtld_now)
	assert h != 0
}

fn run_test_invalid_sym_windows() {
	h := dl.open('shell32', dl.rtld_now)
	proc := dl.sym(h, 'CommandLineToArgvW2')
	assert proc == 0
}

fn run_test_valid_sym_windows() {
	h := dl.open('shell32', dl.rtld_now)
	proc := dl.sym(h, 'CommandLineToArgvW')
	assert proc != 0
}
