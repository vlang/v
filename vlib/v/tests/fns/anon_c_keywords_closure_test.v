fn test_main() {
	mut stderr := 0
	mut stdout := 0
	mut stdin := fn () {}

	causes_error := fn [mut stderr, mut stdin, mut stdout] () int {
		stderr = 0
		stdin()
		stdout = 2
		assert stderr == 0
		assert stdout == 2
		return stderr + stdout
	}
	assert causes_error() == 2
	assert voidptr(C.stdin) != unsafe { nil }
	assert voidptr(C.stdout) != unsafe { nil }
	assert voidptr(C.stderr) != unsafe { nil }
}
