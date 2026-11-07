module os

fn C.clearenv() i32

fn test_clearenv_after_libc_clears_the_environment() {
	saved := environ()
	defer {
		for name, value in saved {
			setenv(name, value, true)
		}
	}
	assert C.clearenv() == 0
	// libc may represent the empty environment with a null environ pointer.
	clearenv()
	setenv('V_TEST_CLEARENV_AFTER_LIBC', 'reused', true)
	assert getenv('V_TEST_CLEARENV_AFTER_LIBC') == 'reused'
	unsetenv('V_TEST_CLEARENV_AFTER_LIBC')
}
