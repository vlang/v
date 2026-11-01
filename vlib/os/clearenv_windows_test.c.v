module os

fn clearenv_windows_test_set(name string, value string) {
	wide_name := name.to_wide()
	wide_value := value.to_wide()
	defer {
		unsafe { free(voidptr(wide_name)) }
		unsafe { free(voidptr(wide_value)) }
	}
	assert C.SetEnvironmentVariableW(wide_name, wide_value) != 0
}

fn clearenv_windows_test_snapshot() map[string]string {
	block := C.GetEnvironmentStringsW()
	assert block != unsafe { nil }
	defer { C.FreeEnvironmentStringsW(block) }
	return clearenv_windows_environment(block)
}

fn test_clearenv_windows_removes_win32_only_and_hidden_entries() {
	saved := clearenv_windows_test_snapshot()
	defer {
		clearenv()
		for name, value in saved {
			if !name.starts_with('=') {
				setenv(name, value, true)
			}
			// The native setter also restores empty values and hidden names.
			clearenv_windows_test_set(name, value)
		}
	}
	// Initialize the CRT environment before adding variables through Win32 alone.
	setenv('V_TEST_CLEARENV_CRT', 'cached', true)
	crt_only_name := 'V_TEST_CLEARENV_CRT'.to_wide()
	assert C.SetEnvironmentVariableW(crt_only_name, unsafe { nil }) != 0
	unsafe { free(voidptr(crt_only_name)) }
	assert getenv('V_TEST_CLEARENV_CRT') == 'cached'
	assert 'V_TEST_CLEARENV_CRT' !in environ()
	unsetenv('V_TEST_CLEARENV_NATIVE')
	clearenv_windows_test_set('V_TEST_CLEARENV_NATIVE', 'native only')
	clearenv_windows_test_set('V_TEST_CLEARENV_NATIVE_EMPTY', '')
	clearenv_windows_test_set('V_TEST_CLEARENV_世界', '広い値')
	clearenv_windows_test_set('=V:', 'V:\\clearenv-test')
	assert getenv_opt('V_TEST_CLEARENV_NATIVE') == none
	assert environ()['V_TEST_CLEARENV_NATIVE'] == 'native only'
	assert 'V_TEST_CLEARENV_NATIVE_EMPTY' in environ()
	assert '=V:' in clearenv_windows_test_snapshot()
	clearenv()
	assert environ().len == 0
	assert clearenv_windows_test_snapshot().len == 0
	assert getenv_opt('V_TEST_CLEARENV_CRT') == none
}
