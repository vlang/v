module os

fn restore_clearenv_test_environment(saved map[string]string) {
	clearenv()
	for name, value in saved {
		setenv(name, value, true)
	}
}

fn test_clearenv_removes_all_variables() {
	saved := environ()
	defer { restore_clearenv_test_environment(saved) }
	names := ['V_TEST_CLEARENV_VALUE', 'V_TEST_CLEARENV_EMPTY', 'V_TEST_CLEARENV_Büro']
	setenv(names[0], 'value=with=equals', true)
	setenv(names[1], '', true)
	setenv(names[2], 'gebäude', true)
	clearenv()
	assert environ().len == 0
	for name in names {
		assert getenv_opt(name) == none
	}
}

fn test_clearenv_is_idempotent_and_environment_can_be_reused() {
	saved := environ()
	defer { restore_clearenv_test_environment(saved) }
	clearenv()
	clearenv()
	assert environ().len == 0
	setenv('V_TEST_CLEARENV_REUSE', 'after clear', true)
	assert getenv('V_TEST_CLEARENV_REUSE') == 'after clear'
	assert environ().len == 1
	clearenv()
	assert environ().len == 0
}

fn test_clearenv_unset_removes_the_environment_entry() {
	name := 'V_TEST_CLEARENV_UNSET'
	saved := getenv_opt(name)
	defer {
		if value := saved {
			setenv(name, value, true)
		} else {
			unsetenv(name)
		}
	}
	setenv(name, 'temporary', true)
	unsetenv(name)
	assert name !in environ()
}
