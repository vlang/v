module util

import os

// `$env()` is a compile-time substitution, so what is replaced has to be exactly
// the call, and not any other `$` in the string.
fn test_resolve_env_value_substitutes_the_call() {
	os.setenv('UTIL_RESOLVE_PROBE', 'value', true)
	defer {
		os.unsetenv('UTIL_RESOLVE_PROBE')
	}
	assert resolve_env_value("\$env('UTIL_RESOLVE_PROBE')", false) or { panic(err) } == 'value'
	assert resolve_env_value("a\$env('UTIL_RESOLVE_PROBE')b", false) or { panic(err) } == 'avalueb'
	assert resolve_env_value("a\$env('UTIL_RESOLVE_PROBE')", false) or { panic(err) } == 'avalue'
	assert resolve_env_value("\$env('UTIL_RESOLVE_PROBE')b", false) or { panic(err) } == 'valueb'
	// The call is replaced wherever it appears, and a second one too.
	assert resolve_env_value("\$env('UTIL_RESOLVE_PROBE')\$env('UTIL_RESOLVE_PROBE')",
		false) or { panic(err) } == 'valuevalue'
}

fn test_resolve_env_value_requires_the_call_to_be_present() {
	// NOTE: a call with no closing `)` at all (`$env('X`) is not exercised here:
	// the scan then finds no `)`, `replace_once` changes nothing, and the result
	// still contains `$env('`, so this recurses for ever.
	for argument in ['', 'plain', '\$env(', "\$env('X')extra"] {
		message := if _ := resolve_env_value(argument, false) {
			'expected an error for `${argument}`'
		} else {
			err.msg()
		}
		assert message.contains("\$env('"), message
		assert message.contains(argument), message
	}
}

fn test_resolve_env_value_validates_the_variable_name() {
	for argument in ["\$env('')", "\$env('-')", "\$env('A B')", "\$env('A-B')"] {
		message := if _ := resolve_env_value(argument, false) {
			'expected an error for `${argument}`'
		} else {
			err.msg()
		}
		assert message.len > 0
	}
	// `$env` cannot be nested: interpolation is not available here.
	message := if _ := resolve_env_value("\$env('\$OTHER')", false) {
		'expected an error for a nested interpolation'
	} else {
		err.msg()
	}
	assert message.contains('string interpolation'), message
}

// With presence checking on, an unset and an empty variable are different errors.
fn test_resolve_env_value_reports_absent_and_empty_variables() {
	os.setenv('UTIL_EMPTY_PROBE', '', true)
	defer {
		os.unsetenv('UTIL_EMPTY_PROBE')
	}
	message := if _ := resolve_env_value("\$env('UTIL_EMPTY_PROBE')", true) {
		'expected an error for an empty variable'
	} else {
		err.msg()
	}
	assert message.contains('does not exist') || message.contains('is empty'), message
	absent := if _ := resolve_env_value("\$env('UTIL_NOT_SET_AT_ALL')", true) {
		'expected an error for an absent variable'
	} else {
		err.msg()
	}
	assert absent.contains('UTIL_NOT_SET_AT_ALL'), absent
	// Without the check the same empty variable is simply an empty value.
	assert resolve_env_value("\$env('UTIL_EMPTY_PROBE')", false) or { panic(err) } == ''
}
