module os

struct ExpandEnvSavedValue {
	name  string
	value ?string
}

fn set_expand_env_test_values(values map[string]string) []ExpandEnvSavedValue {
	mut saved := []ExpandEnvSavedValue{}
	for name, value in values {
		saved << ExpandEnvSavedValue{name, getenv_opt(name)}
		setenv(name, value, true)
	}
	return saved
}

fn restore_expand_env_test_values(saved []ExpandEnvSavedValue) {
	for entry in saved {
		if value := entry.value {
			setenv(entry.name, value, true)
		} else {
			unsetenv(entry.name)
		}
	}
}

fn test_expand_env() {
	saved := set_expand_env_test_values({
		'V_TEST_EXPAND': 'hello'
	})
	defer { restore_expand_env_test_values(saved) }
	assert expand_env('\$V_TEST_EXPAND') == 'hello'
	assert expand_env('\${V_TEST_EXPAND}') == 'hello'
	assert expand_env('x=\$V_TEST_EXPAND!') == 'x=hello!'
	assert expand_env('\$V_TEST_EXPAND/\${V_TEST_EXPAND}') == 'hello/hello'
	assert expand_env('no vars') == 'no vars'
	assert expand_env('') == ''
}

fn test_expand_env_missing_empty_and_nonrecursive_values() {
	saved := set_expand_env_test_values({
		'V_TEST_EXPAND_MISSING': ''
		'V_TEST_EXPAND_EMPTY':   ''
		'V_TEST_EXPAND_LITERAL': '\$V_TEST_EXPAND_MISSING'
	})
	defer { restore_expand_env_test_values(saved) }
	unsetenv('V_TEST_EXPAND_MISSING')
	assert expand_env('a\$V_TEST_EXPAND_MISSING/b') == 'a/b'
	assert expand_env('a\${V_TEST_EXPAND_EMPTY}b') == 'ab'
	assert expand_env('\$V_TEST_EXPAND_LITERAL') == '\$V_TEST_EXPAND_MISSING'
}

fn test_expand_env_special_and_numeric_names() {
	saved := set_expand_env_test_values({
		'1':  'one'
		'10': 'ten'
		'*':  'star'
		'#':  'count'
		'$':  'dollar'
		'@':  'args'
		'!':  'bang'
		'?':  'status'
		'-':  'flags'
	})
	defer { restore_expand_env_test_values(saved) }
	assert expand_env('\$10') == 'one0'
	assert expand_env('\${10}') == 'ten'
	assert expand_env('\$1suffix') == 'onesuffix'
	assert expand_env('\$*\$#\$\$\$@\$!\$?\$-') == 'starcountdollarargsbangstatusflags'
	assert expand_env('\${*}\${#}\${\$}\${@}\${!}\${?}\${-}') == 'starcountdollarargsbangstatusflags'
}

fn test_expand_env_malformed_braces_and_literal_dollars() {
	assert expand_env('before\${}after') == 'beforeafter'
	assert expand_env('before\${after') == 'beforeafter'
	assert expand_env('\${') == ''
	assert expand_env('\$') == '\$'
	assert expand_env('a\$ b') == 'a\$ b'
	assert expand_env('\$+\$%\$}') == '\$+\$%\$}'
}

fn test_expand_env_braced_names_and_unicode() {
	saved := set_expand_env_test_values({
		'V_TEST_EXPAND_WITH-DASH': 'dash'
		'V_TEST_EXPAND_Büro':      'gebäude'
	})
	defer { restore_expand_env_test_values(saved) }
	assert expand_env('\${V_TEST_EXPAND_WITH-DASH}') == 'dash'
	assert expand_env('前\${V_TEST_EXPAND_Büro}後') == '前gebäude後'
	assert expand_env('\$界') == '\$界'
}

fn test_expand_env_does_not_truncate_names_at_nul() {
	saved := set_expand_env_test_values({
		'V_TEST_EXPAND_NUL': 'prefix value'
	})
	defer { restore_expand_env_test_values(saved) }
	assert expand_env('\${V_TEST_EXPAND_NUL\0suffix}') == ''
}
