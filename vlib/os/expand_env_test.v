module os

fn test_expand_env() {
	setenv('V_TEST_EXPAND', 'hello', true)
	assert expand_env('\$V_TEST_EXPAND') == 'hello'
	assert expand_env('\${V_TEST_EXPAND}') == 'hello'
	assert expand_env('x=\$V_TEST_EXPAND!') == 'x=hello!'
	assert expand_env('no vars') == 'no vars'
	unsetenv('V_TEST_EXPAND')
}
