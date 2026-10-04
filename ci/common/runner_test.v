module common

fn test_ci_argv_uses_the_checkout_v() {
	assert ci_argv(['v', 'test', 'vlib'], '/vroot/v') == ['/vroot/v', 'test', 'vlib']
	assert ci_argv(['./v2', '-o', 'v3'], '/vroot/v') == ['./v2', '-o', 'v3']
}

fn test_ci_argv_runs_assignments_through_env() {
	assert ci_argv(['VJOBS=1', 'v', 'test-self'], '/vroot/v') == ['env', 'VJOBS=1', '/vroot/v',
		'test-self']
}

fn test_ci_argv_keeps_an_explicit_env_program() {
	assert ci_argv(['env', 'VJOBS=18', 'v', '-nocache', 'test-self'], '/vroot/v') == [
		'env',
		'VJOBS=18',
		'/vroot/v',
		'-nocache',
		'test-self',
	]
	assert ci_argv(['env', 'UBSAN_OPTIONS=x', './v2', '-o', 'v.c'], '/vroot/v') == [
		'env',
		'UBSAN_OPTIONS=x',
		'./v2',
		'-o',
		'v.c',
	]
}
