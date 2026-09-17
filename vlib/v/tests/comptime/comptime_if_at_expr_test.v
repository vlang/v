module main

$if 'main' == 'main' {
	const c1 = 'main'
} $else {
	const c1 = 'other'
}

$if 'macos' == 'linux' {
	const os = 'linux'
} $else $if 'macos' == 'windows' {
	const os = 'windows'
} $else {
	const os = 'other'
}

fn test_comptime_if_at_expr() {
	assert c1 == 'main'

	$if linux {
		assert os == 'linux'
	} $else $if windows {
		assert os == 'windows'
	} $else {
		assert os == 'other'
	}

	dump(@FN)
	$if 'test_comptime_if_at_expr' == 'test_comptime_if_at_expr' {
		assert true
	} $else {
		assert false
	}
}
