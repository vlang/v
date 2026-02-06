module main

$if @MOD == 'main' {
	const c1 = 'main'
} $else {
	const c1 = 'other'
}

$if @OS == 'linux' {
	const os = 'linux'
} $else $if @OS == 'windows' {
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
	$if @FN == 'test_comptime_if_at_expr' {
		assert true
	} $else {
		assert false
	}
}
