import time

fn test_match_glob_on_empty_string() {
	assert ''.match_glob('')
	assert !''.match_glob('x')
}

fn test_match_glob_on_x() {
	assert !'x'.match_glob('')
	assert 'x'.match_glob('x')
	assert 'xxx'.match_glob('*x')
	assert 'xxx'.match_glob('x*')
}

fn test_match_glob_on_abc() {
	assert !'abc'.match_glob('')
	assert 'abc'.match_glob('*')
	//
	assert !'abc'.match_glob('ab')
	assert 'abc'.match_glob('abc')
	assert 'abc'.match_glob('abc*')
	//
	assert 'abc'.match_glob('*c')
	assert !'abc'.match_glob('*b')
	assert 'abc'.match_glob('*bc')
	assert 'abc'.match_glob('*abc')
	//
	assert 'abc'.match_glob('a*')
	assert !'abc'.match_glob('b*')
	assert 'abc'.match_glob('a*c')
	//
	assert 'abc'.match_glob('ab?')
	assert 'abc'.match_glob('a??')
	assert 'abc'.match_glob('???')
	assert !'abc'.match_glob('??')
	assert !'abc'.match_glob('?')
}

fn test_match_glob_on_a() {
	assert 'a'.match_glob('a')
	assert 'a'.match_glob('?')
	assert !'a'.match_glob('??')
	assert 'a'.match_glob('*')
	assert 'a'.match_glob('a*')
	assert 'a'.match_glob('*a')
}

fn test_match_glob_with_any_charset_patterns() {
	assert 'axbxcxdxe'.match_glob('*c[xyz]d*')
	assert 'axbxcxdxe'.match_glob('*c[yxz]d*')
	assert 'axbxcxdxe'.match_glob('*c[zyx]d*')
	//
	assert 'axbxcxdxe'.match_glob('*dx[QeW]')
	assert 'axbxcxdxe'.match_glob('*dx[QeW]*')
	//
	assert !'axbxcxdxe'.match_glob('*bx[QcW]')
	assert 'axbxcxdxe'.match_glob('*bx[QcW]*')
	//
	assert !'axbxcxdxe'.match_glob('*zx[QeW]')
	assert !'axbxcxdxe'.match_glob('*zx[QeW]*')
}

fn test_match_glob_with_none_of_charset_patterns() {
	assert 'axbxcxdxe'.match_glob('*c[^XYZ]d*')
	assert !'axbxcxdxe'.match_glob('*c[^xYZ]d*')
	assert !'axbxcxdxe'.match_glob('*c[^YxZ]d*')
	assert !'axbxcxdxe'.match_glob('*c[^YZx]d*')
}

fn test_match_glob_with_escaped_metachars() {
	assert 'axbx?cxdxe'.match_glob('*x[?]c*')
	assert !'axbxXcxdxe'.match_glob('*x[?]c*')
	assert 'zaxbx*cxdxez'.match_glob('*x[Q*W]c*')
	assert 'zaxbx*cxdxez'.match_glob('*x[QW*]c*')
	assert 'zaxbx*cxdxez'.match_glob('*bx[*QW]c*')
	assert 'zaxbW*cxdxez'.match_glob('*W[*nmk]c*')
	assert 'zaxbW*cxdxez'.match_glob('*W[n*mk]c*')
	assert 'zaxbW*cxdxez'.match_glob('*W[nm*k]c*')
	assert 'zaxbW*cxdxez'.match_glob('*W[nmk*]c*')
}

fn test_match_glob_with_complex_patterns() {
	assert 'axbxcxdxe'.match_glob('*xdx*')
	assert !'axbxcxdxe'.match_glob('*xzx*')
	assert 'axbxcxdxe'.match_glob('a*b*c*d*e*')
	assert 'axbxcxdxexxx'.match_glob('a*b*c*d*e*')
	assert 'abxbbxdbxebxczzx'.match_glob('a*b?c*x')
	assert !'abxbbxdbxebxczzy'.match_glob('a*b?c*x')
}

fn test_match_glob_search_is_linear() {
	// Note: these are pathological cases, when matches are performed
	// using the exponential recursive approach, that can take many
	// seconds, even minutes, but take usually only microseconds,
	// using the linear approach from https://research.swtch.com/glob
	// that does not backtrack.
	long_a := 'a'.repeat(500)
	sw := time.new_stopwatch()
	assert !long_a.match_glob('a*a*a*a*b')
	assert sw.elapsed().milliseconds() < 10
	assert !long_a.match_glob('a*a*a*a*a*a*a*a*a*b')
	assert sw.elapsed().milliseconds() < 10
}
