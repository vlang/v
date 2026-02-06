import os
import time

fn test_getenv() {
	// VEXE is set by the V builtin test runner
	assert os.getenv('VEXE').len > 0
	assert os.getenv('PATH').len > 0
}

fn test_getenv_opt() {
	assert os.getenv_opt('VEXE') or { '' }.len > 0
}

fn test_setenv() {
	os.setenv('foo', 'bar', true)
	assert os.getenv('foo') == 'bar'
	// `setenv` should not set if `overwrite` is false
	os.setenv('foo', 'bar2', false)
	assert os.getenv('foo') == 'bar'
	// `setenv` should overwrite if `overwrite` is true
	os.setenv('foo', 'bar2', true)
	assert os.getenv('foo') == 'bar2'
	assert os.getenv_opt('foo') or { '' } == 'bar2'
}

fn test_unsetenv() {
	os.setenv('foo', 'bar', true)
	os.unsetenv('foo')
	assert os.getenv('foo') == ''
}

fn test_environ() {
	os.setenv('myvar1', 'bar1', true)
	os.setenv('myvar2', 'bar2', true)
	assert os.getenv('myvar1') == 'bar1'
	assert os.getenv('myvar2') == 'bar2'
	assert os.getenv('myvar_not_defined') == ''
	all := os.environ()
	assert all['myvar1'] == 'bar1'
	assert all['myvar2'] == 'bar2'
	assert all['myvar_not_defined'] == ''
}

fn test_setenv_var_not_exists() {
	key := time.new(time.now()).unix()
	os.setenv('foo${key}', 'bar', false)
	assert os.getenv('foo${key}') == 'bar'
}

fn test_getenv_empty_var() {
	key := time.new(time.now()).unix()
	os.setenv('empty${key}', '""', false)
	assert os.getenv('empty${key}') == '""'
}

fn test_environ_non_ascii() {
	os.setenv('Büro', 'gebäude', false)
	assert os.getenv('Büro') == 'gebäude'
	os.setenv('Büro', 'gebäudehaus', true)
	assert os.getenv('Büro') == 'gebäudehaus'
	os.setenv('Büro', 'gebäudehaus in der Straße', true)
	assert os.getenv('Büro') == 'gebäudehaus in der Straße'
	os.unsetenv('Büro')
	assert os.getenv('Büro') == ''

	os.setenv('한국어', '초보자를 위한', false)
	assert os.getenv('한국어') == '초보자를 위한'
	os.unsetenv('한국어')
	assert os.getenv('한국어') == ''
}
