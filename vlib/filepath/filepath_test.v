import filepath

fn test_ext() {
	assert filepath.ext('file.v') == '.v'
	assert filepath.ext('file') == ''
}

fn test_is_abs() {
	assert filepath.is_abs('/home/user') == true
	assert filepath.is_abs('v/vlib') == false

	$if windows {
		assert filepath.is_abs('C:\\Windows\\') == true
	}
}

fn test_join() {
	$if windows {
		assert filepath.join('v', 'vlib', 'filepath') == 'v\\vlib\\filepath'
	} $else {
		assert filepath.join('v', 'vlib', 'filepath') == 'v/vlib/filepath'
	}
}

fn test_dir() {
	$if windows {
		assert filepath.dir('C:\\a\\b\\c') == 'C:\\a\\b'
	} $else {
		assert filepath.dir('/var/tmp/foo') == '/var/tmp'
	}

	assert filepath.dir('filepath') == '.'
}

fn test_base() {
	$if windows {
		assert filepath.base('v\\vlib\\filepath') == 'v\\vlib'
	} $else {
		assert filepath.base('v/vlib/filepath') == 'v/vlib'
	}

	assert filepath.base('filename') == 'filename'
}
