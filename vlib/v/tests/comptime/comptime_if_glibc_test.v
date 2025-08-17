fn test_comptime_if_glibc() {
	mut x := 1
	mut result := ''
	$if glibc {
		result += ' glibc'
		x = 2
	} $else {
		result += ' else glibc'
		x = 3
	}

	$if !glibc {
		result += ' !glibc'
	} $else {
		result += ' else !glibc'
	}

	println(result)
	assert result == ' glibc else !glibc' || result == ' else glibc  !glibc'
	println('done')
}
