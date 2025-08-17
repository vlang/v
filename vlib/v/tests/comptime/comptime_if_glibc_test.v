fn test_comptime_if_glibc() {
	mut x := 1
	mut result := ''
	$if glibc {
		result += '+glibc'
		x = 2
	} $else {
		result += '-glibc'
		x = 3
	}

	$if !glibc {
		result += '!glibc'
	} $else {
		result += '!!glibc'
	}

	println(result)
	assert result == '+glibc!!glibc' || result == '-glibc!glibc'
	println('done')
}
