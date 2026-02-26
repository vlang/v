fn test_test_ident() {
	mut result := ''
	$if test ? {
		result += '1'
	} $else {
		result += '2'
	}

	$if test {
		result += '3'
	} $else {
		result += '4'
	}

	$if !test {
		result += '5'
	} $else {
		result += '6'
	}

	$if $d('test', false) {
		result += '7'
	} $else {
		result += '8'
	}

	$if $d('test', true) {
		result += '9'
	} $else {
		result += '0'
	}

	assert result == '23689'
}

fn test_gcboehm_full_is_not_treated_as_user_define() {
	mut result := ''
	$if gcboehm_full ? {
		result += 'a'
	} $else {
		result += 'b'
	}
	assert result == 'b'
}
