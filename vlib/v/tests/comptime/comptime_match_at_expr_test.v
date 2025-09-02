module main

$match @MOD {
	'main' {
		const c1 = 'main'
	}
	$else {
		const c1 = 'other'
	}
}

$match @OS {
	'linux' {
		const os = 'linux'
	}
	'windows' {
		const os = 'windows'
	}
	$else {
		const os = 'other'
	}
}

fn test_comptime_match_at_expr() {
	assert c1 == 'main'

	dump(@FN)
	$match @FN {
		'test_comptime_match_at_expr' {
			assert true
		}
		$else {
			assert false
		}
	}

	$if linux {
		assert os == 'linux'
	} $else $if windows {
		assert os == 'windows'
	} $else {
		assert os == 'other'
	}
}
