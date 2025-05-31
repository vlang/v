fn test_match_with_comptime_if_expr_in_branch() {
	x := match 'green' {
		'red' {
			'color is red'
		}
		'green' {
			$if windows {
				'color is green windows'
			} $else {
				'color is green unix'
			}
		}
		else {
			'nothing to say'
		}
	}
	println(x)
	assert true
}
