struct My {
	a int
	b string
	c f64
}

fn test_comptime_match_for_field_value() {
	x := My{}
	mut result := ''
	$for f in x.fields {
		$match f.name {
			'a' {
				result += 'a'
			}
			'b' {
				result += 'b'
			}
			'c' {
				result += 'c'
			}
			$else {
				result += '0'
			}
		}
	}
	assert result == 'abc'
}
