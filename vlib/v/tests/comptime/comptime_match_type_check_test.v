fn test_comptime_match_type_check() {
	x := 100
	mut result := ''
	$match x {
		f64 {
			result += 'f64'
		}
		u32 {
			result += 'u32'
		}
		$int {
			result += '\$int'
		}
		int, i32 {
			result += 'int'
		}
		$else {
			result += 'unknown'
		}
	}

	assert result == '\$int'
}

fn test_comptime_match_type_check_reverse() {
	a := 100
	b := 200
	c := 300
	x := '123'
	y := u64(22)
	z := 1.2

	mut result := ''
	$match $int {
		a, b {
			result += 'a|b'
		}
		c {
			result += 'c'
		}
		x {
			result += 'x'
		}
		y {
			result += 'y'
		}
		z {
			result += 'z'
		}
	}

	assert result == 'a|b'
}
