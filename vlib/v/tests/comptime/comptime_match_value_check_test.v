const version = 123
const other = 456

fn test_comptime_match_value_check() {
	x := version
	mut result := ''

	$match x {
		1 {
			result += 'v1'
		}
		2 {
			result += 'v2'
		}
		3 {
			result += 'v3'
		}
		123 {
			result += 'v123'
		}
		$else {
			result += 'unknown'
		}
	}
	assert result == 'v123'

	result = ''
	y := true
	$match y {
		true {
			result += 'true'
		}
		false {
			result += 'false'
		}
	}
	assert result == 'true'

	result = ''
	z := 'abc'
	$match z {
		'123' {
			result += 'a'
		}
		'abc' {
			result += 'b'
		}
		$else {
			result += 'c'
		}
	}
	assert result == 'b'
}

fn test_comptime_match_value_check_reverse() {
	x := version
	y := 124
	z := 125

	mut result := ''
	$match 124 {
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

	assert result == 'y'

	result = ''
	a := true
	b := true
	c := false
	$match true {
		a {
			result += 'a'
		}
		b {
			result += 'b'
		}
		c {
			result += 'c'
		}
		$else {
			result += 'else'
		}
	}
	assert result == 'a'

	result = ''
	s1 := '123'
	s2 := 'abc'
	s3 := 'kml'
	$match 'abc' {
		s1 {
			result += 'a'
		}
		s2 {
			result += 'b'
		}
		s3 {
			result += 'c'
		}
		$else {
			result += 'else'
		}
	}
	assert result == 'b'
}
