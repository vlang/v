fn test_comptime_match_assign() {
	os := 'windows'
	x := $match os {
		'linux' { 'linux' }
		'windows' { 'windows' }
		$else { 'unknown' }
	}

	assert x == 'windows'

	i := 123
	y := $match i {
		1 { '1' }
		2 { '2' }
		123 { '123' }
		$else { 'unknown' }
	}
	assert y == '123'

	j := true
	z := $match j {
		true { 'T' }
		false { 'F' }
	}

	assert z == 'T'
}

fn test_comptime_match_assign_reverse() {
	os1 := 'windows'
	os2 := 'linux'
	os3 := 'macos'
	x := $match 'windows' {
		os1 { 'w' }
		os2 { 'l' }
		os3 { 'm' }
		$else { 'unknown' }
	}
	assert x == 'w'

	b1 := true
	b2 := false
	b3 := true
	y := $match false {
		b1 { 'b1' }
		b2 { 'b2' }
		b3 { 'b3' }
		$else { 'unknown' }
	}
	assert y == 'b2'

	i1 := 123
	i2 := 245
	i3 := 1023
	z := $match 1024 {
		i1 { '123' }
		i2 { '245' }
		i3 { '1023' }
		$else { 'unknown' }
	}
	assert z == 'unknown'
}
