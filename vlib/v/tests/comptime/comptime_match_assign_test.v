import strconv

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

fn decode_number[T](str string) !T {
	val := $match T.unaliased_typ {
		i8 { strconv.atoi8(str)! }
		i16 { strconv.atoi16(str)! }
		i32 { strconv.atoi32(str)! }
		i64 { strconv.atoi64(str)! }
		u8 { strconv.atou8(str)! }
		u16 { strconv.atou16(str)! }
		u32 { strconv.atou32(str)! }
		u64 { strconv.atou64(str)! }
		int { strconv.atoi(str)! }
		$float { T(strconv.atof_quick(str)) }
		$else { return error('`decode_number` can not decode ${T.name} type') }
	}
	return val
}

fn test_comptime_match_assign_generic() {
	x := decode_number[f32]('1.0')!
	assert x == 1.0
}
