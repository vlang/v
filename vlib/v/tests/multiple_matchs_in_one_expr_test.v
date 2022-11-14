fn hex_to_bytes(s string) ?[]u8 {
	mut ret := []u8{cap: s.len}
	for read := 0; read < s.len; read += 2 {
		high := s[read]
		low := s[read + 1]

		ret << match high {
			48...57 { (high - 48) << 4 }
			65...70 { (high - 65) << 4 }
			97...102 { (high - 97) << 4 }
			else { panic('impossible') }
		} + match low {
			48...57 { low - 48 }
			65...70 { low - 65 }
			97...102 { low - 97 }
			else { panic('impossible') }
		}
	}
	return ret
}

fn test_multiple_matchs_in_one_expr() {
	ret := hex_to_bytes('FFFF') or { 'error'.bytes() }
	println(ret)
	assert '${ret}' == '[85, 85]'
}
