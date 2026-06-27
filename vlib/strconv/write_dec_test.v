import strconv

fn dec_to_string(n i64) string {
	mut buf := []u8{len: 21, init: `x`}
	written := strconv.write_dec(n, mut buf)
	assert written >= 0
	return buf[..written].bytestr()
}

fn dec_u_to_string(n u64) string {
	mut buf := []u8{len: 20, init: `x`}
	written := strconv.write_dec_u(n, mut buf)
	assert written >= 0
	return buf[..written].bytestr()
}

fn test_write_dec_signed_values() {
	assert dec_to_string(0) == '0'
	assert dec_to_string(1) == '1'
	assert dec_to_string(-1) == '-1'
	assert dec_to_string(1001) == '1001'
	assert dec_to_string(-1001) == '-1001'
	assert dec_to_string(1234567890) == '1234567890'
	assert dec_to_string(-1234567890) == '-1234567890'
	assert dec_to_string(max_i64) == '9223372036854775807'
	assert dec_to_string(-9223372036854775807) == '-9223372036854775807'
	assert dec_to_string(min_i64) == '-9223372036854775808'
	assert dec_to_string('-9223372036854775808'.i64()) == '-9223372036854775808'
}

fn test_write_dec_unsigned_values() {
	assert dec_u_to_string(0) == '0'
	assert dec_u_to_string(1) == '1'
	assert dec_u_to_string(1001) == '1001'
	assert dec_u_to_string(1234567890) == '1234567890'
	assert dec_u_to_string(u64(max_i64)) == '9223372036854775807'
	assert dec_u_to_string(u64(max_i64) + 1) == '9223372036854775808'
	assert dec_u_to_string(max_u64) == '18446744073709551615'
}

fn test_write_dec_returns_count_and_preserves_tail() {
	mut buf := []u8{len: 8, init: `x`}
	assert strconv.write_dec(42, mut buf) == 2
	assert buf.bytestr() == '42xxxxxx'

	buf = []u8{len: 8, init: `x`}
	assert strconv.write_dec(-42, mut buf) == 3
	assert buf.bytestr() == '-42xxxxx'

	buf = []u8{len: 8, init: `x`}
	assert strconv.write_dec_u(42, mut buf) == 2
	assert buf.bytestr() == '42xxxxxx'
}

fn test_write_dec_returns_minus_one_for_small_buffer() {
	mut buf := []u8{len: 2, init: `x`}
	assert strconv.write_dec(123, mut buf) == -1
	assert buf.bytestr() == 'xx'
	assert strconv.write_dec(-12, mut buf) == -1
	assert buf.bytestr() == 'xx'
	assert strconv.write_dec_u(123, mut buf) == -1
	assert buf.bytestr() == 'xx'

	assert strconv.write_dec(-1, mut buf) == 2
	assert buf.bytestr() == '-1'
	assert strconv.write_dec_u(99, mut buf) == 2
	assert buf.bytestr() == '99'
}
