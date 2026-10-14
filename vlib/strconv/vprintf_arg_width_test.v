import strconv

enum ArgWidthLevel {
	low
	mid
	high
}

enum ArgWidthByte as u8 {
	low
	high = 200
}

enum ArgWidthShort as i16 {
	low = -300
	high
}

type ArgWidthCode = i32

// v_sprintf reads an integer argument as an `int`. An argument of a narrower type
// has to arrive widened to an `int`, no matter what is stored next to it.
fn test_signed_decimal_of_each_integer_width() {
	unsafe {
		assert strconv.v_sprintf('%d', i32(-1)) == '-1'
		assert strconv.v_sprintf('%d %d %d %d', i8(-128), i16(-32768), i32(-2147483648),
			-2147483647) == '-128 -32768 -2147483648 -2147483647'
		assert strconv.v_sprintf('%i %i %i %i', i8(127), i16(32767), i32(2147483647),
			2147483647) == '127 32767 2147483647 2147483647'
		assert strconv.v_sprintf('%ld %lld', i64(-5000000000), i64(9223372036854775807)) == '-5000000000 9223372036854775807'
		assert strconv.v_sprintf('%d %d %d', u8(255), u16(65535), u32(2147483647)) == '255 65535 2147483647'
	}
}

fn test_unsigned_decimal_of_each_integer_width() {
	unsafe {
		assert strconv.v_sprintf('%u %u %u', u8(255), u16(65535), u32(4294967295)) == '255 65535 4294967295'
		assert strconv.v_sprintf('%lu %llu', u64(18446744073709551615), u64(5000000000)) == '18446744073709551615 5000000000'
		assert strconv.v_sprintf('%u %u %u %u', i8(-1), i16(-1), i32(-1), -1) == '4294967295 4294967295 4294967295 4294967295'
	}
}

fn test_hex_of_each_integer_width() {
	unsafe {
		assert strconv.v_sprintf('%x %x %x %x', u8(255), u16(65535), u32(0xdeadbeef),
			i32(255)) == 'ff ffff deadbeef ff'
		assert strconv.v_sprintf('%lx %X', u64(0xdeadbeefcafe), i32(0xabc)) == 'deadbeefcafe ABC'
	}
}

fn test_char_of_each_integer_width() {
	unsafe {
		assert strconv.v_sprintf('%c%c%c%c%c', `A`, u8(66), i32(67), 68, u32(69)) == 'ABCDE'
	}
}

fn test_rune_enum_and_alias_arguments() {
	unsafe {
		assert strconv.v_sprintf('%d %u %x', `€`, `€`, `€`) == '8364 8364 20ac'
		assert strconv.v_sprintf('%d %i %u %x', ArgWidthLevel.high, ArgWidthLevel.mid,
			ArgWidthLevel.high, ArgWidthLevel.mid) == '2 1 2 1'
		assert strconv.v_sprintf('%d %u %d', ArgWidthByte.high, ArgWidthByte.high,
			ArgWidthShort.low) == '200 200 -300'
		assert strconv.v_sprintf('%d %u', ArgWidthCode(-9), ArgWidthCode(9)) == '-9 9'
	}
}

fn test_narrow_arguments_between_other_arguments() {
	unsafe {
		assert strconv.v_sprintf('%s %d %.2f %d %s', 'a', i32(-1), 1.5, u32(2), 'b') == 'a -1 1.50 2 b'
		assert strconv.v_sprintf('[%5d] [%-5d] [%05d] [%+d] [%8x]', i32(-42), i32(-42),
			i32(-42), i32(42), u32(0xbeef)) == '[  -42] [-42  ] [-0042] [+42] [    beef]'
		assert strconv.v_sprintf('%.*s|', i32(2), 'abcdef') == 'ab|'
	}
}
