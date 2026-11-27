import os
import strconv

// The program below converts boundary and malformed strings with every integer
// conversion. Built with `-check-overflow`, it must print exactly what the
// ordinary build prints, instead of stopping inside the digit scanner.
const conversions_source = r'import strconv

const inputs = ["0", "7", "-7", "127", "128", "-128", "-129", "255", "256", "32767", "32768",
	"-32768", "-32769", "65535", "65536", "2147483647", "2147483648", "-2147483648", "-2147483649",
	"4294967295", "4294967296", "-4294967296", "9223372036854775807", "9223372036854775808",
	"-9223372036854775808", "-9223372036854775809", "18446744073709551615", "18446744073709551616",
	"-18446744073709551616", "99999999999999999999", "0x7fffffffffffffff", "0xffffffffffffffff",
	"0x10000000000000000", "-0x8000000000000000", "0o1777777777777777777777",
	"0o2000000000000000000000", "0b1111111111111111111111111111111111111111111111111111111111111111",
	"0b10000000000000000000000000000000000000000000000000000000000000000", "4294967296_", "1_000",
	"_1", "1.5", "12 ", " 12", "-", "+", "", "x", "-x", "1e3", "9\t", "0x", "0x_f", "zz"]

fn signed(s string, base int, bits int) string {
	value := strconv.parse_int(s, base, bits) or { return err.msg() }
	return value.str()
}

fn strict_signed(s string, base int, bits int) string {
	value := strconv.common_parse_int(s, base, bits, true, true) or { return err.msg() }
	return value.str()
}

fn unsigned(s string, base int, bits int) string {
	value := strconv.parse_uint(s, base, bits) or { return err.msg() }
	return value.str()
}

fn decimal(s string, bits int) string {
	match bits {
		8 {
			value := strconv.atoi8(s) or { return err.msg() }
			return value.str()
		}
		16 {
			value := strconv.atoi16(s) or { return err.msg() }
			return value.str()
		}
		32 {
			value := strconv.atoi32(s) or { return err.msg() }
			return value.str()
		}
		64 {
			value := strconv.atoi64(s) or { return err.msg() }
			return value.str()
		}
		else {
			value := strconv.atoi(s) or { return err.msg() }
			return value.str()
		}
	}
}

fn unsigned_decimal(s string, bits int) string {
	match bits {
		8 {
			value := strconv.atou8(s) or { return err.msg() }
			return value.str()
		}
		16 {
			value := strconv.atou16(s) or { return err.msg() }
			return value.str()
		}
		32 {
			value := strconv.atou32(s) or { return err.msg() }
			return value.str()
		}
		64 {
			value := strconv.atou64(s) or { return err.msg() }
			return value.str()
		}
		else {
			value := strconv.atou(s) or { return err.msg() }
			return value.str()
		}
	}
}

fn main() {
	for s in inputs {
		println("${s}: ${s.i8()} ${s.i16()} ${s.i32()} ${s.int()} ${s.i64()} ${s.u8()} ${s.u16()} ${s.u32()} ${s.u64()}")
		println("atoi: ${decimal(s, 0)} | ${decimal(s, 8)} | ${decimal(s, 16)} | ${decimal(s, 32)} | ${decimal(s, 64)}")
		println("atou: ${unsigned_decimal(s, 0)} | ${unsigned_decimal(s, 8)} | ${unsigned_decimal(s, 16)} | ${unsigned_decimal(s, 32)} | ${unsigned_decimal(s, 64)}")
		for base in [0, 2, 8, 10, 16, 36] {
			for bits in [0, 8, 16, 32, 64] {
				println("${s} ${base} ${bits}: ${signed(s, base, bits)} | ${strict_signed(s, base, bits)} | ${unsigned(s, base, bits)}")
			}
		}
	}
}
'

fn build_and_run(dir string, name string, flags []string) os.Result {
	exe := os.join_path(dir, name)
	mut args := [@VEXE]
	args << flags
	args << ['-o', exe, os.join_path(dir, 'conversions.v')]
	compiled := os.exec(args)
	assert compiled.exit_code == 0, compiled.output
	return os.exec([exe])
}

fn test_integer_parsing_gives_the_same_results_with_check_overflow() ! {
	dir := os.join_path(os.vtmp_dir(), 'strconv_check_overflow_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'conversions.v'), conversions_source)!
	plain := build_and_run(dir, 'plain', [])
	checked := build_and_run(dir, 'checked', ['-check-overflow'])
	assert plain.exit_code == 0, plain.output
	assert checked.exit_code == 0, checked.output
	assert checked.output == plain.output
}

fn test_a_byte_below_zero_ends_the_digits() {
	for s in ['1.5', '1/', '1 ', '1-', '1+', '1\t'] {
		n, code := strconv.common_parse_uint2(s, 10, 64)
		assert n == 1 && code == 2, s
		assert (strconv.atoi(s) or { -1 }) == -1, s
		assert (strconv.atoi64(s) or { -1 }) == -1, s
		assert (strconv.atou(s) or { 9 }) == 9, s
	}
	// No byte is a digit in a negative base.
	for base in [-1, -36] {
		n, code := strconv.common_parse_uint2('7', base, 64)
		assert n == 0 && code == 1, base.str()
	}
}

fn test_undocumented_bases_keep_the_existing_digit_decode() {
	// These values keep dev's existing decoding for compatibility; they are not a specified behavior.
	for base in [169, 170, 216, 217, 256, 300] {
		for byte in 0 .. 48 {
			s := u8(byte).ascii_str()
			digit := u64(byte + 169)
			n, code := strconv.common_parse_uint2(s, base, 64)
			if digit < u64(base) {
				assert n == digit && code == 0
			} else {
				assert n == 0 && code == 1
			}
		}
	}
}

fn test_undocumented_bases_keep_the_full_int_width() {
	if sizeof(int) == 8 {
		for base in [int(4294967297), int(8589934593), max_int] {
			n, code := strconv.common_parse_uint2('7', base, 64)
			assert n == 7 && code == 0
		}
	}
}
