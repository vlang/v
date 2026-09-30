import os

// The overflow check on a wide literal has to read the digits in the base they
// are written in. Comparing the source text against decimal limits by length let
// 2^128 in hex through as zero, so these cases compile a small program with the
// compiler under test and read the error back.
fn compile_probe(name string, body string) string {
	dir := os.join_path(os.temp_dir(), 'v_int128_overflow')
	os.mkdir_all(dir) or {}
	src := os.join_path(dir, '${name}.v')
	os.write_file(src, 'fn main() {\n${body}}\n') or { return '' }
	build := os.execute('${@VEXE} -check ${src}')
	assert build.exit_code != 0, 'the probe was accepted: ${build.output}'
	return build.output
}

fn test_two_to_the_128_in_hex_is_rejected() {
	out := compile_probe('hex128',
		'\tx := u128(0x100000000000000000000000000000000)\n\tprintln(x)\n')
	assert out.contains('overflows `u128`')
}

fn test_a_hex_literal_above_the_signed_limit_is_rejected() {
	// The value is 2^128 - 1. It used to be accepted as -1.
	out := compile_probe('hex128_ones',
		'\tx := i128(0xffffffffffffffffffffffffffffffff)\n\tprintln(x)\n')
	assert out.contains('overflows `i128`')
}

fn test_a_decimal_literal_above_the_unsigned_limit_is_rejected() {
	out := compile_probe('dec128',
		'\tx := u128(340282366920938463463374607431768211456)\n\tprintln(x)\n')
	assert out.contains('overflows `u128`')
}

fn test_a_binary_literal_above_the_unsigned_limit_is_rejected() {
	// 2^128 written in binary: 1 followed by 128 zeros.
	out := compile_probe('bin128',
		'\tx := u128(0b100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)\n\tprintln(x)\n')
	assert out.contains('overflows `u128`')
}

fn test_a_hex_literal_a_bit_above_the_unsigned_limit_is_rejected() {
	out := compile_probe('hex129',
		'\tx := u128(0x200000000000000000000000000000000)\n\tprintln(x)\n')
	assert out.contains('overflows `u128`')
}
