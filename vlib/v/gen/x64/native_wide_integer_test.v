module x64

$if linux && amd64 {
	import os
	import v.parser
	import v.pref
	import v.ssa
	import v.transform
	import v.types
}

// build_and_run_wide compiles one source file through the native x64 pipeline,
// runs the binary, and returns its exit code and output.
fn build_and_run_wide(name string, source string) (int, string) {
	$if linux && amd64 {
		path := os.join_path(os.vtmp_dir(), 'x64_wide_${name}_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, source) or { panic(err) }
		mut preferences := pref.new_preferences()
		preferences.backend = 'x64'
		mut p := parser.Parser.new(preferences)
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.annotate_types()
		assert tc.errors.len == 0, tc.errors.str()
		transform.transform(mut a, tc)
		m := ssa.build_with_used(a, map[string]bool{}, tc)
		mut g := Gen.new(m)
		g.gen()
		g.write_and_link(output)
		result := os.exec([output])
		return result.exit_code, result.output
	}
	return 0, ''
}

const wide_bits_prelude = 'module main
fn C.exit(int)
struct WideBits {
    low u64
    high u64
}
fn check_bits(value voidptr, low u64, high u64, code int) {
    bits := unsafe { *(&WideBits(value)) }
    if bits.low != low || bits.high != high { C.exit(code) }
}
'

fn test_native_wide_integer_sign_extension_and_constant_shifts() {
	$if linux && amd64 {
		// Runtime parameters prevent constant folding from hiding scalar widening.
		source := wide_bits_prelude + r'type SignedWide = i128
fn check_conversions(signed i64, small i8, unsigned_scalar u64) {
    literal_negative := i128(-3)
    check_bits(&literal_negative, 0xfffffffffffffffd, 0xffffffffffffffff, 8)
    literal_positive := u128((0x8000000000000001))
    check_bits(&literal_positive, 0x8000000000000001, 0, 9)
    literal_wrapped := u128(i64(0x8000000000000001))
    check_bits(&literal_wrapped, 0x8000000000000001, 0xffffffffffffffff, 41)
    signed_wide := i128(signed)
    check_bits(&signed_wide, 0xfffffffffffffffd, 0xffffffffffffffff, 1)
    small_wide := i128(small)
    check_bits(&small_wide, 0xfffffffffffffffe, 0xffffffffffffffff, 2)
    unsigned_wide := i128(unsigned_scalar)
    check_bits(&unsigned_wide, 0xffffffffffffffff, 0, 3)
    wrapped := u128(signed)
    check_bits(&wrapped, 0xfffffffffffffffd, 0xffffffffffffffff, 4)
    minimum := i128(i64(-9223372036854775807) - 1)
    check_bits(&minimum, 0x8000000000000000, 0xffffffffffffffff, 6)
    mixed := i128(0) + signed
    check_bits(&mixed, 0xfffffffffffffffd, 0xffffffffffffffff, 7)
    negative := (signed_wide << 64) | i128(u64(0x8000000000000001))
    unsigned := (u128(0xfffffffffffffffd) << 64) | u128(0x8000000000000001)
    negative_1 := negative >> 1
    check_bits(&negative_1, 0xc000000000000000, 0xfffffffffffffffe, 11)
    negative_65 := negative >> 65
    check_bits(&negative_65, 0xfffffffffffffffe, 0xffffffffffffffff, 14)
    negative_127 := negative >> 127
    check_bits(&negative_127, 0xffffffffffffffff, 0xffffffffffffffff, 15)
    // A constant count past the width keeps only the sign of a signed value.
    negative_200 := negative >> 200
    check_bits(&negative_200, 0xffffffffffffffff, 0xffffffffffffffff, 16)
    unsigned_63 := unsigned >> 63
    check_bits(&unsigned_63, 0xfffffffffffffffb, 0x0000000000000001, 24)
    unsigned_200 := unsigned >> 200
    check_bits(&unsigned_200, 0, 0, 25)
    logical_65 := negative >>> 65
    check_bits(&logical_65, 0x7ffffffffffffffe, 0x0000000000000000, 32)
    alias := SignedWide(negative)
    alias_shifted := alias >> 65
    check_bits(&alias_shifted, 0xfffffffffffffffe, 0xffffffffffffffff, 40)
}
fn main() {
    check_conversions(-3, -2, u64(0xffffffffffffffff))
}
'
		exit_code, output := build_and_run_wide('conversions', source)
		assert exit_code == 0, 'exit ${exit_code}: ${output}'
	}
}

fn test_native_wide_integer_comparisons() {
	$if linux && amd64 {
		source := 'module main
fn C.exit(int)
fn compare_unsigned(a u128, b u128) int {
	mut mask := 0
	if a == b { mask += 1 }
	if a != b { mask += 2 }
	if a < b { mask += 4 }
	if a <= b { mask += 8 }
	if a > b { mask += 16 }
	if a >= b { mask += 32 }
	return mask
}
fn compare_signed(a i128, b i128) int {
	mut mask := 0
	if a == b { mask += 1 }
	if a != b { mask += 2 }
	if a < b { mask += 4 }
	if a <= b { mask += 8 }
	if a > b { mask += 16 }
	if a >= b { mask += 32 }
	return mask
}
fn main() {
	one := u128(1)
	high := one << 100
	if compare_unsigned(one, u128(2)) != 14 { C.exit(1) }
	if compare_unsigned(high, high) != 41 { C.exit(2) }
	if compare_unsigned(high + one, high) != 50 { C.exit(3) }
	if compare_unsigned(one << 127, high) != 50 { C.exit(5) }
	negative := i128(-1)
	if compare_signed(negative, i128(0)) != 14 { C.exit(7) }
	if compare_signed(i128(1) << 127, negative) != 14 { C.exit(11) }
	matched := match one { u128(1) { true } else { false } }
	if !matched { C.exit(13) }
	if !(one in u128(1)..u128(3)) { C.exit(15) }
	if !(negative in i128(-2)..i128(3)) { C.exit(16) }
	values := [one, high, high + one]
	if values.index(high) != 1 { C.exit(17) }
}
'
		exit_code, output := build_and_run_wide('comparisons', source)
		assert exit_code == 0, 'exit ${exit_code}: ${output}'
	}
}

fn test_native_wide_integer_arithmetic_and_conversions() {
	$if linux && amd64 {
		// The expected limbs were computed independently with arbitrary-precision
		// integers. The divisors reach every division path: both operands in 64
		// bits, a divisor below 2^32, and the general shift-and-subtract loop.
		source := wide_bits_prelude + r'struct Holder {
mut:
    value u128
    signed i128
}
fn arithmetic(a u128, b u128, c u128, d u128, t i128) {
    sa := -i128(a)
    product := a * b
    check_bits(&product, 0x7530eca876543210, 0xacf1356999999989, 1)
    signed_product := sa * t
    check_bits(&signed_product, 0x07f6e5d4c3b2a190, 0x8091a2b3c4d5e769, 2)
    short_quotient := a / c
    check_bits(&short_quotient, 0x480a8f072d1e6e9a, 0x000000004e2fff8a, 3)
    short_remainder := a % c
    check_bits(&short_remainder, 0x000000003095a7da, 0, 4)
    wide_quotient := a / d
    check_bits(&wide_quotient, 0x0048d159e26af37b, 0, 5)
    wide_remainder := a % d
    check_bits(&wide_remainder, 0xfe93e93e93e93e95, 0x0000000000000030, 6)
    loop_quotient := a / b
    check_bits(&loop_quotient, 0x888888787654321f, 0x0000000012345678, 7)
    loop_remainder := a % b
    check_bits(&loop_remainder, 0x00000000fffffff1, 0, 8)
    small_quotient := u128(100) / (c - u128(1000000000))
    check_bits(&small_quotient, 14, 0, 9)
    below := b / a
    check_bits(&below, 0, 0, 10)
    signed_quotient := sa / t
    check_bits(&signed_quotient, 0x24bbe557ef188b23, 0xfd663cca33099702, 11)
    signed_remainder := sa % t
    check_bits(&signed_remainder, 0xfffffffffffffffb, 0xffffffffffffffff, 12)
    negative_divisor := sa / -t
    check_bits(&negative_divisor, 0xdb441aa810e774dd, 0x0299c335ccf668fd, 13)
    negative_divisor_remainder := sa % -t
    check_bits(&negative_divisor_remainder, 0xfffffffffffffffb, 0xffffffffffffffff, 14)
    minimum := i128(1) << 127
    wrapped := minimum / (t - i128(8))
    check_bits(&wrapped, 0, 0x8000000000000000, 15)
    wrapped_remainder := minimum % (t - i128(8))
    check_bits(&wrapped_remainder, 0, 0, 16)
    negated := -a
    check_bits(&negated, 0x0123456789abcdf0, 0xedcba9876543210f, 17)
    complement := ~a
    check_bits(&complement, 0x0123456789abcdef, 0xedcba9876543210f, 18)
    // A power-of-two divisor in the high limb with a non-zero low dividend limb.
    power_of_two := ((u128(8) << 64) | (c - u128(1000000006))) / ((c - u128(1000000005)) << 64)
    check_bits(&power_of_two, 4, 0, 19)
}
fn shifts(a u128, n0 int, n37 int, n64 int, n100 int, n127 int, n128 int, negative int) {
    sa := -i128(a)
    left_37 := a << n37
    check_bits(&left_37, 0xca86420000000000, 0x579bde1fdb97530e, 20)
    right_37 := a >> n37
    check_bits(&right_37, 0xc4d5e6f787f6e5d4, 0x000000000091a2b3, 21)
    arithmetic_37 := sa >> n37
    check_bits(&arithmetic_37, 0x3b2a190878091a2b, 0xffffffffff6e5d4c, 22)
    logical_37 := sa >>> n37
    check_bits(&logical_37, 0x3b2a190878091a2b, 0x00000000076e5d4c, 23)
    left_64 := a << n64
    check_bits(&left_64, 0, 0xfedcba9876543210, 24)
    arithmetic_100 := sa >> n100
    check_bits(&arithmetic_100, 0xfffffffffedcba98, 0xffffffffffffffff, 25)
    logical_127 := sa >>> n127
    check_bits(&logical_127, 1, 0, 26)
    same := a << n0
    check_bits(&same, 0xfedcba9876543210, 0x123456789abcdef0, 27)
    gone := a << n128
    check_bits(&gone, 0, 0, 28)
    sign := sa >> n128
    check_bits(&sign, 0xffffffffffffffff, 0xffffffffffffffff, 29)
    // A negative count reads as a huge unsigned one and shifts every bit out.
    negative_count := a >> negative
    check_bits(&negative_count, 0, 0, 30)
    // So does a wide count with its upper limb set.
    wide_count := a << ((u128(1) << 64) + u128(n37))
    check_bits(&wide_count, 0, 0, 31)
    small_wide_count := a >> u128(n37)
    check_bits(&small_wide_count, 0xc4d5e6f787f6e5d4, 0x000000000091a2b3, 32)
}
fn assignments(a u128, b u128, c u128, d u128, t i128, n int) {
    mut x := a
    x += b
    x *= u128(3)
    x -= c
    x <<= n
    x >>= 3
    x /= d
    x %= b
    x ^= a
    x |= u128(1)
    x &= ~u128(2)
    check_bits(&x, 0xfedcba98eacdab85, 0x123456789abcdef0, 40)
    mut y := -i128(a)
    y += t
    y *= -3
    y -= i128(5)
    y >>= 4
    y /= t
    y <<= 2
    y %= i128(1000)
    check_bits(&y, 0x34, 0, 41)
    mut holder := Holder{
        value: a
        signed: -i128(a)
    }
    holder.value *= b
    holder.signed /= t
    check_bits(&holder.value, 0x7530eca876543210, 0xacf1356999999989, 42)
    check_bits(&holder.signed, 0x24bbe557ef188b23, 0xfd663cca33099702, 43)
    mut values := [a, b]
    values[0] %= c
    values[1] <<= n
    check_bits(&values[0], 0x000000003095a7da, 0, 44)
    check_bits(&values[1], 0x0000002000000020, 0, 45)
    pointer := &holder.value
    unsafe {
        *pointer -= u128(0x7530eca876543210)
    }
    check_bits(&holder.value, 0, 0xacf1356999999989, 46)
    mut counter := (u128(1) << 64) - u128(1)
    counter++
    check_bits(&counter, 0, 1, 47)
    counter--
    counter--
    check_bits(&counter, 0xfffffffffffffffe, 0, 48)
    holder.signed++
    check_bits(&holder.signed, 0x24bbe557ef188b24, 0xfd663cca33099702, 49)
}
fn conversions(a u128, scale f64) {
    sa := -i128(a)
    if u64(a) != 0xfedcba9876543210 { C.exit(60) }
    if i32(sa) != -1985229328 { C.exit(61) }
    if u8(a) != 0x10 { C.exit(62) }
    if f64(a) != 2.4197857203266735e+37 { C.exit(63) }
    if f64(sa) != -2.4197857203266735e+37 { C.exit(64) }
    from_float := u128(1e30 * scale)
    check_bits(&from_float, 0x4675000000000000, 0x0000000c9f2c9cd0, 65)
    negative_float := i128(-1e30 * scale)
    check_bits(&negative_float, 0xb98b000000000000, 0xfffffff360d3632f, 66)
    fraction := i128(-2.75 * scale)
    check_bits(&fraction, 0xfffffffffffffffe, 0xffffffffffffffff, 67)
    pointer := voidptr(a)
    if u128(pointer) != u128(u64(a)) { C.exit(68) }
    literal := u128(31732946804115296442105984367)
    check_bits(&literal, 0x382d0dc3a8e7f56f, 0x000000006688e0dd, 69)
    below_minimum := i128(-9223372036854775809)
    check_bits(&below_minimum, 0x7fffffffffffffff, 0xffffffffffffffff, 70)
}
fn ranges(start i128, end i128, base u128) {
    mut sum := i128(0)
    mut count := 0
    for i in start .. end {
        sum += i
        count++
    }
    if count != 7 || sum != i128(0) { C.exit(80) }
    mut total := u128(0)
    for j in base .. base + u128(4) {
        total += j
    }
    expected := base * u128(4) + u128(6)
    if total != expected { C.exit(81) }
}
fn main() {
    a := (u128(0x123456789abcdef0) << 64) | u128(0xfedcba9876543210)
    b := u128(0x100000001)
    c := u128(1000000007)
    d := (u128(1) << 70) + u128(1)
    arithmetic(a, b, c, d, i128(7))
    shifts(a, 0, 37, 64, 100, 127, 128, -1)
    assignments(a, b, c, d, i128(7), 5)
    conversions(a, 1.0)
    ranges(i128(-3), i128(4), (u128(1) << 64) - u128(2))
}
'
		exit_code, output := build_and_run_wide('arithmetic', source)
		assert exit_code == 0, 'exit ${exit_code}: ${output}'
	}
}

fn test_native_wide_integer_division_by_zero_panics() {
	$if linux && amd64 {
		for op, message in {
			'/': 'division by zero'
			'%': 'modulo by zero'
		} {
			source := 'module main
fn divide(a i128, b i128) i128 {
	return a ${op} b
}
fn main() {
	divide(i128(7), i128(0))
}
'
			exit_code, output := build_and_run_wide('zero', source)
			assert exit_code != 0, output
			assert output.contains(message), output
		}
	}
}
