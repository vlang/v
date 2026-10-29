module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_wide_integer_sign_extension_and_right_shifts() {
	$if macos && arm64 {
		// Compare the actual native representation to fixed two's-complement limbs.
		// Runtime parameters prevent constant folding from hiding scalar widening.
		source := r'module main
fn C.exit(int)
struct WideBits {
    low u64
    high u64
}
type SignedWide = i128
fn check_bits(value voidptr, low u64, high u64, code int) {
    bits := unsafe { *(&WideBits(value)) }
    if bits.low != low || bits.high != high { C.exit(code) }
}
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
    positive_wide := i128(i64(7))
    check_bits(&positive_wide, 7, 0, 5)
    minimum := i128(i64(-9223372036854775807) - 1)
    check_bits(&minimum, 0x8000000000000000, 0xffffffffffffffff, 6)
    mixed := i128(0) + signed
    check_bits(&mixed, 0xfffffffffffffffd, 0xffffffffffffffff, 7)
    negative := (signed_wide << 64) | i128(u64(0x8000000000000001))
    positive := (i128(3) << 64) | i128(u64(0x8000000000000001))
    unsigned := (u128(0xfffffffffffffffd) << 64) | u128(0x8000000000000001)
    negative_0 := negative >> 0
    check_bits(&negative_0, 0x8000000000000001, 0xfffffffffffffffd, 10)
    negative_1 := negative >> 1
    check_bits(&negative_1, 0xc000000000000000, 0xfffffffffffffffe, 11)
    negative_63 := negative >> 63
    check_bits(&negative_63, 0xfffffffffffffffb, 0xffffffffffffffff, 12)
    negative_64 := negative >> 64
    check_bits(&negative_64, 0xfffffffffffffffd, 0xffffffffffffffff, 13)
    negative_65 := negative >> 65
    check_bits(&negative_65, 0xfffffffffffffffe, 0xffffffffffffffff, 14)
    negative_127 := negative >> 127
    check_bits(&negative_127, 0xffffffffffffffff, 0xffffffffffffffff, 15)
    positive_0 := positive >> 0
    check_bits(&positive_0, 0x8000000000000001, 0x0000000000000003, 16)
    positive_1 := positive >> 1
    check_bits(&positive_1, 0xc000000000000000, 0x0000000000000001, 17)
    positive_63 := positive >> 63
    check_bits(&positive_63, 0x0000000000000007, 0x0000000000000000, 18)
    positive_64 := positive >> 64
    check_bits(&positive_64, 0x0000000000000003, 0x0000000000000000, 19)
    positive_65 := positive >> 65
    check_bits(&positive_65, 0x0000000000000001, 0x0000000000000000, 20)
    positive_127 := positive >> 127
    check_bits(&positive_127, 0x0000000000000000, 0x0000000000000000, 21)
    unsigned_0 := unsigned >> 0
    check_bits(&unsigned_0, 0x8000000000000001, 0xfffffffffffffffd, 22)
    unsigned_1 := unsigned >> 1
    check_bits(&unsigned_1, 0xc000000000000000, 0x7ffffffffffffffe, 23)
    unsigned_63 := unsigned >> 63
    check_bits(&unsigned_63, 0xfffffffffffffffb, 0x0000000000000001, 24)
    unsigned_64 := unsigned >> 64
    check_bits(&unsigned_64, 0xfffffffffffffffd, 0x0000000000000000, 25)
    unsigned_65 := unsigned >> 65
    check_bits(&unsigned_65, 0x7ffffffffffffffe, 0x0000000000000000, 26)
    unsigned_127 := unsigned >> 127
    check_bits(&unsigned_127, 0x0000000000000001, 0x0000000000000000, 27)
    logical_0 := negative >>> 0
    check_bits(&logical_0, 0x8000000000000001, 0xfffffffffffffffd, 28)
    logical_1 := negative >>> 1
    check_bits(&logical_1, 0xc000000000000000, 0x7ffffffffffffffe, 29)
    logical_63 := negative >>> 63
    check_bits(&logical_63, 0xfffffffffffffffb, 0x0000000000000001, 30)
    logical_64 := negative >>> 64
    check_bits(&logical_64, 0xfffffffffffffffd, 0x0000000000000000, 31)
    logical_65 := negative >>> 65
    check_bits(&logical_65, 0x7ffffffffffffffe, 0x0000000000000000, 32)
    logical_127 := negative >>> 127
    check_bits(&logical_127, 0x0000000000000001, 0x0000000000000000, 33)
    alias := SignedWide(negative)
    alias_shifted := alias >> 65
    check_bits(&alias_shifted, 0xfffffffffffffffe, 0xffffffffffffffff, 40)
}
fn main() {
    check_conversions(-3, -2, u64(0xffffffffffffffff))
}
'
		path := os.join_path(os.vtmp_dir(), 'arm64_signed_wide_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, source) or { panic(err) }
		mut p := parser.Parser.new(pref.new_preferences())
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
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}
