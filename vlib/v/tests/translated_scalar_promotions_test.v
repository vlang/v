@[translated]
module main

type TranslatedAddress = u64

struct SentinelFields {
	default_address TranslatedAddress = -1
	address         TranslatedAddress
}

fn translated_address(value TranslatedAddress) TranslatedAddress {
	return value
}

fn test_translated_unsigned_sentinels() {
	mut address := TranslatedAddress(0)
	address = -1
	assert address == u64(0xffffffffffffffff)
	assert address == -1
	assert -1 == address
	negative := i64(-1)
	assert address == negative
	assert negative == address
	assert !(negative < address)
	assert translated_address(-1) == address
	fields := SentinelFields{ address: -1 }
	assert fields.address == address
	assert fields.default_address == address
}

struct DirectUnsignedSentinel {
	address u64
}

fn test_translated_direct_unsigned_field_sentinel() {
	fields := DirectUnsignedSentinel{ address: -1 }
	assert fields.address == u64(0xffff_ffff_ffff_ffff)
}

fn test_translated_mixed_sign_comparisons_use_c_widths() {
	narrow_signed := int(-1)
	unsigned := u32(1)
	assert !(narrow_signed < unsigned)
	assert narrow_signed > unsigned
	assert narrow_signed == u32(0xffff_ffff)
	wide_signed := i64(-1)
	assert wide_signed < unsigned
	small_signed := i8(-1)
	small_unsigned := u16(1)
	assert small_signed < small_unsigned
	assert !(narrow_signed < rune(1))
	assert narrow_signed == rune(0xffff_ffff)
}

enum TranslatedUnsigned32Enum as u32 {
	zero = 0
	two  = 2
	high = 0xffff_ffff
}

fn test_translated_backed_enum_comparisons_use_c_widths() {
	narrow_signed := int(-1)
	assert !(narrow_signed < TranslatedUnsigned32Enum.high)
	assert narrow_signed == TranslatedUnsigned32Enum.high
	wide_signed := i64(-1)
	assert wide_signed < TranslatedUnsigned32Enum.high
}

fn translated_wrapped_subtraction() u32 {
	return u32(0) - int(1)
}

fn test_translated_mixed_arithmetic_uses_common_c_type() {
	assert (u32(0) - int(1)) > i64(0)
	assert translated_wrapped_subtraction() == u32(0xffff_ffff)
	assert (int(-1) + u32(1)) == u32(0)
	assert u32(0xffff_ffff) + i64(1) == i64(0x1_0000_0000)
	assert (int(-1) + rune(0)) > i64(0)
	assert (rune(0) + int(-1)) > i64(0)
}

fn test_translated_mixed_compound_arithmetic_uses_common_c_type() {
	mut quotient := int(-3)
	quotient /= u32(2)
	assert quotient == 2147483646
	mut remainder := int(-3)
	remainder %= u32(2)
	assert remainder == 1
	mut wrapped := int(-1)
	wrapped += u32(1)
	assert wrapped == 0
	mut values := [int(-3)]
	mut evaluations := [0]
	values[translated_compound_index(mut evaluations)] /= u32(2)
	assert evaluations[0] == 1
	assert values[0] == 2147483646
	mut fixed := [int(-3), 0]!
	ptr := unsafe { &fixed[0] }
	unsafe {
		ptr[0] /= u32(2)
	}
	assert fixed[0] == 2147483646
	mut direct_fixed := [int(-3), 0]!
	direct_fixed[0] /= u32(2)
	assert direct_fixed[0] == 2147483646
	mut mapped := {
		'a': int(-3)
	}
	mapped['a'] /= u32(2)
	assert mapped['a'] == 2147483646
	mut with_enum := int(-3)
	with_enum /= TranslatedUnsigned32Enum.two
	assert with_enum == 2147483646
	mut with_char := int(-3)
	with_char /= char(2)
	assert with_char == -1
	mut with_bool := int(-3)
	with_bool += true
	assert with_bool == -2
}

fn test_translated_enum_arithmetic_uses_common_type() {
	assert (int(-1) + TranslatedUnsigned32Enum.zero) > i64(0)
}

fn translated_compound_index(mut evaluations []int) int {
	evaluations[0]++
	return 0
}

fn translated_narrow_return(value u32) int {
	return value
}

fn translated_int_argument(value int) int {
	return value
}

fn test_translated_int_conversion_boundaries_use_c_width() {
	wide := u32(0xffff_ffff)
	assert translated_narrow_return(wide) == -1
	assert translated_int_argument(wide) == -1
	mut assigned := int(0)
	assigned = wide
	assert assigned == -1
}

fn translated_int_shift(count int) int {
	value := int(1)
	return value << count
}

fn test_translated_int_shift_uses_c_width() {
	assert translated_int_shift(40) == 0
}

enum TranslatedWideEnum as u64 {
	zero = 0
	high = 0x8000_0000_0000_0000
}

fn test_translated_wide_enum_preserves_backing_type() {
	value := TranslatedWideEnum.high | TranslatedWideEnum.zero
	assert typeof(value).name == 'u64'
	assert value > 0
	assert TranslatedWideEnum.high >> 63 == 1
}

fn test_translated_boolean_indices_and_pointer_offsets() {
	values := [11, 22, 33]!
	index := true
	assert values[index] == 22
	text := c'ab'
	assert text[index] == `b`
	mut ptr := unsafe { &values[0] }
	ptr += true
	assert *ptr == 22
	ptr -= true
	assert *ptr == 11
	mut raw := voidptr(ptr)
	raw += 1
	assert usize(raw) == usize(ptr) + 1
	raw -= 1
	assert raw == voidptr(ptr)
}

enum TranslatedPointerOffset {
	one = 1
}

fn test_translated_integral_pointer_expressions() {
	values := [11, 22, 33]!
	start := unsafe { &values[0] }
	assert *(start + true) == 22
	assert *(start + char(2)) == 33
	assert *(start + TranslatedPointerOffset.one) == 22
	assert *(true + start) == 22
	end := unsafe { &values[2] }
	assert *(end - true) == 22
}

fn translated_mixed_branch(flag bool) int {
	value := if flag { flag } else { 42 }
	return value
}

fn test_translated_mixed_scalar_branches() {
	assert translated_mixed_branch(true) == 1
	assert translated_mixed_branch(false) == 42
	flag := false
	value := if flag { 42 } else { flag }
	assert value == 0
	floating := if flag { true } else { 2.5 }
	assert floating == 2.5
	chained := if flag {
		true
	} else if !flag {
		42
	} else {
		false
	}
	assert chained == 42
}

fn test_translated_narrow_integer_shift() {
	value := u16(2)
	assert int(value << 16) == 131072
	shifted := value << 16
	assert shifted == 131072
}

type TranslatedHandler = fn (int) int

fn translated_increment(value int) int {
	return value + 1
}

fn translated_handler_address() voidptr {
	return voidptr(translated_increment)
}

fn test_translated_callback_address_assignment() {
	mut callback := TranslatedHandler(unsafe { nil })
	assert callback == 0
	assert 0 == callback
	assert callback != -1
	assert -1 != callback
	callback = translated_handler_address()
	assert callback(41) == 42
	assert callback != 0
	assert callback != -1
}

enum TranslatedScalarKind {
	zero
	value = 42
}

fn test_translated_mixed_fixed_width_and_enum_branches() {
	flag := false
	fixed := if flag { true } else { i32(42) }
	assert fixed == 42
	kind := if flag { true } else { TranslatedScalarKind.value }
	assert int(kind) == 42
	character := if flag { true } else { char(42) }
	assert int(character) == 42
	reversed := if !flag { TranslatedScalarKind.value } else { true }
	assert int(reversed) == 42
}

fn translated_float_return(value f64) int {
	return value
}

fn translated_negative_unsigned_return() u64 {
	return -1
}

fn test_translated_scalar_return_conversions() {
	assert translated_float_return(4.75) == 4
	assert translated_negative_unsigned_return() == u64(0xffffffffffffffff)
}

fn test_translated_unary_and_shift_integral_promotions() {
	kind := TranslatedScalarKind.value
	character := char(2)
	negative_bool := -true
	assert negative_bool == -1
	assert typeof(negative_bool).name == 'int'
	assert int(~kind) == -43
	assert int(-kind) == -42
	assert int(~character) == -3
	assert int(-character) == -2
	bool_sum := true + 2
	assert bool_sum == 3
	assert typeof(bool_sum).name == 'int'
	assert int(kind << 1) == 84
	assert int(character >> 1) == 1
	assert int(1 << true) == 2
	bool_shifted := true << 1
	assert bool_shifted == 2
	assert typeof(bool_shifted).name == 'i32'
}

fn test_translated_conditional_uses_common_numeric_type() {
	flag := false
	value := if flag { 1 } else { 2.5 }
	assert value == 2.5
	assert typeof(value).name == 'f64'
}

fn translated_large_hex() i64 {
	return 0xffff_ffff + int(0)
}

fn translated_large_decimal() i64 {
	return 4294967295 + int(0)
}

fn translated_large_hex_conditional(flag bool) i64 {
	return if flag { 0xffff_ffff } else { int(0) }
}

fn translated_large_decimal_conditional(flag bool) i64 {
	return if flag { int(0) } else { 4294967295 }
}

fn test_translated_large_integer_literal_types() {
	assert translated_large_hex() == i64(4294967295)
	assert translated_large_decimal() == i64(4294967295)
	assert translated_large_hex_conditional(true) == i64(4294967295)
	assert translated_large_hex_conditional(false) == 0
	assert translated_large_decimal_conditional(false) == i64(4294967295)
	assert translated_large_decimal_conditional(true) == 0
	assert (0xffff_ffff + int(1)) == u32(0)
	assert (4294967295 + int(1)) == i64(4294967296)
	assert (0o37777777777 + int(1)) == u32(0)
	assert (0b11111111111111111111111111111111 + int(1)) == u32(0)
	assert (0x1_0000_0000 + int(1)) == i64(4294967297)
	assert (0x8000_0000_0000_0000 + int(1)) == u64(9223372036854775809)
	assert (-0xffff_ffff) == u32(1)
	assert (0xffff_ffff >> 31) == u32(1)
	assert (0x1_0000_0000 >> 32) == i64(1)
	hex := 0xffff_ffff
	decimal := 4294967295
	assert typeof(hex).name == 'u32'
	assert typeof(decimal).name == 'i64'
	assert hex + int(1) == u32(0)
	assert decimal + int(1) == i64(4294967296)
}

const translated_literal_shift = 1 << 40
const translated_wide_literal_shift = 0x1_0000_0000 << 1

fn test_translated_literal_shifts_keep_their_c_width() {
	literal := 1 << 40
	assert literal == 0
	assert translated_literal_shift == 0
	assert translated_wide_literal_shift == i64(0x2_0000_0000)
	assert 0x1_0000_0000 << 1 == i64(0x2_0000_0000)
	for count in [31, 32, 40, 64, -1] {
		assert (1 << count) == (int(1) << count)
		assert (1 >> count) == (int(1) >> count)
		assert (1 >>> count) == (int(1) >>> count)
	}
}
