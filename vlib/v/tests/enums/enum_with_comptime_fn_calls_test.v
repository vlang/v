@[comptime]
fn pack_color(r u8, g u8, b u8, a u8) u32 {
	return (u32(r) << 24) | (u32(g) << 16) | (u32(b) << 8) | u32(a)
}

enum Colors as u32 {
	red   = pack_color(255, 0, 0, 255)
	green = pack_color(0, 255, 0, 255)
	blue  = pack_color(0, 0, 255, 255)
}

enum CastedColors {
	red   = int(pack_color(1, 2, 3, 4))
	green = int(pack_color(5, 6, 7, 8))
	blue  = int(pack_color(9, 10, 11, 12))
}

fn test_enum_values_from_comptime_function_calls() {
	assert u32(Colors.red) == u32(0xff0000ff)
	assert u32(Colors.green) == u32(0x00ff00ff)
	assert u32(Colors.blue) == u32(0x0000ffff)
}

fn test_enum_values_from_casted_comptime_function_calls() {
	assert int(CastedColors.red) == 0x01020304
	assert int(CastedColors.green) == 0x05060708
	assert int(CastedColors.blue) == 0x090a0b0c
}

@[comptime]
fn enum_wrap8(a u8, b u8) u8 {
	return a + b
}

@[comptime]
fn enum_wrap16(a u16, b u16) u16 {
	return a + b
}

@[comptime]
fn enum_signed8(a i8, b i8) i8 {
	return a + b
}

@[comptime]
fn enum_cast_local(a int) int {
	converted := u8(a)
	return converted
}

enum WrappedComptime as u32 {
	byte_sum      = enum_wrap8(250, 10)
	short_sum     = enum_wrap16(65530, 12)
	argument_cast = enum_wrap8(u8(255) + 5, 1)
	local_cast    = enum_cast_local(511)
}

enum SignedComptime {
	sum = enum_signed8(120, 10)
}

fn test_comptime_enum_integer_conversions_match_runtime() {
	assert u32(WrappedComptime.byte_sum) == u32(enum_wrap8(250, 10))
	assert u32(WrappedComptime.short_sum) == u32(enum_wrap16(65530, 12))
	assert u32(WrappedComptime.argument_cast) == u32(enum_wrap8(u8(255) + 5, 1))
	assert u32(WrappedComptime.local_cast) == u32(enum_cast_local(511))
	assert int(SignedComptime.sum) == int(enum_signed8(120, 10))
}

@[comptime]
fn enum_highbit(a u32) u32 {
	return a >> 31
}

@[comptime]
fn enum_half_byte_sum(a u8, b u8) u8 {
	return (a + b) / 2
}

enum HighbitComptime as u32 {
	bit = enum_highbit(0x80000000)
}

enum PromotedByteComptime as u32 {
	half = enum_half_byte_sum(250, 10)
}

fn test_comptime_enum_unsigned_parameter_and_promoted_arithmetic() {
	assert u32(HighbitComptime.bit) == enum_highbit(0x80000000)
	assert u32(PromotedByteComptime.half) == u32(enum_half_byte_sum(250, 10))
	$for member in PromotedByteComptime.values {
		assert member.value == int(enum_half_byte_sum(250, 10))
	}
}

@[comptime]
fn enum_logical8(a i8) int {
	return int(a >>> 1)
}

@[comptime]
fn enum_logical16(a i16) int {
	return int(a >>> 1)
}

@[comptime]
fn enum_logical8_zero(a i8) int {
	return int(a >>> 0)
}

@[comptime]
fn enum_logical16_zero(a i16) int {
	return int(a >>> 0)
}

enum LogicalShiftComptime {
	byte_half  = enum_logical8(-128)
	short_half = enum_logical16(-32768)
	byte_bits  = enum_logical8_zero(-128)
	short_bits = enum_logical16_zero(-32768)
}

fn test_comptime_enum_unsigned_shift_uses_source_width() {
	assert int(LogicalShiftComptime.byte_half) == enum_logical8(-128)
	assert int(LogicalShiftComptime.short_half) == enum_logical16(-32768)
	assert int(LogicalShiftComptime.byte_bits) == enum_logical8_zero(-128)
	assert int(LogicalShiftComptime.short_bits) == enum_logical16_zero(-32768)
	$for member in LogicalShiftComptime.values {
		$if member.name == 'byte_half' {
			assert member.value == enum_logical8(-128)
		}
		$if member.name == 'short_half' {
			assert member.value == enum_logical16(-32768)
		}
		$if member.name == 'byte_bits' {
			assert member.value == enum_logical8_zero(-128)
		}
		$if member.name == 'short_bits' {
			assert member.value == enum_logical16_zero(-32768)
		}
	}
}

@[comptime]
fn enum_negate8(a u8) int {
	return int(-a) / 2
}

@[comptime]
fn enum_invert8(a u8) int {
	return int(~a) / 2
}

@[comptime]
fn enum_negate16(a u16) int {
	return int(-a) / 2
}

enum UnaryPromotionComptime {
	byte_negated  = enum_negate8(255)
	byte_inverted = enum_invert8(128)
	short_negated = enum_negate16(65535)
}

fn test_comptime_enum_unary_arithmetic_preserves_integer_promotion() {
	assert int(UnaryPromotionComptime.byte_negated) == enum_negate8(255)
	assert int(UnaryPromotionComptime.byte_inverted) == enum_invert8(128)
	assert int(UnaryPromotionComptime.short_negated) == enum_negate16(65535)
	$for member in UnaryPromotionComptime.values {
		$if member.name == 'byte_negated' {
			assert member.value == enum_negate8(255)
		}
		$if member.name == 'byte_inverted' {
			assert member.value == enum_invert8(128)
		}
		$if member.name == 'short_negated' {
			assert member.value == enum_negate16(65535)
		}
	}
}

@[comptime]
fn enum_logical_native_int(a int) int {
	return int(a >>> 1)
}

@[comptime]
fn enum_logical_i32(a i32) i64 {
	return i64(a >>> 1)
}

@[comptime]
fn enum_cast_i32(a i64) i64 {
	converted := i32(a)
	return i64(converted)
}

enum NativeAnd32BitComptime as i64 {
	native_shift = enum_logical_native_int(-2147483648)
	i32_shift    = enum_logical_i32(-2147483648)
	i32_cast     = enum_cast_i32(0x1ffffffff)
}

fn test_comptime_enum_native_int_and_i32_keep_distinct_widths() {
	assert i64(NativeAnd32BitComptime.native_shift) == i64(enum_logical_native_int(-2147483648))
	assert i64(NativeAnd32BitComptime.i32_shift) == enum_logical_i32(-2147483648)
	assert i64(NativeAnd32BitComptime.i32_cast) == enum_cast_i32(0x1ffffffff)
	$for member in NativeAnd32BitComptime.values {
		$if member.name == 'native_shift' {
			assert member.value == enum_logical_native_int(-2147483648)
		}
		$if member.name == 'i32_shift' {
			assert member.value == enum_logical_i32(-2147483648)
		}
		$if member.name == 'i32_cast' {
			assert member.value == enum_cast_i32(0x1ffffffff)
		}
	}
}

@[comptime]
fn enum_local_invert8(a u8) int {
	converted := ~a
	return int(converted) / 2
}

@[comptime]
fn enum_assigned_invert8(a u8) int {
	mut converted := a
	converted = ~a
	return int(converted) / 2
}

enum LocalPromotionComptime {
	inverted = enum_local_invert8(128)
}

enum AssignedPromotionComptime {
	inverted = enum_assigned_invert8(128)
}

fn test_comptime_enum_local_storage_converts_promoted_values() {
	assert int(LocalPromotionComptime.inverted) == enum_local_invert8(128)
	assert int(AssignedPromotionComptime.inverted) == enum_assigned_invert8(128)
	$for member in LocalPromotionComptime.values {
		assert member.value == enum_local_invert8(128)
	}
	$for member in AssignedPromotionComptime.values {
		assert member.value == enum_assigned_invert8(128)
	}
}

@[comptime]
fn enum_direct_invert8(a u8) u8 {
	return ~a / 2
}

@[comptime]
fn enum_direct_negate16(a u16) u16 {
	return -a / 2
}

enum NarrowDivisionComptime {
	inverted = enum_direct_invert8(128)
	negated  = enum_direct_negate16(65535)
}

fn test_comptime_enum_division_converts_operands_before_calculating() {
	assert int(NarrowDivisionComptime.inverted) == int(enum_direct_invert8(128))
	assert int(NarrowDivisionComptime.negated) == int(enum_direct_negate16(65535))
	$for member in NarrowDivisionComptime.values {
		$if member.name == 'inverted' {
			assert member.value == int(enum_direct_invert8(128))
		}
		$if member.name == 'negated' {
			assert member.value == int(enum_direct_negate16(65535))
		}
	}
}

@[comptime]
fn enum_unsigned64_shift(a u64) u64 {
	return a >> 1
}

@[comptime]
fn enum_unsigned64_divide(a u64) u64 {
	return a / 2
}

@[comptime]
fn enum_unsigned64_modulo(a u64) u64 {
	return a % 3
}

enum WideUnsignedComptime as u64 {
	shifted           = enum_unsigned64_shift(0x8000000000000000)
	divided           = enum_unsigned64_divide(0xffffffffffffffff)
	moduloed          = enum_unsigned64_modulo(0xffffffffffffffff)
	decimal           = enum_unsigned64_divide(010)
	argument_shift    = enum_unsigned64_shift(u64(0x8000000000000000) >> 1)
	argument_division = enum_unsigned64_divide(u64(0x8000000000000000) / 4)
	argument_modulo   = enum_unsigned64_modulo(u64(0xffffffffffffffff) % 7)
}

fn test_comptime_enum_unsigned64_keeps_high_bits() {
	assert u64(WideUnsignedComptime.shifted) == enum_unsigned64_shift(0x8000000000000000)
	assert u64(WideUnsignedComptime.divided) == enum_unsigned64_divide(0xffffffffffffffff)
	assert u64(WideUnsignedComptime.moduloed) == enum_unsigned64_modulo(0xffffffffffffffff)
	assert u64(WideUnsignedComptime.decimal) == 5
	assert u64(WideUnsignedComptime.argument_shift) == enum_unsigned64_shift(u64(0x8000000000000000) >> 1)
	assert u64(WideUnsignedComptime.argument_division) == enum_unsigned64_divide(u64(0x8000000000000000) / 4)
	assert u64(WideUnsignedComptime.argument_modulo) == enum_unsigned64_modulo(u64(0xffffffffffffffff) % 7)
	$for member in WideUnsignedComptime.values {
		$if member.name == 'shifted' {
			assert u64(member.value) == enum_unsigned64_shift(0x8000000000000000)
		}
		$if member.name == 'divided' {
			assert u64(member.value) == enum_unsigned64_divide(0xffffffffffffffff)
		}
		$if member.name == 'moduloed' {
			assert u64(member.value) == enum_unsigned64_modulo(0xffffffffffffffff)
		}
		$if member.name == 'decimal' {
			assert u64(member.value) == 5
		}
		$if member.name == 'argument_shift' {
			assert u64(member.value) == enum_unsigned64_shift(u64(0x8000000000000000) >> 1)
		}
		$if member.name == 'argument_division' {
			assert u64(member.value) == enum_unsigned64_divide(u64(0x8000000000000000) / 4)
		}
		$if member.name == 'argument_modulo' {
			assert u64(member.value) == enum_unsigned64_modulo(u64(0xffffffffffffffff) % 7)
		}
	}
}

@[comptime]
fn enum_passthrough_byte(a u8) u8 {
	return a
}

enum NarrowArgumentComptime {
	inverted = enum_passthrough_byte(~u8(128) / 2)
	shifted  = enum_passthrough_byte(i8(-128) >>> 1)
}

fn test_comptime_enum_narrow_argument_arithmetic_matches_runtime() {
	assert int(NarrowArgumentComptime.inverted) == int(enum_passthrough_byte(~u8(128) / 2))
	assert int(NarrowArgumentComptime.shifted) == int(enum_passthrough_byte(i8(-128) >>> 1))
	$for member in NarrowArgumentComptime.values {
		$if member.name == 'inverted' {
			assert member.value == int(enum_passthrough_byte(~u8(128) / 2))
		}
		$if member.name == 'shifted' {
			assert member.value == int(enum_passthrough_byte(i8(-128) >>> 1))
		}
	}
}
