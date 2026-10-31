#include "@VMODROOT/vlib/v/tests/c_enums/symbolic_shift.h"

type ShiftAlias = i16

enum LogicalShift as u64 {
	byte_value     = i8(C.symbolic_negative) >>> 1
	short_value    = i16(C.symbolic_negative) >>> 1
	int_value      = i32(C.symbolic_negative) >>> 1
	long_value     = i64(C.symbolic_negative) >>> 1
	alias_value    = ShiftAlias(C.symbolic_negative) >>> 1
	grouped_value  = (i8(C.symbolic_negative) >>> 1) + 2
	nested_value   = i8(C.symbolic_negative) >>> (int(C.symbolic_one) + 1)
	wide_count     = i8(C.symbolic_negative) >>> int(C.symbolic_eight)
	negative_count = i64(C.symbolic_negative) >>> int(C.symbolic_negative_count)
}

enum CopiedLogicalShift as u64 {
	value = u64(LogicalShift.long_value)
}

enum OtherSymbolicShifts as i64 {
	wide_left        = 1 << int(C.symbolic_forty)
	int_boundary     = i32(C.symbolic_one) << 31
	long_boundary    = i64(C.symbolic_one) << 63
	narrow_left      = u8(C.symbolic_one) << int(C.symbolic_eight)
	narrow_right     = i8(C.symbolic_negative) >> int(C.symbolic_eight)
	arithmetic_right = i8(C.symbolic_negative) >> int(C.symbolic_one)
	negative_left    = u8(C.symbolic_one) << int(C.symbolic_negative_count)
}

enum SymbolicOperators {
	grouped_shift     = C.symbolic_one << 1 + 1
	grouped_bits      = C.symbolic_one | 1 + 1
	narrow_complement = ~u8(C.symbolic_one)
	narrow_sum        = u8(C.symbolic_one) + u8(255)
	known_power       = int(C.symbolic_one) + 2 ** 3
	wide_known_power  = int(C.symbolic_one) + 2 ** 40
}

enum KnownPowerContext as i64 {
	exponent = 3
	value    = int(C.symbolic_one) + 2 ** int(KnownPowerContext.exponent)
}

enum CopiedSymbolicOperators {
	grouped = int(SymbolicOperators.grouped_shift)
	narrow  = int(SymbolicOperators.narrow_sum)
}

fn test_symbolic_logical_shifts_preserve_operand_width() {
	assert u64(LogicalShift.byte_value) == 127
	assert u64(LogicalShift.short_value) == 32767
	assert u64(LogicalShift.int_value) == 2147483647
	assert u64(LogicalShift.long_value) == 9223372036854775807
	assert u64(LogicalShift.alias_value) == 32767
	assert u64(LogicalShift.grouped_value) == 129
	assert u64(LogicalShift.nested_value) == 63
	assert u64(CopiedLogicalShift.value) == 9223372036854775807
}

fn test_symbolic_shift_counts_use_operand_width() {
	assert u64(LogicalShift.wide_count) == 0
	assert u64(LogicalShift.negative_count) == 0
	assert i64(OtherSymbolicShifts.wide_left) == 1099511627776
	assert i64(OtherSymbolicShifts.int_boundary) == -2147483648
	assert i64(OtherSymbolicShifts.long_boundary) == -9223372036854775808
	assert i64(OtherSymbolicShifts.narrow_left) == 0
	assert i64(OtherSymbolicShifts.narrow_right) == 0
	assert i64(OtherSymbolicShifts.arithmetic_right) == -1
	assert i64(OtherSymbolicShifts.negative_left) == 0
}

fn test_symbolic_operators_preserve_grouping_and_storage_width() {
	assert int(SymbolicOperators.grouped_shift) == 3
	assert int(SymbolicOperators.grouped_bits) == 2
	assert int(SymbolicOperators.narrow_complement) == 254
	assert int(SymbolicOperators.narrow_sum) == 0
	assert int(SymbolicOperators.known_power) == 9
	assert int(SymbolicOperators.wide_known_power) == 1099511627777
	assert i64(KnownPowerContext.value) == 9
	assert int(CopiedSymbolicOperators.grouped) == 3
	assert int(CopiedSymbolicOperators.narrow) == 0
}
