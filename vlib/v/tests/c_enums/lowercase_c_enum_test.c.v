import v.tests.c_enums.importedflags

#include "@VMODROOT/vlib/v/tests/c_enums/lowercase_c_enum.h"

enum LowercaseCEnum {
	first  = C.enum_first_opt
	second = C.enum_second_opt
	third  = C.enum_third_opt + 1
}

fn test_lowercase_c_enum_values() {
	assert int(LowercaseCEnum.first) == 3
	assert int(LowercaseCEnum.second) == 4
	assert int(LowercaseCEnum.third) == 6
	assert LowercaseCEnum.second.str() == 'second'
}

enum CopiedCEnum {
	first = int(LowercaseCEnum.first)
	next
	reset = 10
	after_reset
}

enum TransitiveCEnum {
	first = int(CopiedCEnum.next)
	next
}

@[_allow_multiple_values]
enum SameEnumCopy {
	first  = C.enum_first_opt
	copied = int(SameEnumCopy.first)
	next
}

fn test_same_enum_c_copy_remains_symbolic() {
	assert int(SameEnumCopy.copied) == 3
	assert int(SameEnumCopy.next) == 4
	mut values := []int{}
	$for member in SameEnumCopy.values {
		values << int(member.value)
	}
	assert values == [3, 3, 4]
	copied := SameEnumCopy.from_string('copied') or { panic('missing enum member') }
	assert int(copied) == 3
}

enum UnknownReset {
	first = C.enum_first_opt
	next
	zero  = 0
	after_zero
}

fn test_c_derived_implicit_successor_can_reset_to_zero() {
	assert int(UnknownReset.next) == 4
	assert int(UnknownReset.zero) == 0
	assert int(UnknownReset.after_zero) == 1
	mut values := []int{}
	$for member in UnknownReset.values {
		values << int(member.value)
	}
	assert values == [3, 4, 0, 1]
}

fn test_c_derived_enum_selector_remains_symbolic() {
	assert int(CopiedCEnum.first) == 3
	assert int(CopiedCEnum.next) == 4
	assert int(CopiedCEnum.reset) == 10
	assert int(CopiedCEnum.after_reset) == 11
	assert int(TransitiveCEnum.first) == 4
	assert int(TransitiveCEnum.next) == 5
}

fn test_c_derived_enum_reflection_preserves_values() {
	mut values := []int{}
	mut metadata := []EnumData{}
	$for member in LowercaseCEnum.values {
		values << int(member.value)
		metadata << member
	}
	assert values == [3, 4, 6]
	assert metadata.map(it.value) == [i64(3), 4, 6]
	mut copied_values := []int{}
	$for member in CopiedCEnum.values {
		copied_values << int(member.value)
	}
	assert copied_values == [3, 4, 10, 11]
}

fn test_c_derived_enum_from_string_preserves_values() {
	value := LowercaseCEnum.from_string('second') or { panic('missing enum member') }
	assert int(value) == 4
	copied := CopiedCEnum.from_string('next') or { panic('missing enum member') }
	assert int(copied) == 4
}

enum KnownFlagPosition {
	third = 3
}

@[flag]
enum VEnumPositionFlag {
	item = int(KnownFlagPosition.third)
	next
}

@[flag]
enum VEnumPositionFlag64 as u64 {
	item = int(KnownFlagPosition.third)
	next
}

fn test_v_enum_selector_as_flag_position() {
	assert int(VEnumPositionFlag.item) == 8
	assert int(VEnumPositionFlag.next) == 16
	assert u64(VEnumPositionFlag64.item) == 8
	assert u64(VEnumPositionFlag64.next) == 16
}

enum BackedFlagPosition as u8 {
	third = 3
}

@[flag]
enum BackedPositionFlag {
	item = int(BackedFlagPosition.third)
	next
}

@[flag]
enum BackedPositionFlag64 as u64 {
	item = int(BackedFlagPosition.third)
	next
}

fn test_backed_enum_selector_as_flag_position() {
	assert int(BackedPositionFlag.item) == 8
	assert int(BackedPositionFlag.next) == 16
	assert u64(BackedPositionFlag64.item) == 8
	assert u64(BackedPositionFlag64.next) == 16
}

fn test_selectively_imported_enum_as_flag_position() {
	assert int(importedflags.Flags.item) == 8
	assert int(importedflags.Flags.next) == 16
}
