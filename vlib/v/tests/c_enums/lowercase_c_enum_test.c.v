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
