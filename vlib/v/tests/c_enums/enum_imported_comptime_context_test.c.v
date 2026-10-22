import v.tests.c_enums.enumhelpercontext as imported
import v.tests.c_enums.enumhelperconsumer
import v.tests.c_enums.enumhelperdecoy as own
import v.tests.c_enums.enumhelperselected { selected_offset }

#include "@VMODROOT/vlib/v/tests/c_enums/lowercase_c_enum.h"

// These caller bindings must not affect helper declarations in other files.
const delta = 100
const base = 1000
const argument = 4

type Local = u16

@[comptime]
fn offset(value int) int {
	return value + 1000
}

enum KnownImportedCall {
	qualified    = imported.offset(argument)
	selected     = int(selected_offset(u8(255) + argument))
	parameter    = int(imported.widths(u8(255) + argument))
	returned     = int(imported.return_width(250))
	casted       = imported.cast_width(259)
	other_module = own.offset(argument)
	local        = offset(argument)
}

enum SymbolicImportedCall {
	qualified    = C.enum_first_opt + imported.offset(argument)
	selected     = C.enum_first_opt + int(selected_offset(u8(255) + argument))
	parameter    = C.enum_first_opt + int(imported.widths(u8(255) + argument))
	returned     = C.enum_first_opt + int(imported.return_width(250))
	casted       = C.enum_first_opt + imported.cast_width(259)
	other_module = C.enum_first_opt + own.offset(argument)
	local        = C.enum_first_opt + offset(argument)
}

enum Values {
	local = own.offset(argument)
}

fn test_imported_enum_helpers_keep_their_declaration_context() {
	assert delta == 100
	assert base == 1000
	assert Local(259) == 259
	assert own.Word(259) == 259
	mut known := []int{}
	$for member in KnownImportedCall.values {
		known << int(member.value)
	}
	assert known == [11, 14, 10, 1, 3, 104, 1004]
	assert int(SymbolicImportedCall.qualified) == 14
	assert int(SymbolicImportedCall.selected) == 17
	assert int(SymbolicImportedCall.parameter) == 13
	assert int(SymbolicImportedCall.returned) == 4
	assert int(SymbolicImportedCall.casted) == 6
	assert int(SymbolicImportedCall.other_module) == 107
	assert int(SymbolicImportedCall.local) == 1007
	assert int(enumhelperconsumer.Values.known) == 12
	assert int(enumhelperconsumer.Values.symbolic) == 15
	mut same_named := []int{}
	$for member in Values.values {
		same_named << int(member.value)
	}
	assert same_named == [104]
}
