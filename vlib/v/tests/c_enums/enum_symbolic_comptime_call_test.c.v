#include "@VMODROOT/vlib/v/tests/c_enums/lowercase_c_enum.h"

@[comptime]
fn symbolic_enum_offset(value int) int {
	return value + 1
}

enum KnownCallEnum {
	value = symbolic_enum_offset(symbolic_enum_offset(1))
}

enum CopiedSymbolicCallEnum {
	value = int(SymbolicCallEnum.nested)
	next
}

enum TransitiveSymbolicCallEnum {
	value       = int(CopiedSymbolicCallEnum.next)
	conditional = int(SymbolicCallEnum.conditional)
}

enum SymbolicCallEnum {
	known       = 4
	plain       = C.enum_first_opt + symbolic_enum_offset(1)
	nested      = C.enum_first_opt + symbolic_enum_offset(symbolic_enum_offset(2))
	casted      = C.enum_first_opt + int(u8(symbolic_enum_offset(258)))
	prefix      = C.enum_first_opt - (-symbolic_enum_offset(5))
	member      = C.enum_first_opt + symbolic_enum_offset(int(SymbolicCallEnum.known))
	conditional = if C.enum_first_opt > symbolic_enum_offset(1) {
		symbolic_enum_offset(10)
	} else {
		12
	}
	else_if     = if C.enum_first_opt < 0 {
		13
	} else if C.enum_second_opt > 0 {
		symbolic_enum_offset(13)
	} else {
		15
	}
	unselected  = if C.enum_first_opt > 0 {
		16
	} else {
		symbolic_enum_offset(19)
	}
	narrowed    = if C.enum_first_opt > 0 {
		int(u8(symbolic_enum_offset(258)))
	} else {
		int(u8(symbolic_enum_offset(259)))
	}
	size        = int(sizeof(int)) + C.enum_first_opt
	character   = `a` + C.enum_first_opt
}

fn test_known_comptime_calls_fold_inside_symbolic_enum_initializers() {
	assert int(KnownCallEnum.value) == 3
	assert int(SymbolicCallEnum.plain) == 5
	assert int(SymbolicCallEnum.nested) == 7
	assert int(SymbolicCallEnum.casted) == 6
	assert int(SymbolicCallEnum.prefix) == 9
	assert int(SymbolicCallEnum.member) == 8
	assert int(SymbolicCallEnum.conditional) == 11
	assert int(SymbolicCallEnum.else_if) == 14
	assert int(SymbolicCallEnum.unselected) == 16
	assert int(SymbolicCallEnum.narrowed) == 3
	assert int(SymbolicCallEnum.size) == int(sizeof(int)) + 3
	assert int(SymbolicCallEnum.character) == 100
	assert int(CopiedSymbolicCallEnum.value) == 7
	assert int(CopiedSymbolicCallEnum.next) == 8
	assert int(TransitiveSymbolicCallEnum.value) == 8
	assert int(TransitiveSymbolicCallEnum.conditional) == 11
}
