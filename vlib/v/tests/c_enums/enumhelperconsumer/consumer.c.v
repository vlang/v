module enumhelperconsumer

import v.tests.c_enums.enumhelpercontext as imported

const argument = 5
const delta = 200

pub enum CopiedValues {
	value = int(Values.symbolic_member)
}

pub enum TransitiveValues {
	value = int(CopiedValues.value)
}

pub enum Values {
	known           = imported.offset(argument)
	symbolic        = C.enum_first_opt + imported.offset(argument)
	base            = 4
	symbolic_member = C.enum_first_opt + imported.offset(int(Values.base))
}

pub enum BackedValues as u16 {
	base     = 4
	symbolic = u16(C.enum_first_opt + imported.offset(int(BackedValues.base)))
	next
	copied   = u16(Values.symbolic_member)
}

@[flag]
pub enum FlagValues {
	base     = 0
	selected = imported.offset(int(Values.base))
}

@[flag]
pub enum BackedFlagValues as u16 {
	base     = 0
	selected = imported.offset(int(BackedValues.base))
}
