module enumhelperconsumer

import v.tests.c_enums.enumhelpercontext as imported

const argument = 5
const delta = 200

pub enum Values {
	known    = imported.offset(argument)
	symbolic = C.enum_first_opt + imported.offset(argument)
}
