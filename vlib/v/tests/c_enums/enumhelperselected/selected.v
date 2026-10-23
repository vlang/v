module enumhelperselected

import v.tests.c_enums.enumhelperwidths as own

const base = 11
const delta = base

type Local = u8

@[comptime]
pub fn selected_offset(value own.Word) own.Word {
	mut narrowed := Local(value)
	narrowed = Local(narrowed + delta)
	return own.Word(narrowed)
}
