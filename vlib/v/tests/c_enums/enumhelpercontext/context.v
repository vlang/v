module enumhelpercontext

import v.tests.c_enums.enumhelperwidths as own

const base = 7
const delta = base

type Local = u8

@[comptime]
pub fn offset(value int) int {
	return value + delta
}

@[comptime]
pub fn widths(value own.Word) own.Word {
	mut narrowed := Local(value)
	narrowed = Local(narrowed + delta)
	return own.Word(narrowed)
}

@[comptime]
pub fn return_width(value int) own.Word {
	return own.Word(value + delta)
}

@[comptime]
pub fn cast_width(value int) int {
	return int(own.Word(value))
}
