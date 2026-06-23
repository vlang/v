// this file is just to test the parser so there may be a
// bunch of stuff in here that does not really make sense

const len_a = 6
struct StructA {
	// NOTE: ideally attributes would not use `[]`
	// it would eliminate these issues completely. 

	// ambiguous: return `!` followed by attribute or
	// result w/ fixed array `![attribute_a]func_b`?
	// rely on newline and space between `!` and `[`,
	// or check later if we are using a var or const?
	// try eliminate this type of thing from the syntax.
	// TODO/FIXME: currently broken, must fix.
	func_a fn() ! [attribute_a]
	// this is fine
	func_b fn() ![len_a]u8

	// fixed - parse as attribute, not as index of `'foo'`
	field_c string = 'foo' [attribute_a]
}
