module custombuilder

pub struct Builder {
mut:
	buf []u8
}

// write_string returns the custom builder's value.
pub fn (b &Builder) write_string(s string) int {
	return 42 + s.len
}

// str returns the custom builder's string.
pub fn (b &Builder) str() string {
	return 'custom-builder'
}

// from_pointer calls methods through a cast back to this module's Builder.
pub fn from_pointer(p voidptr) int {
	// The pointer came from a live Builder allocation in the caller.
	r := unsafe { &Builder(p) }
	assert r.str() == 'custom-builder'
	return r.write_string('xy')
}

// new returns a custom builder.
pub fn new() &Builder {
	return &Builder{}
}
