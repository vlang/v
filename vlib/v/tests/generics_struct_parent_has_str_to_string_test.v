import datatypes

const (
	w = 64
	h = 32
)

interface Display {
mut:
	pixel(x int, y int, val bool)
	clear()
	refresh()
}

struct Vm {
mut:
	display Display
	stack   datatypes.Stack<u16>
}

struct Pattern {
	pattern []u8
	handler fn (mut m Vm)
}

fn new_pattern(pattern string, handler fn (mut m Vm)) Pattern {
	return Pattern{pattern.runes().map(u8('0x$it'.int())), handler}
}

fn test_generics_struct_parent_has_str_to_string() {
	p := new_pattern('00E0', fn (mut m Vm) {
		println(m)
	})
	println(p)
	assert true
}
