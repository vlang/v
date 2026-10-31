@[has_globals]
module globaldata

__global (
	table          []int
	fixed_elements [3]int
)

fn init() {
	table = [10, 20]
	fixed_elements = [30, 40, 50]!
}

// first returns the first element of the module's dynamic array global.
pub fn first() int {
	return table[0]
}

// fixed_first returns the first element of the module's fixed array global.
pub fn fixed_first() int {
	return fixed_elements[0]
}

// lengths returns the lengths of both module globals.
pub fn lengths() (int, int) {
	return table.len, fixed_elements.len
}
