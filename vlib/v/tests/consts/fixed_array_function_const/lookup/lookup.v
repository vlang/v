module lookup

pub const table = make_table()
pub const aliased = make_alias()

type Pair = [2]int

fn make_table() [4]int {
	return [11, 22, 33, 44]!
}

fn make_alias() Pair {
	return Pair([55, 66]!)
}

// get reads a function-initialized fixed array constant.
pub fn get(i int) int {
	return table[i]
}

// get_alias reads an alias of a function-initialized fixed array constant.
pub fn get_alias(i int) int {
	return aliased[i]
}
