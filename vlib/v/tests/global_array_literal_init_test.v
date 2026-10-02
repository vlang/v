@[has_globals]
module main

struct Def {
	target int
	name   [4]u8
}

__global nums = [1, 2, 3]
__global defs = [Def{
	target: 3
	name:   [u8(116), 0, 0, 0]!
}, Def{}]
__global nested = [[1, 2], [3]]
__global words = ['a', 'bc']

// A global initialized with an array literal holds its elements from the start
// (C translated by c2v declares SQLite's `static ProgDef progs[8] = {{...}, {...}}`
// this way).
fn test_globals_initialized_with_array_literals() {
	assert nums == [1, 2, 3]
	assert defs.len == 2
	assert defs[0].target == 3
	assert defs[0].name[0] == 116
	assert defs[1].target == 0
	assert nested == [[1, 2], [3]]
	assert words == ['a', 'bc']
	nums << 4
	assert nums == [1, 2, 3, 4]
}
