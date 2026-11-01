module constevalhelper

import constevalhelper.seed as evaluated

pub const seed = 7

pub const symbolic_bound = u8(evaluated.value) + 1

// make_pair generates a fixed pair using this module's constant.
@[comptime]
pub fn make_pair(n u32) [2]u32 {
	return [n, n + seed]!
}

@[comptime]
pub fn symbolic_length() int {
	return [(seed + 1)]u8{}.len
}
