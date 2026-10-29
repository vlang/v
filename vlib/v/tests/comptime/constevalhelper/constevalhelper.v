module constevalhelper

pub const seed = 7

// make_pair generates a fixed pair using this module's constant.
@[comptime]
pub fn make_pair(n u32) [2]u32 {
	return [n, n + seed]!
}
