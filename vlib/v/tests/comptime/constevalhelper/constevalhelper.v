module constevalhelper

import constevalhelper.seed as evaluated

pub const seed = 7

pub type BoundWord = u16

pub const ordinary_bound_word = BoundWord(65530) + BoundWord(10)
pub const ordinary_bound_chain = ordinary_bound_word + BoundWord(65533)
pub const ordinary_bound_ratio = (u16(65530) + u16(10)) / u16(2)
pub const ordinary_bound_cast = int(u8(250 + 50))

pub const symbolic_bound = u8(evaluated.value) + 1

const symbolic_width = 2

pub type SymbolicPair = [symbolic_width]u8

pub struct SymbolicRecord {
pub:
	items [symbolic_width]u8
	value int
}

pub struct SymbolicOuter {
pub:
	record SymbolicRecord
}

@[comptime]
pub fn record_length(value SymbolicRecord) int {
	return value.items.len
}

@[comptime]
pub fn nested_length(value SymbolicOuter) int {
	return value.record.items.len
}

pub const symbolic_table = make_symbolic_table()

@[comptime]
pub fn make_symbolic_table() [symbolic_width]u8 {
	return [u8(1), 2]!
}

@[comptime]
pub fn last_symbolic(value [3]u8) int {
	return value[2]
}

// make_pair generates a fixed pair using this module's constant.
@[comptime]
pub fn make_pair(n u32) [2]u32 {
	return [n, n + seed]!
}

@[comptime]
pub fn symbolic_length() int {
	return [(seed + 1)]u8{}.len
}
