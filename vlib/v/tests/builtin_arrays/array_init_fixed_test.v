module main

pub const n = 7
pub const nsq = n * n

pub fn empty_board(with_init bool) [nsq]u8 {
	if with_init {
		a := [nsq]u8{init: 0}
		dump(a)
		return [nsq]u8{init: 0}
	} else {
		a := [nsq]u8{}
		dump(a)
		return [nsq]u8{}
	}
}

fn test_with_init() {
	a := dump(empty_board(true))
	assert a.len == 49
}

fn test_without_init() {
	a := dump(empty_board(false))
	assert a.len == 49
}
