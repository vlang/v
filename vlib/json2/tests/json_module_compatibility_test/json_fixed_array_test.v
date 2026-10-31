// vtest vflags: -w
import json2

struct Base {
	options  Options
	profiles Profiles
}

struct Options {
	cfg [6]u8
}

struct Profiles {
	cfg [4][7]u8
}

fn test_main() {
	a := json2.encode(Base{}, escape_unicode: true)
	println(a)
	assert a.contains('"cfg":[[0,0,0,0,0,0,0],[0,0,0,0,0,0,0],[0,0,0,0,0,0,0],[0,0,0,0,0,0,0]]')

	b := json2.decode[Base](a)!
	assert b.options.cfg.len == 6
	assert b.profiles.cfg.len == 4
	assert b.profiles.cfg[0].len == 7
}
