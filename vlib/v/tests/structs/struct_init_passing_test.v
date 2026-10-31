// vtest vflags: -w

module main

import json2

struct Definition {
	version u8
}

struct Logic {
	run fn () i8 @[required]
}

fn test_main() {
	logic := Logic{
		run: fn () i8 {
			json2.encode(Definition{}, prettify: true, escape_unicode: true)
			return 0
		}
	}
	logic.run()
	assert true
}
