module main

import json

struct Definition {
	version u8
}

struct Logic {
	run fn () i8 @[required]
}

fn test_main() {
	logic := Logic{
		run: fn () i8 {
			json.encode_pretty(Definition{})
			return 0
		}
	}
	logic.run()
	assert true
}
