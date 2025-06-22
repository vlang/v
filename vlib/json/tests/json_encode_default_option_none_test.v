// vtest vflags: -w
module main

import json

struct Test {
	id ?string = none
}

fn test_main() {
	assert json.encode(Test{}) == '{}'
}
