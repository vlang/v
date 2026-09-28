// vtest vflags: -w
module main

import json2

struct Test {
	id ?string = none
}

fn test_main() {
	assert json2.encode(Test{}, escape_unicode: true) == '{}'
}
