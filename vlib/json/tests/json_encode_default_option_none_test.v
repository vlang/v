module main

import json

struct Test {
	id ?string
}

fn test_main() {
	assert json.encode(Test{}) == '{}'
}
