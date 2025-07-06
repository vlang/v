module main

import json

struct Req {
	height ?int
	width  ?i32
}

const payload = '{}'

fn test_main() {
	r := json.decode(Req, payload) or { panic(err) }
	assert r.height == none
	assert r.width == none
}
