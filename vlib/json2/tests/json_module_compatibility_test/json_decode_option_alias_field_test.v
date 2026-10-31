// vtest vflags: -w

module main

import json2

struct Req {
	height ?int
	width  ?i32
}

const payload = '{}'

fn test_main() {
	r := json2.decode[Req](payload) or { panic(err) }
	assert r.height == none
	assert r.width == none
}
