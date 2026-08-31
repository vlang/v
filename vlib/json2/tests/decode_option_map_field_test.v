import json2

struct WithOptMap {
mut:
	m ?map[string]string
	n ?map[string]int
}

struct BaseWithOptMap {
mut:
	bm ?map[string]string
}

struct EmbedWithOptMap {
	BaseWithOptMap
mut:
	m ?map[string]string
}

// https://github.com/vlang/v/issues/27810
fn test_decode_option_map_field() {
	foo := json2.decode[WithOptMap]('{"m":{"a":"b"}}')!
	if m := foo.m {
		assert m['a'] == 'b'
	} else {
		assert false
	}
	assert foo.n == none
}

fn test_decode_option_map_field_null() {
	foo := json2.decode[WithOptMap]('{"m":null,"n":{"x":1}}')!
	assert foo.m == none
	if n := foo.n {
		assert n['x'] == 1
	} else {
		assert false
	}
}

fn test_decode_option_map_field_with_embed() {
	foo := json2.decode[EmbedWithOptMap]('{"bm":{"k":"v"},"m":{"a":"b"}}')!
	if bm := foo.bm {
		assert bm['k'] == 'v'
	} else {
		assert false
	}
	if m := foo.m {
		assert m['a'] == 'b'
	} else {
		assert false
	}
}
