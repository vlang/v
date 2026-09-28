import json2

struct MultiPointerBase {
	d &&int
}

// The embedded struct is decoded by a separate path from plain struct fields.
struct MultiPointerFields {
	MultiPointerBase
	e ?&&&string
	n &&int
	m ?&&int
}

fn test_multi_pointer_fields() {
	decoded := json2.decode[MultiPointerFields]('{"d": 7, "e": "x", "n": null, "m": null}')!
	assert **decoded.d == 7
	e := decoded.e or { panic('e should be set') }
	assert ***e == 'x'
	assert decoded.n == unsafe { nil }
	assert decoded.m == none
}
