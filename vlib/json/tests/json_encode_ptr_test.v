import json

struct Number {
	min int
	max int
}

pub struct Resp {
pub:
	options []string @[omitempty]
	number  &Number = unsafe { nil }  @[omitempty]
}

fn (r Resp) str() string {
	return json.encode(r)
}

fn test_main() {
	r1 := Resp{
		options: ['first', 'second']
	}
	r2 := Resp{
		number: &Number{0, 0}
	}
	r3 := Resp{
		number: &Number{1, 2}
	}
	assert r1.str() == '{"options":["first","second"]}'
	assert r2.str() == '{"number":{"min":0,"max":0}}'
	assert r3.str() == '{"number":{"min":1,"max":2}}'
}
