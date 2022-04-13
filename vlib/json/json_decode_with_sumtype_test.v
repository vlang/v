import json

type Test = []string | string

struct Some {
	t Test
}

fn test_json_decode_with_sumtype() ? {
	v := json.decode(Some, '{"t": ["string", "string2"]}') ?
	println(v)
	assert '$v.t' == "Test(['string', 'string2'])"
}
