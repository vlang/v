import json

type Test = []bool | []int | []string | string

struct Some {
	t Test
}

fn test_json_decode_with_sumtype() ? {
	v1 := json.decode(Some, '{"t": ["string", "string2"]}') ?
	println(v1)
	assert v1.t == Test(['string', 'string2'])

	v2 := json.decode(Some, '{"t": [11, 22]}') ?
	println(v2)
	assert v2.t == Test([11, 22])

	v3 := json.decode(Some, '{"t": [true, false]}') ?
	println(v3)
	assert v3.t == Test([true, false])
}
