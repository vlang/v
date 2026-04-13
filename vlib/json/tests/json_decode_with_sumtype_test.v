import json

type Test = []bool | []int | []string | string

struct Some {
	t Test
}

type GetBlockResult = string | GetBlockResultB

struct GetBlockResponse {
	result GetBlockResult
}

struct GetBlockResultB {
	hash          string
	confirmations u64
}

fn test_json_decode_with_sumtype() {
	v1 := json.decode(Some, '{"t": ["string", "string2"]}')!
	println(v1)
	assert v1.t == Test(['string', 'string2'])

	v2 := json.decode(Some, '{"t": [11, 22]}')!
	println(v2)
	assert v2.t == Test([11, 22])

	v3 := json.decode(Some, '{"t": [true, false]}')!
	println(v3)
	assert v3.t == Test([true, false])
}

struct ComplexValue {
	foo string
}

type StringOrComplexValue = ComplexValue | string

fn test_json_decode_with_sumtype_struct_variant_without_type_field() {
	decoded := json.decode([]StringOrComplexValue, '["abc",{"foo":"bar"}]')!

	assert decoded.len == 2
	assert decoded[0] == StringOrComplexValue('abc')
	assert decoded[1] == StringOrComplexValue(ComplexValue{
		foo: 'bar'
	})
}

fn test_json_decode_with_sumtype_struct_field_without_type_field() {
	decoded := json.decode(GetBlockResponse,
		'{ "result": { "hash": "00000000c937983704a73af28acdec37b049d214adbda81d7e2a3dd146f6ed09", "confirmations": 743970 } }')!

	assert decoded.result == GetBlockResult(GetBlockResultB{
		hash:          '00000000c937983704a73af28acdec37b049d214adbda81d7e2a3dd146f6ed09'
		confirmations: 743970
	})
}
