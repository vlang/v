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

fn test_json_decode_with_sumtype_single_struct_variant() {
	text := json.decode(GetBlockResponse, '{"result":"hello"}')!
	assert text.result == GetBlockResult('hello')

	object := json.decode(GetBlockResponse, '{"result":{"hash":"00000000c937983704a73af28acdec37b049d214adbda81d7e2a3dd146f6ed09","confirmations":743970}}')!
	assert object.result == GetBlockResult(GetBlockResultB{
		hash:          '00000000c937983704a73af28acdec37b049d214adbda81d7e2a3dd146f6ed09'
		confirmations: 743970
	})
}
