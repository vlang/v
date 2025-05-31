import json

pub struct StructB {
	kind  string
	value i32
}

fn test_json_i32() {
	struct_b := json.decode(StructB, '{"kind": "Int32", "value": 100}')!
	assert struct_b == StructB{
		kind:  'Int32'
		value: 100
	}

	assert json.encode(struct_b) == '{"kind":"Int32","value":100}'
}
