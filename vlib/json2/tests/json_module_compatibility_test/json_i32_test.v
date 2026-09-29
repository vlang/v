// vtest vflags: -w
import json2

pub struct StructB {
	kind  string
	value i32
}

fn test_json_i32() {
	struct_b := json2.decode[StructB]('{"kind": "Int32", "value": 100}')!
	assert struct_b == StructB{
		kind:  'Int32'
		value: 100
	}

	assert json2.encode(struct_b, escape_unicode: true) == '{"kind":"Int32","value":100}'
}
