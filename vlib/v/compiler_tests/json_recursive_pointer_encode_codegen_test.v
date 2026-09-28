import json

struct JsonRecursivePointerNode {
	value int
	next  ?&JsonRecursivePointerNode
}

struct JsonPointerEncodeEnvelope {
	omitted &JsonRecursivePointerNode = unsafe { nil } @[omitempty]
	values  []&JsonRecursivePointerNode
	by_name map[string]&JsonRecursivePointerNode
}

fn json_pointer_encode_node(value int, is_nil bool) &JsonRecursivePointerNode {
	return if is_nil {
		unsafe { nil }
	} else {
		&JsonRecursivePointerNode{
			value: value
		}
	}
}

fn test_json_recursive_pointer_helpers_encode_nested_values_and_nil() {
	leaf := json_pointer_encode_node(3, false)
	middle := &JsonRecursivePointerNode{
		value: 2
		next:  leaf
	}
	root := &JsonRecursivePointerNode{
		value: 1
		next:  middle
	}
	nil_node := json_pointer_encode_node(0, true)

	assert json.encode(root) == '{"value":1,"next":{"value":2,"next":{"value":3}}}'
	assert json.encode(nil_node) == 'null'
	assert json.encode(JsonPointerEncodeEnvelope{
		values:  [root, nil_node]
		by_name: {
			'root': root
			'nil':  nil_node
		}
	}) == '{"values":[{"value":1,"next":{"value":2,"next":{"value":3}}},null],"by_name":{"root":{"value":1,"next":{"value":2,"next":{"value":3}}},"nil":null}}'
}
