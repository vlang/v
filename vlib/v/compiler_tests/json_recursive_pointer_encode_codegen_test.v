import json2

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

	assert json2.encode(root, escape_unicode: true) == '{"value":1,"next":{"value":2,"next":{"value":3}}}'
	assert json2.encode(nil_node, escape_unicode: true) == 'null'
	assert json2.encode(JsonPointerEncodeEnvelope{
		values:  [root, nil_node]
		by_name: {
			'root': root
			'nil':  nil_node
		}
	}, escape_unicode: true) == '{"values":[{"value":1,"next":{"value":2,"next":{"value":3}}},null],"by_name":{"root":{"value":1,"next":{"value":2,"next":{"value":3}}},"nil":null}}'
}
