module json2

fn test_truncated_nested_values_require_their_parent_delimiters() {
	for input in ['[0', '[12', '[true', '[false', '[null', '["text"', '[[1]', '[[]', '[{}', '{"a":0',
		'{"a":12', '{"a":true', '{"a":null', '{"a":"text"', '{"a":{}', '{"a":[1]', '{"a":{"b":2}',
		'[{"a":[1]}'] {
		mut rejected := false
		decode[Any](input) or { rejected = true }
		assert rejected, input
	}
}

fn test_nested_values_with_distinct_parent_delimiters_remain_valid() ! {
	for input in ['[0]', '[12]', '[true]', '[false]', '[null]', '["text"]', '[[1]]', '[[]]', '[{}]',
		'{"a":0}', '{"a":12}', '{"a":true}', '{"a":null}', '{"a":"text"}', '{"a":{}}', '{"a":[1]}',
		'{"a":{"b":2}}', '[{"a":[1]}]'] {
		_ := decode[Any](input)!
	}
}
