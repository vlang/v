import x.json2 as json

type Issue24127Any = json.Any
type Issue24127Map = map[string]Issue24127Any

const issue_24127_json_inputs = [
	'{}',
	'{"a":"b"}',
	'{"a":1}',
	'{"a":3.14}',
	'{"a":{"b":"c"}}',
	'{"a":true}',
]

fn test_decode_alias_of_any_sumtype() {
	for input in issue_24127_json_inputs {
		direct := json.decode[json.Any](input)!
		aliased := json.decode[Issue24127Any](input)!
		assert json.encode(direct) == json.encode(aliased)
	}
}

fn test_decode_map_with_any_alias_value() {
	for input in issue_24127_json_inputs {
		direct := json.decode[map[string]json.Any](input)!
		aliased := json.decode[Issue24127Map](input)!
		assert json.encode(direct) == json.encode(aliased)
	}
}
