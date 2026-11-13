import json2 as json

type Issue24127Any = json.Any
type Issue24127Map = map[string]Issue24127Any
type Issue24127NestedAny = Issue24127Any

struct Issue24127Defaults {
	name  string = 'kept'
	value int    = 7
}

type Issue24127Sum = Issue24127Defaults | string
type Issue24127SumAlias = Issue24127Sum

const issue_24127_json_inputs = [
	'{}',
	'{"a":"b"}',
	'{"a":1}',
	'{"a":3.14}',
	'{"a":{"b":"c"}}',
	'{"a":true}',
	'{"a":null}',
	'{"a":[1,true,"text",null,{"b":[]}]}',
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

fn test_decode_any_alias_scalar_and_array_values() {
	for input in ['null', 'true', 'false', '42', '3.14', '"text"', '[]',
		'[1,true,"text",null,{"nested":[2]}]'] {
		direct := json.decode[json.Any](input)!
		aliased := json.decode[Issue24127Any](input)!
		nested := json.decode[Issue24127NestedAny](input)!
		assert json.encode(direct) == json.encode(json.Any(aliased))
		assert json.encode(direct) == json.encode(json.Any(nested))
	}
}

fn test_decode_array_with_any_alias_values() {
	input := '[null,true,false,42,3.14,"text",[],{},[1],{"nested":[2]}]'
	direct := json.decode[[]json.Any](input)!
	aliased := json.decode[[]Issue24127Any](input)!
	assert direct.len == aliased.len
	for i, value in direct {
		assert json.encode(value) == json.encode(json.Any(aliased[i]))
	}
}

fn test_decode_sumtype_alias_keeps_struct_defaults() {
	direct := json.decode[Issue24127Sum]('{}')!
	aliased := json.decode[Issue24127SumAlias]('{}')!
	assert (direct as Issue24127Defaults).name == 'kept'
	assert (direct as Issue24127Defaults).value == 7
	assert (aliased as Issue24127Defaults).name == 'kept'
	assert (aliased as Issue24127Defaults).value == 7
}
