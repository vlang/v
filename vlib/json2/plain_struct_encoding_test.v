import json2

struct PlainStructEncoding {
	name     string @[json: 'display"name']
	optional ?int
	zero     int    @[omitempty]
	hidden   string @[skip]
	ignored  string @[json: '-']
	forced   ?bool  @[required]
}

struct PlainStructEnvelope[T] {
	value T
}

struct PlainStructSibling {
	name string @[json: 'other_name']
}

fn test_plain_struct_encoding_keeps_metadata_and_nested_wrappers() {
	value := PlainStructEncoding{
		name:    'quoted'
		hidden:  'must not appear'
		ignored: 'must not appear either'
		forced:  none
	}
	expected := '{"display\\"name":"quoted","forced":null}'
	assert json2.encode(value) == expected
	assert json2.encode(PlainStructEnvelope[PlainStructEncoding]{ value: value }) == '{"value":${expected}}'
	assert json2.encode(PlainStructEnvelope[PlainStructSibling]{ value: PlainStructSibling{ name: 'sibling' } }) == '{"value":{"other_name":"sibling"}}'
	mut appended := 'prefix:'.bytes()
	json2.encode_append(value, mut appended)
	assert appended.bytestr() == 'prefix:${expected}'
	pretty := json2.encode(value, prettify: true)
	assert pretty == '{\n    "display\\"name": "quoted",\n    "forced": null\n}'
}
