import json2

type Enabled = bool

struct BooleanOptions {
	plain    bool
	enabled  bool    @[json: 'isEnabled'; omitempty]
	aliased  Enabled @[omitempty]
	optional ?bool   @[omitempty]
}

struct EmbeddedBooleanOptions {
	BooleanOptions
	name string
}

fn test_omitempty_false_boolean_fields() {
	assert json2.encode(BooleanOptions{}) == '{"plain":false}'
	assert json2.encode(BooleanOptions{ optional: false }) == '{"plain":false}'
	assert json2.encode(BooleanOptions{
		enabled:  true
		aliased:  Enabled(true)
		optional: true
	}) == '{"plain":false,"isEnabled":true,"aliased":true,"optional":true}'
}

fn test_omitempty_false_booleans_in_embedded_structs() {
	assert json2.encode(EmbeddedBooleanOptions{
		BooleanOptions: BooleanOptions{ optional: false }
		name:           'example'
	}) == '{"plain":false,"name":"example"}'
}
