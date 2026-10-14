// Characterization of the current attribute shapes produced by the
// parser and observed through comptime `$for ... .attributes`.
// The custom-attributes work must preserve these shapes: the call
// form `@[Name(args)]` is flattened into a base attribute plus one
// `Name_arg: value` attribute per argument (the first positional
// argument becomes the base attribute's own value).
// Note: `vfmt` moves the bare `@[AttrPlain]` group after the
// valued groups (canonical attribute order); iteration follows
// that source order.
@[AttrStr: 'hello']
@[AttrNum: 42]
@[AttrBoolT: true]
@[AttrBoolF: false]
@[AttrCall(field: 'value')]
@[AttrPos('p0', second: 's1')]
@[AttrPos2('a', 'b')]
@[AttrNamed(msg: 'hello', after: '2026-01-01')]
@[AttrTypes(1, true)]
@[AttrEmpty()]
@[AttrMulti: 1; AttrOther]
@[AttrPlain]
struct AttrShapes {}

fn test_attr_shapes_decl_attributes() {
	mut got := []string{}
	$for attr in AttrShapes.attributes {
		mut kind := 'plain'
		if attr.kind == .string {
			kind = 'string'
		} else if attr.kind == .number {
			kind = 'number'
		} else if attr.kind == .bool {
			kind = 'bool'
		}
		has_arg := if attr.has_arg { 'true' } else { 'false' }
		got << '${attr.name}|${has_arg}|${attr.arg}|${kind}'
	}
	assert got == [
		'AttrStr|true|hello|string',
		'AttrNum|true|42|number',
		'AttrBoolT|true|true|bool',
		'AttrBoolF|true|false|bool',
		'AttrCall|false||plain',
		'AttrCall_field|true|value|string',
		'AttrPos|true|p0|string',
		'AttrPos_second|true|s1|string',
		'AttrPos2|true|a|string',
		'AttrPos2_1|true|b|string',
		'AttrNamed|false||plain',
		'AttrNamed_msg|true|hello|string',
		'AttrNamed_after|true|2026-01-01|string',
		'AttrTypes|true|1|number',
		'AttrTypes_1|true|true|bool',
		'AttrEmpty|false||plain',
		'AttrMulti|true|1|number',
		'AttrOther|false||plain',
		'AttrPlain|false||plain',
	]
}
