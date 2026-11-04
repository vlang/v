@[attribute]
struct AttrStr {
	value string
}

@[attribute]
struct AttrNum {
	value int
}

@[attribute]
struct AttrBoolT {
	value bool
}

@[attribute]
struct AttrBoolF {
	value bool
}

@[attribute]
struct AttrCall {
	field string
}

@[attribute]
struct AttrPos {
	first  string
	second string
}

@[attribute]
struct AttrPos2 {
	first  string
	second string
}

@[attribute]
struct AttrNamed {
	msg   string
	after string
}

@[attribute]
struct AttrTypes {
	first  int
	second bool
}

@[attribute]
struct AttrEmpty {}

@[attribute]
struct AttrMulti {
	value int
}

@[attribute]
struct AttrOther {}

@[attribute]
struct AttrPlain {}

// Characterization of the current attribute shapes produced by the
// parser and observed through comptime `$for ... .attributes`.
// The parser flattens `@[Name(args)]` into a base attribute plus
// `Name_arg: value` pieces. A registered schema groups those pieces
// into one comptime attribute with typed fields.
// Note: `vfmt` moves the bare `@[AttrPlain]` group after the
// valued groups (canonical attribute order); iteration follows
// that source order.
// vfmt off
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
// vfmt on

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
		$if attr is AttrCall {
			assert attr.field == 'value'
		}
		$if attr is AttrPos {
			assert attr.first == 'p0'
			assert attr.second == 's1'
		}
		$if attr is AttrPos2 {
			assert attr.first == 'a'
			assert attr.second == 'b'
		}
		$if attr is AttrNamed {
			assert attr.msg == 'hello'
			assert attr.after == '2026-01-01'
		}
		$if attr is AttrTypes {
			assert attr.first == 1
			assert attr.second
		}
	}
	// Schemas group their flattened arguments into one comptime attribute.
	assert got == [
		'AttrStr|true|hello|string',
		'AttrNum|true|42|number',
		'AttrBoolT|true|true|bool',
		'AttrBoolF|true|false|bool',
		'AttrCall|false||plain',
		'AttrPos|true|p0|string',
		'AttrPos2|true|a|string',
		'AttrNamed|false||plain',
		'AttrTypes|true|1|number',
		'AttrEmpty|false||plain',
		'AttrMulti|true|1|number',
		'AttrOther|false||plain',
		'AttrPlain|false||plain',
	]
}
