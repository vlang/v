module protobuf

struct Annotated {
pub mut:
	plain   string @[protobuf: 1]
	quoted  i32    @[protobuf: '2']
	zigzag  i32    @[protobuf: 3; protobuf_type: 'sint32']
	grouped string @[protobuf: 4; protobuf_oneof: 'filter']
	skipped string @[protobuf_skip]
	noname  string
}

// must_attr returns the argument of `attr`, failing the test if it has none.
fn must_attr(attr string) string {
	return attr_value(attr) or { panic('no argument in ${attr}') }
}

// must_scalar returns the ProtoScalar `name` resolves to, failing if it does not.
fn must_scalar(name string) ProtoScalar {
	return scalar_by_name(name) or { panic('no scalar named ${name}') }
}

fn test_field_number_parses_both_forms() {
	// V hands attributes over as plain strings, and the quoted form keeps its
	// quotes, so both spellings have to be accepted.
	$for field in Annotated.fields {
		match field.name {
			'plain' {
				assert field_number(field.attrs) == 1
				if field_scalar_override(field.attrs) != none {
					assert false, 'plain has no protobuf_type override'
				}
			}
			'quoted' {
				assert field_number(field.attrs) == 2, 'the quoted form must parse too'
			}
			'zigzag' {
				assert field_number(field.attrs) == 3
				// The option is unwrapped rather than compared to `none` or to
				// a bare enum: this compiler miscompiles an option on either
				// side of an `==` inside an assert.
				override := field_scalar_override(field.attrs) or { ProtoScalar.boolean }
				assert override == .sint32
			}
			'noname' {
				assert field_number(field.attrs) == 0, 'no attribute means no number'
			}
			else {}
		}
	}
}

fn test_oneof_group_is_readable() {
	$for field in Annotated.fields {
		match field.name {
			'grouped' {
				assert oneof_group(field.attrs) == 'filter'
			}
			'plain' {
				assert oneof_group(field.attrs) == ''
			}
			else {}
		}
	}
}

fn test_field_skipped() {
	$for field in Annotated.fields {
		if field.name == 'skipped' {
			assert field_skipped(field.attrs)
			assert field_number(field.attrs) == 0
		} else {
			assert !field_skipped(field.attrs), '${field.name} should not be skipped'
		}
	}
}

fn test_attr_value_strips_one_layer_of_quotes() {
	assert must_attr('protobuf: 1') == '1'
	assert must_attr("protobuf_type: 'sint32'") == 'sint32'
	assert must_attr('protobuf_type: "sint64"') == 'sint64'
	assert must_attr('protobuf_oneof: filter') == 'filter'
	// a bare word with no colon has no argument
	if attr_value('protobuf_skip') != none {
		assert false, 'a bare attribute has no argument'
	}
}

fn test_scalar_by_name_covers_the_schema_vocabulary() {
	assert must_scalar('bool') == .boolean
	assert must_scalar('boolean') == .boolean
	assert must_scalar('int32') == .int32
	assert must_scalar('int64') == .int64
	assert must_scalar('uint32') == .uint32
	assert must_scalar('uint64') == .uint64
	assert must_scalar('sint32') == .sint32
	assert must_scalar('sint64') == .sint64
	assert must_scalar('fixed32') == .fixed32
	assert must_scalar('sfixed32') == .sfixed32
	assert must_scalar('fixed64') == .fixed64
	assert must_scalar('sfixed64') == .sfixed64
	assert must_scalar('float') == .float32
	assert must_scalar('double') == .float64
	// not scalars: the generator must not confuse these for numbers
	for name in ['bytes', 'string', 'nonsense'] {
		if scalar_by_name(name) != none {
			assert false, ' must not resolve to a scalar'
		}
	}
}

fn test_scalar_name_round_trips() {
	// The generator writes the schema's own spelling, so it has to survive a
	// round trip through scalar_by_name.
	names := [
		'bool',
		'int32',
		'int64',
		'uint32',
		'uint64',
		'sint32',
		'sint64',
		'fixed32',
		'sfixed32',
		'fixed64',
		'sfixed64',
		'float',
		'double',
	]
	for name in names {
		assert must_scalar(name).scalar_name() == name
	}
}

fn test_scalar_wire_types_match_the_spec() {
	assert ProtoScalar.boolean.wire_type() == .varint
	assert ProtoScalar.int32.wire_type() == .varint
	assert ProtoScalar.int64.wire_type() == .varint
	assert ProtoScalar.uint32.wire_type() == .varint
	assert ProtoScalar.uint64.wire_type() == .varint
	assert ProtoScalar.sint32.wire_type() == .varint
	assert ProtoScalar.sint64.wire_type() == .varint
	assert ProtoScalar.fixed32.wire_type() == .fixed32
	assert ProtoScalar.sfixed32.wire_type() == .fixed32
	assert ProtoScalar.float32.wire_type() == .fixed32
	assert ProtoScalar.fixed64.wire_type() == .fixed64
	assert ProtoScalar.sfixed64.wire_type() == .fixed64
	assert ProtoScalar.float64.wire_type() == .fixed64
}
