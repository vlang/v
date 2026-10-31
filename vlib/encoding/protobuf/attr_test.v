module protobuf

// must_scalar returns the ProtoScalar `name` resolves to, failing if it does not.
fn must_scalar(name string) ProtoScalar {
	return scalar_by_name(name) or { panic('no scalar named ${name}') }
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
			assert false, '${name} must not resolve to a scalar'
		}
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
