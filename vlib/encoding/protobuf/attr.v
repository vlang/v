module protobuf

// The schema vocabulary shared by hand-written codecs and by the code
// generator. Everything here is non-generic on purpose: a `$if` chain inside a
// generic only resolves against a concrete type in this compiler, and these
// helpers are reached from inside comptime loops over struct fields, where a
// generic call is a second hop and its chain stops matching.

// ProtoScalar names a protobuf scalar type. It exists because a V type does not
// determine one on its own: `i32` is `int32`, `sint32`, or `sfixed32` depending
// on the schema, and only the schema knows which.
pub enum ProtoScalar {
	boolean
	int32
	int64
	uint32
	uint64
	sint32
	sint64
	fixed32
	sfixed32
	fixed64
	sfixed64
	float32
	float64
}

// wire_type returns the wire type a ProtoScalar's values travel as.
pub fn (s ProtoScalar) wire_type() WireType {
	return match s {
		.boolean, .int32, .int64, .uint32, .uint64, .sint32, .sint64 { .varint }
		.fixed32, .sfixed32, .float32 { .fixed32 }
		.fixed64, .sfixed64, .float64 { .fixed64 }
	}
}

// attr_value returns the argument of a `name: value` attribute, with one layer
// of surrounding quotes removed. It mirrors the helper in `encoding.cbor` and
// `json2`, since V hands attributes over as plain strings.
pub fn attr_value(attr string) ?string {
	idx := attr.index(':') or { return none }
	mut value := attr[idx + 1..].trim_space()
	if value.len >= 2 && ((value.starts_with("'") && value.ends_with("'"))
		|| (value.starts_with('"') && value.ends_with('"'))) {
		value = value[1..value.len - 1]
	}
	return value
}

// field_number returns the protobuf field number declared by `@[protobuf: n]`,
// or zero when the field carries no such attribute.
//
// Zero is the answer for a field with no number because zero is not a legal
// field number, so it cannot be confused with a real one.
pub fn field_number(field_attrs []string) int {
	for attr in field_attrs {
		if attr.starts_with('protobuf:') {
			return (attr_value(attr) or { '' }).int()
		}
	}
	return 0
}

// oneof_group returns the name of the `oneof` group a field belongs to, from
// `@[protobuf_oneof: 'name']`, or an empty string when it belongs to none.
//
// A group makes its members mutually exclusive. A V sumtype variant carries a
// type but no attributes in this compiler, so a `oneof` cannot be expressed as
// a sumtype; parallel optional fields sharing a group name are the
// representation this module reads.
pub fn oneof_group(field_attrs []string) string {
	for attr in field_attrs {
		if attr.starts_with('protobuf_oneof:') {
			return attr_value(attr) or { '' }
		}
	}
	return ''
}

// field_skipped reports whether a field opts out of the wire with
// `@[protobuf_skip]`, which is how a V-only field rides along on a message that
// also has a wire representation.
pub fn field_skipped(field_attrs []string) bool {
	return 'protobuf_skip' in field_attrs
}

// field_scalar_override returns the ProtoScalar named by a field's
// `@[protobuf_type: '...']`, or none when the field carries no such attribute.
// This is the escape hatch for the i32-ambiguity: a field of type `i32` that
// the schema calls `sint32` says so with the attribute.
pub fn field_scalar_override(field_attrs []string) ?ProtoScalar {
	for attr in field_attrs {
		if attr.starts_with('protobuf_type:') {
			return scalar_by_name(attr_value(attr) or { '' })
		}
	}
	return none
}

// scalar_by_name resolves a name as it is spelled in a .proto schema, so a field
// can be annotated with the schema's own vocabulary.
pub fn scalar_by_name(name string) ?ProtoScalar {
	return match name {
		'bool', 'boolean' { ProtoScalar.boolean }
		'int32' { ProtoScalar.int32 }
		'int64' { ProtoScalar.int64 }
		'uint32' { ProtoScalar.uint32 }
		'uint64' { ProtoScalar.uint64 }
		'sint32' { ProtoScalar.sint32 }
		'sint64' { ProtoScalar.sint64 }
		'fixed32' { ProtoScalar.fixed32 }
		'sfixed32' { ProtoScalar.sfixed32 }
		'fixed64' { ProtoScalar.fixed64 }
		'sfixed64' { ProtoScalar.sfixed64 }
		'float' { ProtoScalar.float32 }
		'double' { ProtoScalar.float64 }
		else { none }
	}
}

// scalar_name returns the .proto spelling of a ProtoScalar, which is what the
// code generator writes into an `@[protobuf_type: ...]` attribute.
pub fn (s ProtoScalar) scalar_name() string {
	return match s {
		.boolean { 'bool' }
		.int32 { 'int32' }
		.int64 { 'int64' }
		.uint32 { 'uint32' }
		.uint64 { 'uint64' }
		.sint32 { 'sint32' }
		.sint64 { 'sint64' }
		.fixed32 { 'fixed32' }
		.sfixed32 { 'sfixed32' }
		.fixed64 { 'fixed64' }
		.sfixed64 { 'sfixed64' }
		.float32 { 'float' }
		.float64 { 'double' }
	}
}
