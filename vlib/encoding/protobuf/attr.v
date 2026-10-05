module protobuf

// The schema vocabulary the code generator emits against. Everything here is
// non-generic on purpose: a `$if` chain inside a generic only resolves against a
// concrete type in this compiler.
//
// There is deliberately nothing here for reading `@[...]` attributes back off a
// struct: the generator writes an explicit call per field instead, so the field
// numbers live in the generated code rather than in attributes.

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
