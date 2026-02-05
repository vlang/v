module builtin

// AttributeKind describes the type of an attribute
pub enum AttributeKind {
	plain           // [name]
	string          // ['name']
	number          // [123]
	bool            // [true] || [false]
	comptime_define // [if name]
}

// VAttribute holds information about a V attribute
// Used by compile-time reflection ($for attr in field.attrs)
pub struct VAttribute {
pub:
	name    string
	has_arg bool
	arg     string
	kind    AttributeKind
}
