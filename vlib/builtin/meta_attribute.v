module builtin

pub enum AttributeKind {
	plain           // [name]
	string          // ['name']
	number          // [123]
	bool            // [true] || [false]
	comptime_define // [if name]	
}

pub struct VAttribute {
pub:
	name    string
	has_arg bool
	arg     string
	kind    AttributeKind
}
