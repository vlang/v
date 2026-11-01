module diagnostic_types

pub type Type = u32

pub const int_type = Type(1)
pub const string_type = Type(2)

pub struct TypeSymbol {
pub:
	typ Type
}
