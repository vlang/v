module types

pub const bool_ = Primitive{
	props: .boolean
}
pub const int_ = Primitive{
	props: .integer
}
pub const i8_ = Primitive{
	props: .integer
	size:  8
}
pub const i16_ = Primitive{
	props: .integer
	size:  16
}
pub const i32_ = Primitive{
	props: .integer
	size:  32
}
pub const i64_ = Primitive{
	props: .integer
	size:  64
}
pub const u8_ = Primitive{
	props: .integer | .unsigned
	size:  8
}
pub const u16_ = Primitive{
	props: .integer | .unsigned
	size:  16
}
pub const u32_ = Primitive{
	props: .integer | .unsigned
	size:  32
}
pub const u64_ = Primitive{
	props: .integer | .unsigned
	size:  64
}
pub const f32_ = Primitive{
	props: .float
	size:  32
}
pub const f64_ = Primitive{
	props: .float
	size:  64
}
pub const string_ = String{}
pub const char_ = Char{}
pub const rune_ = Rune{}
pub const isize_ = ISize{}
pub const usize_ = USize{}
pub const void_ = Void{}
pub const nil_ = Nil{}
pub const none_ = None{}
pub const voidptr_ = Pointer{
	base_type: Type(Void{})
}
pub const charptr_ = Pointer{
	base_type: Type(Char{})
}
pub const byteptr_ = Pointer{
	base_type: Type(Primitive{
		props: .integer | .unsigned
		size:  8
	})
}

// is_builtin_type_name reports whether name is one of V's builtin type names.
pub fn is_builtin_type_name(name string) bool {
	return name == 'bool' || name == 'int' || name == 'i8' || name == 'i16' || name == 'i32'
		|| name == 'i64' || name == 'u8' || name == 'byte' || name == 'u16' || name == 'u32'
		|| name == 'u64' || name == 'f32' || name == 'f64' || name == 'string' || name == 'char'
		|| name == 'rune' || name == 'isize' || name == 'usize' || name == 'void'
		|| name == 'voidptr' || name == 'array' || name == 'charptr' || name == 'byteptr'
		|| name == 'nil' || name == 'none'
}

// builtin_type_value returns the Type for a known builtin type name.
pub fn builtin_type_value(name string) Type {
	if name == 'bool' {
		return Type(Primitive{
			props: .boolean
		})
	}
	if name == 'int' {
		return Type(Primitive{
			props: .integer
		})
	}
	if name == 'i8' {
		return Type(Primitive{
			props: .integer
			size:  8
		})
	}
	if name == 'i16' {
		return Type(Primitive{
			props: .integer
			size:  16
		})
	}
	if name == 'i32' {
		return Type(Primitive{
			props: .integer
			size:  32
		})
	}
	if name == 'i64' {
		return Type(Primitive{
			props: .integer
			size:  64
		})
	}
	if name == 'u8' || name == 'byte' {
		return Type(Primitive{
			props: .integer | .unsigned
			size:  8
		})
	}
	if name == 'u16' {
		return Type(Primitive{
			props: .integer | .unsigned
			size:  16
		})
	}
	if name == 'u32' {
		return Type(Primitive{
			props: .integer | .unsigned
			size:  32
		})
	}
	if name == 'u64' {
		return Type(Primitive{
			props: .integer | .unsigned
			size:  64
		})
	}
	if name == 'f32' {
		return Type(Primitive{
			props: .float
			size:  32
		})
	}
	if name == 'f64' {
		return Type(Primitive{
			props: .float
			size:  64
		})
	}
	if name == 'string' {
		return Type(String{})
	}
	if name == 'char' {
		return Type(Char{})
	}
	if name == 'rune' {
		return Type(Rune{})
	}
	if name == 'isize' {
		return Type(ISize{})
	}
	if name == 'usize' {
		return Type(USize{})
	}
	if name == 'void' {
		return Type(Void{})
	}
	if name == 'voidptr' {
		return Type(Pointer{
			base_type: Type(Void{})
		})
	}
	if name == 'array' {
		return Type(Array{
			elem_type: Type(Void{})
		})
	}
	if name == 'charptr' {
		return Type(Pointer{
			base_type: Type(Char{})
		})
	}
	if name == 'byteptr' {
		return Type(Pointer{
			base_type: Type(Primitive{
				props: .integer | .unsigned
				size:  8
			})
		})
	}
	if name == 'nil' {
		return Type(Nil{})
	}
	if name == 'none' {
		return Type(None{})
	}
	return Type(Unknown{
		reason: 'unknown builtin type'
	})
}

// builtin_type returns the Type for a builtin type name, or none otherwise.
pub fn builtin_type(name string) ?Type {
	if is_builtin_type_name(name) {
		return builtin_type_value(name)
	}
	return none
}
