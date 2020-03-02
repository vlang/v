module table

pub type Type int

pub enum TypeExtra {
	unset
	optional
	variadic
}

pub fn (types []Type) contains(typ Type) bool {
	for t in types {
		if int(typ) == int(t) {
			return true
		}
	}
	return false
}

// return underlying TypeSymbol idx
[inline]
pub fn type_idx(t Type) int {
	return u16(t) & 0xffff
}

// return nr_muls
[inline]
pub fn type_nr_muls(t Type) int {
	return (int(t)>>16) & 0xff
}

// return true if pointer (nr_muls>0)
[inline]
pub fn type_is_ptr(t Type) bool {
	return type_nr_muls(t) > 0
}

// set nr_muls on Type and return it
[inline]
pub fn type_set_nr_muls(t Type, nr_muls int) Type {
	if nr_muls < 0 || nr_muls > 255 {
		panic('typ_set_nr_muls: nr_muls must be between 0 & 255')
	}
	return (int(type_extra(t))<<24) | (nr_muls<<16) | u16(type_idx(t))
}

// increments nr_nuls on Type and return it
[inline]
pub fn type_to_ptr(t Type) Type {
	nr_muls := type_nr_muls(t)
	if nr_muls == 255 {
		panic('type_to_pre: nr_muls is already at max of 255')
	}
	return (int(type_extra(t))<<24) | ((nr_muls + 1)<<16) | u16(type_idx(t))
}

// decrement nr_muls on Type and return it
[inline]
pub fn type_deref(t Type) Type {
	nr_muls := type_nr_muls(t)
	if nr_muls == 0 {
		panic('deref: type `$t` is not a pointer')
	}
	return (int(type_extra(t))<<24) | ((nr_muls - 1)<<16) | u16(type_idx(t))
}

// return extra info
[inline]
pub fn type_extra(t Type) TypeExtra {
	return (int(t)>>24) & 0xff
}

// set extra info
[inline]
pub fn type_set_extra(t Type, extra TypeExtra) Type {
	return (int(extra)<<24) | (type_nr_muls(t)<<16) | u16(type_idx(t))
}

[inline]
pub fn type_is_optional(t Type) bool {
	return type_extra(t) == .optional
}

[inline]
pub fn type_to_optional(t Type) Type {
	return type_set_extra(t, .optional)
}

[inline]
pub fn type_is_variadic(t Type) bool {
	return type_extra(t) == .variadic
}

[inline]
pub fn type_to_variadic(t Type) Type {
	return type_set_extra(t, .variadic)
}

// new type with idx of TypeSymbol, not pointer (nr_muls=0)
[inline]
pub fn new_type(idx int) Type {
	if idx < 1 || idx > 65536 {
		panic('new_type_id: idx must be between 1 & 65536')
	}
	return idx
}

// return Type idx of TypeSymbol & specify if ptr (nr_muls)
[inline]
pub fn new_type_ptr(idx int, nr_muls int) Type {
	if idx < 1 || idx > 65536 {
		panic('typ_ptr: idx must be between 1 & 65536')
	}
	if nr_muls < 0 || nr_muls > 255 {
		panic('typ_ptr: nr_muls must be between 0 & 255')
	}
	return (nr_muls<<16) | u16(idx)
}

pub const (
	number_idxs = [int_type_idx, byte_type_idx, u16_type_idx, i16_type_idx, i64_type_idx, u32_type_idx, u64_type_idx]
)
/*
pub fn is_number(typ Type) bool {
	typ_sym := c.table.get_type_symbol(typ)
	return typ_sym.is_int()
	//idx := type_idx(typ)
	//return idx in [int_type_idx, byte_type_idx, u64_type_idx]
}
*/


pub const (
	void_type = new_type(void_type_idx)
	voidptr_type = new_type(voidptr_type_idx)
	byteptr_type = new_type(byteptr_type_idx)
	charptr_type = new_type(charptr_type_idx)
	i8_type = new_type(i8_type_idx)
	int_type = new_type(int_type_idx)
	i16_type = new_type(i16_type_idx)
	i64_type = new_type(i64_type_idx)
	byte_type = new_type(byte_type_idx)
	u16_type = new_type(u16_type_idx)
	u32_type = new_type(u32_type_idx)
	u64_type = new_type(u64_type_idx)
	f32_type = new_type(f32_type_idx)
	f64_type = new_type(f64_type_idx)
	char_type = new_type(char_type_idx)
	bool_type = new_type(bool_type_idx)
	none_type = new_type(none_type_idx)
	string_type = new_type(string_type_idx)
	array_type = new_type(array_type_idx)
	map_type = new_type(map_type_idx)
)
