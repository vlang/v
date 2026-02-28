// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

pub type TypeID = int

pub enum TypeKind {
	void_t
	int_t
	float_t
	ptr_t
	array_t
	struct_t
	func_t
	label_t
	metadata_t
}

pub struct Type {
pub:
	kind        TypeKind
	width       int      // Bit width
	elem_type   TypeID   // For Ptr, Array
	len         int      // For Array
	fields      []TypeID // For Structs
	field_names []string // Field names for Structs
	params      []TypeID // For Funcs
	ret_type    TypeID
	is_c_struct bool // True for C interop structs (use raw field names, typedef to C struct)
	is_union    bool // True for union types (all fields overlap at offset 0)
	is_unsigned bool // True for unsigned integer types (u8, u16, u32, u64)
}

pub struct TypeStore {
pub mut:
	types []Type
	cache map[string]TypeID
}

pub fn TypeStore.new() &TypeStore {
	mut ts := &TypeStore{}
	ts.register(Type{ kind: .void_t }) // ID 0 = Void
	return ts
}

pub fn (mut ts TypeStore) get_int(width int) TypeID {
	key := 'i${width}'
	if id := map_get_type_id(ts.cache, key) {
		return id
	}
	id := ts.register(Type{ kind: .int_t, width: width })
	ts.cache[key] = id
	return id
}

pub fn (mut ts TypeStore) get_uint(width int) TypeID {
	key := 'u${width}'
	if id := map_get_type_id(ts.cache, key) {
		return id
	}
	id := ts.register(Type{ kind: .int_t, width: width, is_unsigned: true })
	ts.cache[key] = id
	return id
}

pub fn (mut ts TypeStore) get_float(width int) TypeID {
	key := 'f${width}'
	if id := map_get_type_id(ts.cache, key) {
		return id
	}
	id := ts.register(Type{ kind: .float_t, width: width })
	ts.cache[key] = id
	return id
}

pub fn (mut ts TypeStore) get_ptr(elem TypeID) TypeID {
	key := 'p${elem}'
	if id := map_get_type_id(ts.cache, key) {
		return id
	}
	id := ts.register(Type{ kind: .ptr_t, elem_type: elem })
	ts.cache[key] = id
	return id
}

pub fn (mut ts TypeStore) get_array(elem TypeID, length int) TypeID {
	key := 'a${elem}_${length}'
	if id := map_get_type_id(ts.cache, key) {
		return id
	}
	id := ts.register(Type{ kind: .array_t, elem_type: elem, len: length })
	ts.cache[key] = id
	return id
}

pub fn (mut ts TypeStore) get_tuple(elem_types []TypeID) TypeID {
	// Create a cache key from element types
	mut key := 'tuple'
	for t in elem_types {
		key += '_${t}'
	}
	if id := map_get_type_id(ts.cache, key) {
		return id
	}
	id := ts.register(Type{
		kind:   .struct_t
		fields: elem_types
	})
	ts.cache[key] = id
	return id
}

pub fn (mut ts TypeStore) register(t Type) TypeID {
	id := ts.types.len
	ts.types << t
	return id
}
