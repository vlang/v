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
	if id := ts.cache[key] {
		return id
	}
	id := ts.register(Type{ kind: .int_t, width: width })
	ts.cache[key] = id
	return id
}

pub fn (mut ts TypeStore) get_float(width int) TypeID {
	key := 'f${width}'
	if id := ts.cache[key] {
		return id
	}
	id := ts.register(Type{ kind: .float_t, width: width })
	ts.cache[key] = id
	return id
}

pub fn (mut ts TypeStore) get_ptr(elem TypeID) TypeID {
	key := 'p${elem}'
	if id := ts.cache[key] {
		return id
	}
	id := ts.register(Type{ kind: .ptr_t, elem_type: elem })
	ts.cache[key] = id
	return id
}

fn (mut ts TypeStore) register(t Type) TypeID {
	id := ts.types.len
	ts.types << t
	return id
}
