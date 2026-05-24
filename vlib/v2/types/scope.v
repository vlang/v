// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

pub type Object = Const | Fn | Global | Module | SmartCastSelector | Type | TypeObject

// pub type Object = Const | Fn | Global | Module |
// 	Alias | Array | Enum | Map | Pointer | Primitive | String | Struct | SumType

struct SmartCastSelector {
	origin    Type
	field     string
	cast_type Type
}

pub struct TypeObject {
pub:
	typ Type
}

@[heap]
pub struct Scope {
pub:
	parent &Scope = unsafe { nil }
pub mut:
	objects map[string]Object
	types   map[string]Type
	// TODO: try implement using original concept
	field_smartcasts map[string]Type
	// smartcasts map[string]Type
	// TODO: it may be more efficient looking up local vars using an ID
	// even if we had to store them in two different places. investigate.
	// variables []Object
	start int
	end   int
}

pub fn new_scope(parent &Scope) &Scope {
	unsafe {
		return &Scope{
			parent: parent
		}
	}
}

// same_scope_ptr compares scope identity by address instead of structural equality.
pub fn same_scope_ptr(a &Scope, b &Scope) bool {
	return voidptr(a) == voidptr(b)
}

// TODO: try implement the alternate method I was experimenting with (SmartCastSelector)
// i'm not sure if it is actually possible though. need to explore it.
pub fn (s &Scope) lookup_field_smartcast(name string) ?Type {
	if !scope_lookup_string_is_valid(name) {
		return none
	}
	if name in s.field_smartcasts {
		return s.field_smartcasts[name] or { return none }
	}
	if s.parent != unsafe { nil } {
		return s.parent.lookup_field_smartcast(name)
	}
	return none
}

pub fn (s &Scope) lookup(name string) ?Object {
	if !scope_lookup_string_is_valid(name) {
		return none
	}
	if name in s.objects {
		return s.objects[name] or { return none }
	}
	return none
}

pub fn (s &Scope) lookup_parent(name string, pos int) ?Object {
	if obj := s.lookup(name) {
		return obj
		// if !pos.is_valid() || cmpPos(obj.scopePos(), pos) <= 0 {
		// 	return s, obj
		// }
	}
	if s.parent != unsafe { nil } {
		if parent_obj := s.parent.lookup_parent(name, pos) {
			return parent_obj
		}
	}
	// println('lookup_parent: NOT FOUND: ${name}')
	return none
}

pub fn (s &Scope) lookup_type(name string) ?Type {
	if !scope_lookup_string_is_valid(name) {
		return none
	}
	if name in s.types {
		return s.types[name] or { return none }
	}
	return none
}

pub fn (s &Scope) lookup_type_parent(name string, pos int) ?Type {
	if typ := s.lookup_type(name) {
		return typ
		// if !pos.is_valid() || cmpPos(obj.scopePos(), pos) <= 0 {
		// 	return s, obj
		// }
	}
	if s.parent != unsafe { nil } {
		if parent_typ := s.parent.lookup_type_parent(name, pos) {
			return parent_typ
		}
	}
	return none
}

// lookup_var_type looks up a variable by name and returns its type.
// Walks up the scope chain to find the variable.
pub fn (s &Scope) lookup_var_type(name string) ?Type {
	if obj := s.lookup_parent(name, 0) {
		return obj.typ()
	}
	return none
}

pub fn (s &Scope) lookup_parent_with_scope(name string, pos int) ?(&Scope, Object) {
	if obj := s.lookup(name) {
		return s, obj
		// if !pos.is_valid() || cmpPos(obj.scopePos(), pos) <= 0 {
		// 	return s, obj
		// }
	}
	if s.parent != unsafe { nil } {
		if parent_scope, parent_obj := s.parent.lookup_parent_with_scope(name, pos) {
			return parent_scope, parent_obj
		}
	}
	// println('lookup_parent: NOT FOUND: ${name}')
	return none
}

pub fn (mut s Scope) insert(name string, obj Object) {
	if !scope_lookup_string_is_valid(name) {
		return
	}
	trace_scope_fixed_array_object('insert_in', name, obj)
	if typ := object_decl_type(obj) {
		s.types[name] = typ
	}
	if name in s.objects {
		existing := s.objects[name] or { return }
		// Module scopes pre-register a self-module placeholder so code can
		// reference `mod_name.CONST` from inside the same module. A real symbol
		// with the same name should override that placeholder.
		if existing is Module && obj !is Module {
			s.objects[name] = obj
		}
		return
	}
	s.objects[name] = obj
	if stored := s.objects[name] {
		trace_scope_fixed_array_object('insert_out', name, stored)
	}
}

// insert_or_update always overwrites an existing entry. Used for fn_root_scope
// where variables from nested scopes must be updated when re-declared.
pub fn (mut s Scope) insert_or_update(name string, obj Object) {
	if !scope_lookup_string_is_valid(name) {
		return
	}
	trace_scope_fixed_array_object('insert_update_in', name, obj)
	if typ := object_decl_type(obj) {
		s.types[name] = typ
	}
	s.objects[name] = obj
	if stored := s.objects[name] {
		trace_scope_fixed_array_object('insert_update_out', name, stored)
	}
}

fn trace_scope_fixed_array_object(label string, name string, obj Object) {
	if name !in ['g_autostr_type_stack', 'g_autostr_addr_stack', 'g_v_os_execute_mutex_storage'] {
		return
	}
	match obj {
		Global {
			trace_scope_fixed_array_type(label, name, obj.typ)
		}
		TypeObject {
			trace_scope_fixed_array_type(label, name, obj.typ)
		}
		Type {
			trace_scope_fixed_array_type(label, name, obj)
		}
		else {}
	}
}

fn trace_scope_fixed_array_type(label string, name string, typ Type) {
	if typ is ArrayFixed {
		arr := typ as ArrayFixed
		eprintln('SCOPE_TRACE ${label} name=${name} len=${arr.len} elem=${arr.elem_type.name()}')
	}
}

pub fn (mut s Scope) insert_type(name string, typ Type) {
	if !scope_lookup_string_is_valid(name) {
		return
	}
	s.types[name] = typ
}

fn object_decl_type(obj Object) ?Type {
	match obj {
		Type {
			return obj
		}
		else {}
	}

	return none
}

fn scope_lookup_string_is_valid(s string) bool {
	if s.len <= 0 || s.len > 512 {
		return false
	}
	ptr := unsafe { u64(voidptr(s.str)) }
	return ptr > 4096
}

pub fn (s &Scope) print(recurse_parents bool) {
	println('# SCOPE:')
	for name, obj in s.objects {
		println(' * ${name}: ${obj.type_name()}')
		// if obj is Type {
		// 	println('    - ${name}: ${obj.type_name()}')
		// }
	}
	if recurse_parents && s.parent != unsafe { nil } {
		s.parent.print(recurse_parents)
	}
}

pub fn (obj &Object) typ() Type {
	match obj {
		Const {
			return obj.typ
		}
		Fn {
			return obj.typ
		}
		Global {
			return obj.typ
		}
		Module {
			// TODO: modules don't have a type, return a placeholder
			return Type(u16_)
		}
		SmartCastSelector {
			return obj.origin
		}
		Type {
			return obj
		}
		TypeObject {
			return obj.typ
		}
	}
}
