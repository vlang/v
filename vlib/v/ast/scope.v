// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

@[heap]
pub struct Scope {
pub mut:
	// mut:
	objects              map[string]ScopeObject
	struct_fields        map[string]ScopeStructField
	parent               &Scope = unsafe { nil }
	detached_from_parent bool
	children             []&Scope
	start_pos            int
	end_pos              int
}

@[unsafe]
pub fn (s &Scope) free() {
	if s == unsafe { nil } {
		return
	}
	unsafe {
		s.objects.free()
		s.struct_fields.free()
		for child in s.children {
			child.free()
		}
		s.children.free()
	}
}

/*
pub fn new_scope(parent &Scope, start_pos int) &Scope {
	return &Scope{
		parent: parent
		start_pos: start_pos
	}
}
*/

@[inline]
fn (s &Scope) dont_lookup_parent() bool {
	return s.parent == unsafe { nil } || s.detached_from_parent
}

pub fn (s &Scope) find(name string) ?ScopeObject {
	if s == unsafe { nil } {
		return none
	}
	for sc := unsafe { s }; true; sc = sc.parent {
		if name in sc.objects {
			return unsafe { sc.objects[name] }
		}
		if sc.dont_lookup_parent() {
			break
		}
	}
	return none
}

// selector_expr:  name.field_name
pub fn (s &Scope) find_struct_field(name string, struct_type Type, field_name string) &ScopeStructField {
	if s == unsafe { nil } {
		return unsafe { nil }
	}
	k := '${name}.${field_name}'
	for sc := unsafe { s }; true; sc = sc.parent {
		if field := sc.struct_fields[k] {
			if field.struct_type == struct_type {
				return &ScopeStructField{
					...field
				}
			}
		}
		if sc.dont_lookup_parent() {
			break
		}
	}
	return unsafe { nil }
}

pub fn (s &Scope) find_var(name string) ?&Var {
	if obj := s.find(name) {
		match obj {
			Var { return &obj }
			else {}
		}
	}
	return none
}

pub fn (s &Scope) find_global(name string) ?&GlobalField {
	if obj := s.find(name) {
		match obj {
			GlobalField { return &obj }
			else {}
		}
	}
	return none
}

pub fn (s &Scope) find_const(name string) ?&ConstField {
	if obj := s.find(name) {
		match obj {
			ConstField { return &obj }
			else {}
		}
	}
	return none
}

pub fn (s &Scope) known_var(name string) bool {
	if s == unsafe { nil } {
		return false
	}
	for sc := unsafe { s }; true; sc = sc.parent {
		if name in sc.objects {
			obj := unsafe { sc.objects[name] or { empty_scope_object } }
			if obj is Var {
				return true
			}
		}
		if sc.dont_lookup_parent() {
			break
		}
	}
	return false
}

pub fn (s &Scope) known_global(name string) bool {
	s.find_global(name) or { return false }
	return true
}

pub fn (s &Scope) known_const(name string) bool {
	s.find_const(name) or { return false }
	return true
}

pub fn (mut s Scope) update_var_type(name string, typ Type) {
	mut obj := unsafe { s.objects[name] }
	if mut obj is Var {
		if obj.typ != typ {
			obj.typ = typ
		}
	}
}

pub fn (mut s Scope) update_ct_var_kind(name string, kind ComptimeVarKind) {
	mut obj := unsafe { s.objects[name] }
	if mut obj is Var {
		obj.ct_type_var = kind
	}
}

pub fn (mut s Scope) update_smartcasts(name string, typ Type, is_unwrapped bool) {
	mut obj := unsafe { s.objects[name] }
	if mut obj is Var {
		obj.smartcasts = [typ]
		obj.is_unwrapped = is_unwrapped
	}
}

// selector_expr:  name.field_name
pub fn (mut s Scope) register_struct_field(name string, field ScopeStructField) {
	k := '${name}.${field.name}'
	if f := s.struct_fields[k] {
		if f.struct_type == field.struct_type {
			return
		}
	}
	s.struct_fields[k] = field
}

pub fn (mut s Scope) register(obj ScopeObject) {
	if !(obj.name == '_' || obj.name in s.objects) {
		s.objects[obj.name] = obj
	}
}

// returns the innermost scope containing pos
// pub fn (s &Scope) innermost(pos int) ?&Scope {
@[direct_array_access]
pub fn (s &Scope) innermost(pos int) &Scope {
	if s.contains(pos) {
		// binary search
		mut first := 0
		mut last := s.children.len - 1
		mut middle := last / 2
		for first <= last {
			// println('FIRST: $first, LAST: $last, LEN: $s.children.len-1')
			s1 := s.children[middle]
			if s1.end_pos < pos {
				first = middle + 1
			} else if s1.contains(pos) {
				return s1.innermost(pos)
			} else {
				last = middle - 1
			}
			middle = (first + last) / 2
			if first > last {
				break
			}
		}
		return s
	}
	// return none
	return s
}

// get_all_vars extracts all current scope vars
pub fn (s &Scope) get_all_vars() []ScopeObject {
	if s == unsafe { nil } {
		return []
	}
	mut scope_vars := []ScopeObject{}
	for sc := unsafe { s }; true; sc = sc.parent {
		if sc.objects.len > 0 {
			scope_vars << sc.objects.values().filter(|it| it is Var)
		}
		if sc.dont_lookup_parent() {
			break
		}
	}
	return scope_vars
}

@[inline]
pub fn (s &Scope) contains(pos int) bool {
	return pos >= s.start_pos && pos <= s.end_pos
}

pub fn (s &Scope) has_inherited_vars() bool {
	for _, obj in s.objects {
		if obj is Var {
			if obj.is_inherited {
				return true
			}
		}
	}
	return false
}

pub fn (s &Scope) is_inherited_var(var_name string) bool {
	for _, obj in s.objects {
		if obj is Var {
			if obj.is_inherited && obj.name == var_name {
				return true
			}
		}
	}
	return false
}

pub fn (sc &Scope) show(depth int, max_depth int) string {
	mut out := ''
	mut indent := ''
	for _ in 0 .. depth * 4 {
		indent += ' '
	}
	out += '${indent}# ${sc.start_pos} - ${sc.end_pos}\n'
	for _, obj in sc.objects {
		match obj {
			ConstField { out += '${indent}  * const: ${obj.name} - ${obj.typ}\n' }
			Var { out += '${indent}  * var: ${obj.name} - ${obj.typ}\n' }
			else {}
		}
	}
	for _, field in sc.struct_fields {
		out += '${indent}  * struct_field: ${field.struct_type} ${field.name} - ${field.typ}\n'
	}
	if max_depth == 0 || depth < max_depth - 1 {
		for i, _ in sc.children {
			out += sc.children[i].show(depth + 1, max_depth)
		}
	}
	return out
}

pub fn (mut sc Scope) mark_var_as_used(varname string) bool {
	mut obj := sc.find(varname) or { return false }
	if mut obj is Var {
		obj.is_used = true
		return true
	} else if obj is GlobalField {
		return true
	}
	return false
}

pub fn (sc &Scope) str() string {
	return sc.show(0, 0)
}
