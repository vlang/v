// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

pub type Object = Const | Fn | Global | Module | SmartCastSelector | Type

// pub type Object = Const | Fn | Global | Module |
// 	Alias | Array | Enum | Map | Pointer | Primitive | String | Struct | SumType

struct SmartCastSelector {
	origin    Type
	field     string
	cast_type Type
}

@[heap]
pub struct Scope {
pub:
	parent &Scope = unsafe { nil }
pub mut:
	objects map[string]Object
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

// TODO: try implement the alternate method I was experimenting with (SmartCastSelector)
// i'm not sure if it is actually possible though. need to explore it.
pub fn (s &Scope) lookup_field_smartcast(name string) ?Type {
	if name in s.field_smartcasts {
		return s.field_smartcasts[name] or { return none }
	}
	if s.parent != unsafe { nil } {
		return s.parent.lookup_field_smartcast(name)
	}
	return none
}

pub fn (s &Scope) lookup(name string) ?Object {
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
	}
}
