// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

pub struct Scope {
mut:
	parent    &Scope
	children  []&Scope
	start_pos int
	end_pos   int
	// vars      map[string]table.Var
	vars      map[string]VarDecl
}

pub fn new_scope(parent &Scope, start_pos int) &Scope {
	return &Scope{
		parent: parent
		start_pos: start_pos
	}
}

pub fn (s &Scope) find_scope_and_var(name string) ?(&Scope,VarDecl) {
	if name in s.vars {
		return s,s.vars[name]
	}
	for sc := s; !isnil(sc.parent); sc = sc.parent {
		if name in sc.vars {
			return sc,sc.vars[name]
		}
	}
	return none
}

pub fn (s &Scope) find_var(name string) ?VarDecl {
	if name in s.vars {
		return s.vars[name]
	}
	for sc := s; !isnil(sc.parent); sc = sc.parent {
		if name in sc.vars {
			return sc.vars[name]
		}
	}
	return none
}

pub fn (s &Scope) known_var(name string) bool {
	if _ := s.find_var(name) {
		return true
	}
	return false
}

pub fn (s mut Scope) register_var(var VarDecl) {
	if x := s.find_var(var.name) {
		// println('existing var: $var.name')
		return
	}
	s.vars[var.name] = var
}

pub fn (s mut Scope) override_var(var VarDecl) {
	s.vars[var.name] = var
}

pub fn (s &Scope) outermost() &Scope {
	mut sc := s
	for !isnil(sc.parent) {
		sc = sc.parent
	}
	return sc
}

// returns the innermost scope containing pos
pub fn (s &Scope) innermost(pos int) ?&Scope {
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
			}
			else if s1.contains(pos) {
				return s1.innermost(pos)
			}
			else {
				last = middle - 1
			}
			middle = (first + last) / 2
			if first > last {
				break
			}
		}
		return s
	}
	return none
}

/*
pub fn (s &Scope) innermost(pos int) ?&Scope {
	if s.contains(pos) {
		for s1 in s.children {
			if s1.contains(pos) {
				return s1.innermost(pos)
			}
		}
		return s
	}
	return none
}
*/


[inline]
fn (s &Scope) contains(pos int) bool {
	return pos > s.start_pos && pos < s.end_pos
}

pub fn (sc &Scope) show(level int) string {
	mut out := ''
	mut indent := ''
	for _ in 0 .. level * 4 {
		indent += ' '
	}
	out += '$indent# $sc.start_pos - $sc.end_pos\n'
	for _, var in sc.vars {
		out += '$indent  * $var.name - $var.typ\n'
	}
	for child in sc.children {
		out += child.show(level + 1)
	}
	return out
}

pub fn (sc &Scope) str() string {
	return sc.show(0)
}

