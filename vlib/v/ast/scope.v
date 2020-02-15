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
	//vars      map[string]table.Var
	vars      map[string]VarDecl
}

pub fn new_scope(parent &Scope, start_pos int) &Scope {
	return &Scope{
		parent: parent
		start_pos: start_pos
	}
}

[inline]
pub fn (s &Scope) find_scope_and_var(name string) ?(&Scope,VarDecl) {
	if name in s.vars {
		return s,s.vars[name]
	}
	for sc := s; !isnil(sc.parent); sc = sc.parent {
		if name in sc.vars {
			return sc,sc.vars[name]
		}
	}
	return error('not found')
}

[inline]
pub fn (s &Scope) find_var(name string) ?VarDecl {
//pub fn (s &Scope) find_var(name string) ?table.Var {
	if name in s.vars {
		return s.vars[name]
	}
	for sc := s; !isnil(sc.parent); sc = sc.parent {
		if name in sc.vars {
			return sc.vars[name]
		}
	}
	return error('not found')
}

/*
[inline]
pub fn (s &Scope) find_var2(name string, pos int) ?VarDecl {
	return find_var_in_scope(name, pos, s)
}

[inline]
fn find_var_in_scope(name string, pos int, scope &Scope) ?VarDecl {
	if pos != 0 && (pos < scope.start_pos || pos > scope.end_pos) {
		return none
	}
	if name in scope.vars {
		return scope.vars[name]
	}
	for child in scope.children {
		//if pos < child.start_pos || pos > child.end_pos {
		//	continue
		//}
		var := find_var_in_scope(name, pos, child) or {
			continue
		}
		return var
		//return find_var_in_scope(name, pos, child)
	}
	return none
}
*/

//pub fn (s mut Scope) register_var(var table.Var) {
[inline]
pub fn (s mut Scope) register_var(var VarDecl) {
	if x := s.find_var(var.name) {
		println('existing var: $var.name')
		return
	}
	s.vars[var.name] = var
}

pub fn (s &Scope) innermost(pos int) ?&Scope {
	if s.contains(pos) {
		/*	
		for s1 in s.children {
			if s1.contains(pos) {
				return s1.innermost(pos)
			}
		}
		return s
		*/
		// binary search
		mut first := 0
		mut last := s.children.len-1
		mut middle := last/2
		for first <= last {
			//println('FIRST: $first, LAST: $last, LEN: $s.children.len-1')
			s1 := s.children[middle]
			if s1.end_pos < pos {
				first = middle+1
			}
			else if s1.contains(pos) {
				return s1.innermost(pos)
			}
			else {
				last = middle-1
			}
			middle = (first+last)/2
			if first > last {
				break
			}
		}
		return s
	}
	return s
	//return error('none')
	//return none
}

[inline]
fn (s &Scope) contains(pos int) bool {
	return pos > s.start_pos && pos < s.end_pos
}

pub fn print_scope_vars(sc &Scope, level int) {
	mut indent := ''
	for _ in 0..level*4 {
		indent += ' '
	}
	println('$indent# $sc.start_pos - $sc.end_pos')
	for _, var in sc.vars {
		println('$indent  * $var.name - $var.typ')
	}
	for child in sc.children {
		print_scope_vars(&child, level+1)
	}
}
