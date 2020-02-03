// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module table
// import (
// v.ast
// )
pub struct Table {
	// struct_fields map[string][]string
pub mut:
	types      []Type
	// type_idxs Hashmap
	type_idxs  map[string]int
	local_vars []Var
	// fns Hashmap
	fns        map[string]Fn
	consts     map[string]Var
	tmp_cnt    int
	imports    []string
}

pub struct Fn {
pub:
	name        string
	args        []Var
	return_type Type
}

pub struct Var {
pub:
	name      string
	is_mut    bool
	is_const  bool
	is_global bool
	// expr   ast.Expr
mut:
	typ       Type
}

pub fn new_table() &Table {
	mut t := &Table{}
	t.register_builtin_types()
	return t
}

pub fn (t &Table) find_var_idx(name string) int {
	for i, var in t.local_vars {
		if var.name == name {
			return i
		}
	}
	return -1
}

pub fn (t &Table) find_var(name string) ?Var {
	/*
	for i in 0 .. p.var_idx {
		if p.local_vars[i].name == name {
			return p.local_vars[i]
		}
	}
	*/

	// println(t.names)
	for var in t.local_vars {
		if var.name == name {
			return var
		}
	}
	return none
}

pub fn (t mut Table) clear_vars() {
	// shared a := [1, 2, 3]
	// p.var_idx = 0
	if t.local_vars.len > 0 {
		// if p.pref.autofree {
		// p.local_vars.free()
		// }
		t.local_vars = []
	}
}

pub fn (t mut Table) register_const(v Var) {
	t.consts[v.name] = v
}

pub fn (t mut Table) register_global(name string, typ Type) {
	t.consts[name] = Var{
		name: name
		typ: typ
		is_const: true
		is_global: true
		// mod: p.mod
		// is_mut: true
		// idx: -1
		
	}
}

pub fn (t mut Table) register_var(v Var) {
	println('register_var: $v.name - $v.typ.name')
	t.local_vars << v
	/*
	mut new_var := {
		v |
		idx:p.var_idx,
		scope_level:p.cur_fn.scope_level
	}
	if v.line_nr == 0 {
		new_var.token_idx = p.cur_tok_index()
		new_var.line_nr = p.cur_tok().line_nr
	}
	// Expand the array
	if p.var_idx >= p.local_vars.len {
		p.local_vars << new_var
	}
	else {
		p.local_vars[p.var_idx] = new_var
	}
	p.var_idx++
	*/

}

pub fn (t &Table) find_fn(name string) ?Fn {
	f := t.fns[name]
	if f.name.str != 0 {
		// TODO
		return f
	}
	return none
}

pub fn (t &Table) find_const(name string) ?Var {
	f := t.consts[name]
	if f.name.str != 0 {
		// TODO
		return f
	}
	return none
}

pub fn (t mut Table) register_fn(new_fn Fn) {
	// println('reg fn $new_fn.name nr_args=$new_fn.args.len')
	t.fns[new_fn.name] = new_fn
}

pub fn (t mut Table) register_method(typ &Type, new_fn Fn) bool {
	idx := typ.idx
	println('register method `$new_fn.name` type=$typ.name idx=$typ.idx')
	mut methods := t.types[idx].methods
	methods << new_fn
	t.types[idx].methods = methods
	return true
}

pub fn (t &Table) has_method(type_idx int, name string) bool {
	t.find_method(type_idx, name) or {
		return false
	}
	return true
}

pub fn (t &Table) find_method(type_idx int, name string) ?Fn {
	for method in t.types[type_idx].methods {
		if method.name == name {
			return method
		}
	}
	return none
}

pub fn (t mut Table) new_tmp_var() string {
	t.tmp_cnt++
	return 'tmp$t.tmp_cnt'
}

pub fn (t &Table) struct_has_field(s &Type, name string) bool {
	println('struct_has_field($s.name, $name) types.len=$t.types.len s.parent_idx=$s.parent_idx')
	// for typ in t.types {
	// println('$typ.idx $typ.name')
	// }
	if _ := t.struct_find_field(s, name) {
		return true
	}
	return false
}

pub fn (t &Table) struct_find_field(s &Type, name string) ?Field {
	println('struct_find_field($s.name, $name) types.len=$t.types.len s.parent_idx=$s.parent_idx')
	info := s.info as Struct
	for field in info.fields {
		if field.name == name {
			return field
		}
	}
	if s.parent_idx != 0 {
		parent := t.types[s.parent_idx]
		parent_info := s.info as Struct
		println('got parent $parent.name')
		for field in parent_info.fields {
			if field.name == name {
				return field
			}
		}
	}
	return none
}

[inline]
pub fn (t &Table) find_type_idx(name string) int {
	return t.type_idxs[name]
}

[inline]
pub fn (t &Table) find_type(name string) ?Type {
	idx := t.type_idxs[name]
	if idx > 0 {
		return t.types[idx]
	}
	return none
}

[inline]
pub fn (t mut Table) register_type(typ Type) int {
	existing_idx := t.type_idxs[typ.name]
	if existing_idx > 0 {
		ex_type := t.types[existing_idx]
		match ex_type.kind {
			.placeholder {
				// override placeholder
				println('overriding type placeholder `$typ.name`')
				t.types[existing_idx] = {
					typ |
					methods:ex_type.methods
				}
				return existing_idx
			}
			else {
				if ex_type.kind == typ.kind {
					return existing_idx
				}
				panic('cannot register type `$typ.name`, another type with this name exists')
			}
	}
	}
	idx := t.types.len
	t.types << typ
	t.type_idxs[typ.name] = idx
	return idx
}

pub fn (t mut Table) find_or_register_map(key_typ &Type, value_typ &Type) (int,string) {
	name := 'map_${key_typ.name}_${value_typ.name}'
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx,name
	}
	// register
	map_type := Type{
		kind: .map
		name: name
		info: Map{
			key_type_idx: key_typ.idx
			value_type_idx: value_typ.idx
		}
	}
	idx := t.register_type(map_type)
	return idx,name
}

pub fn (t mut Table) find_or_register_array(elem_typ &Type, nr_dims int) (int,string) {
	name := 'array_${elem_typ.name}' + if nr_dims > 1 { '_${nr_dims}d' } else { '' }
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx,name
	}
	// register
	parent_idx := t.type_idxs['array']
	array_type := Type{
		parent_idx: parent_idx
		kind: .array
		name: name
		info: Array{
			elem_type_idx: elem_typ.idx
			elem_is_ptr: elem_typ.is_ptr()
			nr_dims: nr_dims
		}
	}
	idx := t.register_type(array_type)
	return idx,name
}

pub fn (t mut Table) find_or_register_array_fixed(elem_typ &Type, size int, nr_dims int) (int,string) {
	name := 'array_fixed_${elem_typ.name}_${size}' + if nr_dims > 1 { '_${nr_dims}d' } else { '' }
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx,name
	}
	// register
	array_fixed_type := Type{
		kind: .array_fixed
		name: name
		info: ArrayFixed{
			elem_type_idx: elem_typ.idx
			elem_is_ptr: elem_typ.is_ptr()
			size: size
			nr_dims: nr_dims
		}
	}
	idx := t.register_type(array_fixed_type)
	return idx,name
}

pub fn (t mut Table) find_or_register_multi_return(mr_typs []Type) (int,string) {
	mut name := 'multi_return'
	for mr_typ in mr_typs {
		name += '_$mr_typ.name'
	}
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx,name
	}
	// register
	mr_type := Type{
		kind: .multi_return
		name: name
		info: MultiReturn{
			tis: mr_typs
		}
	}
	idx := t.register_type(mr_type)
	return idx,name
}

pub fn (t mut Table) find_or_register_variadic(variadic_typ &Type) (int,string) {
	name := 'variadic_$variadic_typ.name'
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx,name
	}
	// register
	variadic_type := Type{
		kind: .variadic
		name: name
		info: Variadic{
			ti: variadic_typ
		}
	}
	idx := t.register_type(variadic_type)
	return idx,name
}

pub fn (t mut Table) add_placeholder_type(name string) int {
	ph_type := Type{
		kind: .placeholder
		name: name
	}
	idx := t.register_type(ph_type)
	println('added placeholder: $name - $idx')
	return idx
}

// [inline]
// pub fn (t &Table) update_typ(ti &types.Type) types.Type {
// if typ.kind == .unresolved {
// }
// }
pub fn (t &Table) check(got, expected &Type) bool {
	println('check: $got.name, $expected.name')
	if expected.kind == .voidptr {
		return true
	}
	// if expected.name == 'array' {
	// return true
	// }
	if got.idx != expected.idx && got.name != expected.name {
		return false
	}
	return true
}

/*
[inline]
pub fn (t &Table) get_expr_typ(expr ast.Expr) Type {
	match expr {
		ast.ArrayInit {
			return it.typ
		}
		ast.IndexExpr {
			return t.get_expr_typ(it.left)
		}
		ast.CallExpr {
			func := t.find_fn(it.name) or {
				return void_typ
			}
			return func.return_typ
		}
		ast.MethodCallExpr {
			ti := t.get_expr_typ(it.expr)
			func := t.find_method(typ.idx, it.name) or {
				return void_type
			}
			return func.return_typ
		}
		ast.Ident {
			if it.kind == .variable {
				info := it.info as ast.IdentVar
				if info.typ.kind != .unresolved {
					return info.ti
				}
				return t.get_expr_typ(info.expr)
			}
			return types.void_typ
		}
		ast.StructInit {
			return it.ti
		}
		ast.StringLiteral {
			return types.string_typ
		}
		ast.IntegerLiteral {
			return types.int_typ
		}
		ast.SelectorExpr {
			ti := t.get_expr_typ(it.expr)
			kind := t.types[typ.idx].kind
			if typ.kind == .placeholder {
				println(' ##### PH $typ.name')
			}
			if !(kind in [.placeholder, .struct_]) {
				return types.void_typ
			}
			struct_ := t.types[typ.idx]
			struct_info := struct_.info as types.Struct
			for field in struct_info.fields {
				if field.name == it.field {
					return field.ti
				}
			}
			if struct_.parent_idx != 0 {
				parent := t.types[struct_.parent_idx]
				parent_info := parent.info as types.Struct
				for field in parent_info.fields {
					if field.name == it.field {
						return field.ti
					}
				}
			}
			return types.void_typ
		}
		ast.InfixExpr {
			return t.get_expr_typ(it.left)
		}
		else {
			return types.void_typ
		}
	}
}
*/
