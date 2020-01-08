module table

import (
	v.types
	v.ast
)

pub struct Table {
	// struct_fields map[string][]string
pub mut:
	types         []types.Type
	// type_idxs Hashmap
	type_idxs     map[string]int
	local_vars    []Var
	// fns Hashmap
	fns           map[string]Fn
	//
	unknown_calls []ast.CallExpr
	tmp_cnt       int
}

pub struct Var {
pub:
	name   string
	ti     types.TypeIdent
	is_mut bool
}

pub struct Fn {
pub:
	name      string
	args      []Var
	return_ti types.TypeIdent
}

pub fn new_table() &Table {
	// mut t := &Table{}
	// t.register_type(types.void_type)
	// t.register_type(types.int_type)
	// t.register_type(types.string_type)
	// t.register_type(types.f64_type)
	// t.register_type(types.bool_type)
	// t.register_type(types.voidptr_type)
	mut t := &Table{}
	// add dummy type at 0 so nothing can go there
	// save index check, 0 will mean not found
	t.types << types.Type{}
	t.type_idxs['dymmy_type_at_idx_0'] = 0
	return t
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

pub fn (t mut Table) register_var(v Var) {
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

pub fn (t mut Table) register_fn(new_fn Fn) {
	// println('reg fn $new_fn.name nr_args=$new_fn.args.len')
	t.fns[new_fn.name] = new_fn
}

pub fn (t mut Table) register_method(ti types.TypeIdent, new_fn Fn) bool {
	println('register method `$new_fn.name` tiname=$ti.name ')
	match t.types[ti.idx] {
		types.Struct {
			println('got struct')
		}
		else {
			return false
		}
	}
	mut struc := t.types[ti.idx] as types.Struct
	if struc.methods.len == 0 {
		struc.methods = make(0, 0, sizeof(types.Field))
	}
	println('register method `$new_fn.name` struct=$struc.name ')
	struc.methods << types.Field{
		name: new_fn.name
	}
	t.types[ti.idx] = struc
	return true
}

pub fn (t mut Table) new_tmp_var() string {
	t.tmp_cnt++
	return 'tmp$t.tmp_cnt'
}

pub fn (t &Table) struct_has_field(s &types.Struct, name string) bool {
	println('struct_has_field($s.name, $name) s.idx=$s.idx types.len=$t.types.len s.parent_idx=$s.parent_idx')
	// for typ in t.types {
	// println('$typ.idx $typ.name')
	// }
	for field in s.fields {
		if field.name == name {
			return true
		}
	}
	if s.parent_idx != 0 {
		parent := t.types[s.parent_idx] as types.Struct
		println('got parent $parent.name')
		for field in parent.fields {
			if field.name == name {
				return true
			}
		}
	}
	return false
}

pub fn (t &Table) struct_has_method(s &types.Struct, name string) bool {
	for field in s.methods {
		if field.name == name {
			return true
		}
	}
	return false
}

[inline]
pub fn (t &Table) find_type_idx(name string) int {
	return t.type_idxs[name]
}

[inline]
pub fn (t &Table) find_type(name string) ?types.Type {
	idx := t.type_idxs[name]
	if idx > 0 {
		return t.types[idx]
	}
	return none
}

pub fn (t mut Table) register_struct(typ types.Struct) int {
	println('register_struct($typ.name)')
	// existing
	existing_idx := t.type_idxs[typ.name]
	if existing_idx > 0 {
		ex_type := t.types[existing_idx]
		match ex_type {
			types.Placeholder {
				// override placeholder
				println('overriding type placeholder `$it.name` with struct')
				mut struct_type := types.Type{}
				struct_type = {
					typ |
					idx:existing_idx
				}
				t.types[existing_idx] = struct_type
				return existing_idx
			}
			types.Struct {
				return existing_idx
			}
			else {
				panic('cannot register type `$typ.name`, another type with this name exists')
			}
	}
	}
	// register
	println('registering: $typ.name')
	idx := t.types.len
	t.type_idxs[typ.name] = idx
	mut struct_type := types.Type{}
	struct_type = {
		typ |
		idx:idx,
		parent_idx:0,
	}
	t.types << struct_type
	return idx
}

pub fn (t mut Table) find_or_register_map(key_ti &types.TypeIdent, value_ti &types.TypeIdent) (int,string) {
	name := 'map_${key_ti.name}_${value_ti.name}'
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx,name
	}
	// register
	idx := t.types.len
	mut map_type := types.Type{}
	map_type = types.Map{
		name: name
		key_type_idx: key_ti.idx
		value_type_idx: value_ti.idx
	}
	t.type_idxs[name] = idx
	t.types << map_type
	return idx,name
}

pub fn (t mut Table) find_or_register_array(elem_ti &types.TypeIdent, nr_dims int) (int,string) {
	name := 'array_${elem_ti.name}' + if nr_dims > 1 { '_${nr_dims}d' } else { '' }
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx,name
	}
	// register
	idx := t.types.len
	mut array_type := types.Type{}
	array_type = types.Array{
		idx: idx
		name: name
		elem_type_idx: elem_ti.idx
		elem_is_ptr: elem_ti.is_ptr()
		nr_dims: nr_dims
	}
	t.type_idxs[name] = idx
	t.types << array_type
	return idx,name
}

pub fn (t mut Table) find_or_register_array_fixed(elem_ti &types.TypeIdent, size int, nr_dims int) (int,string) {
	name := 'array_fixed_${elem_ti.name}_${size}' + if nr_dims > 1 { '_${nr_dims}d' } else { '' }
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx,name
	}
	// register
	idx := t.types.len
	mut array_fixed_type := types.Type{}
	array_fixed_type = types.ArrayFixed{
		idx: idx
		name: name
		elem_type_idx: elem_ti.idx
		elem_is_ptr: elem_ti.is_ptr()
		size: size
		nr_dims: nr_dims
	}
	t.type_idxs[name] = idx
	t.types << array_fixed_type
	return idx,name
}

pub fn (t mut Table) find_or_register_multi_return(mr_tis []types.TypeIdent) (int,string) {
	mut name := 'multi_return'
	for mr_ti in mr_tis {
		name += '_$mr_ti.name'
	}
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx,name
	}
	// register
	idx := t.types.len
	mut mr_type := types.Type{}
	mr_type = types.MultiReturn{
		idx: idx
		name: name
		tis: mr_tis
	}
	t.type_idxs[name] = idx
	t.types << mr_type
	return idx,name
}

pub fn (t mut Table) find_or_register_variadic(variadic_ti &types.TypeIdent) (int,string) {
	name := 'variadic_$variadic_ti.name'
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx,name
	}
	// register
	idx := t.types.len
	mut variadic_type := types.Type{}
	variadic_type = types.Variadic{
		idx: idx
		ti: variadic_ti
	}
	t.type_idxs[name] = idx
	t.types << variadic_type
	return idx,name
}

pub fn (t mut Table) add_placeholder_type(name string) int {
	idx := t.types.len
	t.type_idxs[name] = t.types.len
	mut pt := types.Type{}
	pt = types.Placeholder{
		idx: idx
		name: name
	}
	println('added placeholder: $name - $idx ')
	t.types << pt
	return idx
}
