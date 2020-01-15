module table

import (
	v.ast
	v.types
)

pub struct Table {
	// struct_fields map[string][]string
pub mut:
	types         []types.Type
	// type_idxs Hashmap
	type_idxs     map[string]int
	type_kinds    []types.Kind
	local_vars    []Var
	// fns Hashmap
	fns           map[string]Fn
	methods       [][]Fn
	tmp_cnt       int
}

pub struct Var {
pub:
	name   string
	is_mut bool
	expr   ast.Expr
mut:
	ti     types.TypeIdent
}

pub struct Fn {
pub:
	name      string
	args      []Var
	return_ti types.TypeIdent
}

pub fn new_table() &Table {
	mut t := &Table{}
	t.register_builtin_types()
	return t
}

pub fn (t mut Table) register_builtin_types() {
	// add dummy type at 0 so nothing can go there
	// save index check, 0 will mean not found
	t.register_type(types.Type{}, .placeholder, 'dymmy_type_at_idx_0')
	t.register_type(types.Primitive{kind: .void}, .void, 'void')
	t.register_type(types.Primitive{kind: .voidptr}, .voidptr, 'voidptr')
	t.register_type(types.Primitive{kind: .charptr}, .charptr, 'charptr')
	t.register_type(types.Primitive{kind: .byteptr}, .byteptr, 'byteptr')
	t.register_type(types.Int{8,false}, .i8, 'i8')
	t.register_type(types.Int{16,false}, .i16, 'i16')
	t.register_type(types.Int{32,false}, .int, 'int')
	t.register_type(types.Int{64,false}, .i64,  'i64')
	t.register_type(types.Int{16,true}, .u16, 'u16')
	t.register_type(types.Int{32,true}, .u32, 'u32')
	t.register_type(types.Int{64, true}, .u64, 'u64')
	t.register_type(types.Float{32}, .f32, 'f32')
	t.register_type(types.Float{64}, .f64, 'f64')
	t.register_type(types.String{}, .string, 'string')
	t.register_type(types.Primitive{kind: .char}, .char, 'char')
	t.register_type(types.Primitive{kind: .byte}, .byte, 'byte')
	t.register_type(types.Bool{}, .bool, 'bool')
}

pub fn (t &Table) find_var_idx(name string) int {
	for i,var in t.local_vars {
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

pub fn (t mut Table) register_var(v Var) {
	println('register_var: $v.name - $v.ti.name')
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
	/*
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
	*/

	println('register method `$new_fn.name` struct=$ti.name ')
	println('##### $ti.idx - $t.methods.len')
	t.methods[ti.idx] << new_fn
	return true
}

pub fn (t mut Table) new_tmp_var() string {
	t.tmp_cnt++
	return 'tmp$t.tmp_cnt'
}

pub fn (t &Table) struct_has_field(s &types.Struct, name string) bool {
	// println('struct_has_field($s.name, $name) s.idx=$s.idx types.len=$t.types.len s.parent_idx=$s.parent_idx')
	println('struct_has_field($s.name, $name) types.len=$t.types.len s.parent_idx=$s.parent_idx')
	// for typ in t.types {
	// println('$typ.idx $typ.name')
	// }
	if _ := t.struct_find_field(s, name) {
		return true
	}
	return false
}

pub fn (t &Table) struct_find_field(s &types.Struct, name string) ?types.Field {
	println('struct_find_field($s.name, $name) types.len=$t.types.len s.parent_idx=$s.parent_idx')
	for field in s.fields {
		if field.name == name {
			return field
		}
	}
	if s.parent_idx != 0 {
		parent := t.types[s.parent_idx] as types.Struct
		println('got parent $parent.name')
		for field in parent.fields {
			if field.name == name {
				return field
			}
		}
	}
	return none
}

pub fn (t &Table) has_method(type_idx int, name string) bool {
	for field in t.methods[type_idx] {
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

[inline]
pub fn (t mut Table) register_type(typ types.Type, kind types.Kind, name string) int {
	idx := t.types.len
	t.types << typ
	t.type_idxs[name] = idx
	t.type_kinds << kind
	e := []Fn
	t.methods << e // TODO [] breaks V
	return idx
}

/*
pub fn (t mut Table) update_placeholder(typ types.Type, kind, idx) {
	ex_idx := t.type_idxs[name]
	ex_kind := t.type_kinds[ex_id]
	// other type with same name exists
	if ex_idx > 0 && !(ex_kind in [.placeholder, kind]) {
		panic('cannot register type `$typ.name`, another type with this name exists')
	}
	// type was declared and placeholder existed, update placehlder to actual type
	if ex_idx > 0 && ex_kind == .placeholder {
		t.types[idx] = typ
		t.type_kinds[idx] = .kind
	}
}
*/

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
				t.types[existing_idx] = typ
				// t.type_kinds[existing_idx] = .struct_
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
	struct_type := {
		typ |
		parent_idx:0,
	}
	idx := t.register_type(struct_type, .struct_, typ.name)
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
	map_type := types.Map{
		name: name
		key_type_idx: key_ti.idx
		value_type_idx: value_ti.idx
	}
	idx := t.register_type(map_type, .map, name)
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
	parent_idx := t.type_idxs['array']
	array_type := types.Array{
		parent_idx: parent_idx
		name: name
		elem_type_idx: elem_ti.idx
		elem_is_ptr: elem_ti.is_ptr()
		nr_dims: nr_dims
	}
	idx := t.register_type(array_type, .array, name)
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
	array_fixed_type := types.ArrayFixed{
		name: name
		elem_type_idx: elem_ti.idx
		elem_is_ptr: elem_ti.is_ptr()
		size: size
		nr_dims: nr_dims
	}
	idx := t.register_type(array_fixed_type, .array_fixed, name)
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
	mr_type := types.MultiReturn{
		name: name
		tis: mr_tis
	}
	idx := t.register_type(mr_type, .multi_return, name)
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
	variadic_type := types.Variadic{
		ti: variadic_ti
	}
	idx := t.register_type(variadic_type, .variadic, name)
	return idx,name
}

pub fn (t mut Table) add_placeholder_type(name string) int {
	ph_type := types.Placeholder{
		name: name
	}
	idx := t.register_type(ph_type, .placeholder, name)
	println('added placeholder: $name - $idx')
	return idx
}

//pub fn (t mut Table) check(tok &token.Token, got, expected &types.TypeIdent) bool {
pub fn (t &Table) check(got, expected &types.TypeIdent) bool {
	println('check: $got.name, $expected.name')
	if expected.kind == .voidptr {
		return true
	}
	//if expected.name == 'array' {
	//	return true
	//}
	if got.idx != expected.idx {
		return false
	}
	return true
}
