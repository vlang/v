module table

import (
	v.types
	v.ast
)

pub struct Table {
	// struct_fields map[string][]string
pub mut:
	// types         map[string]types.Type
	types      []types.Type
	type_idxs  map[string]int
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
	// typ    types.Type
	ti     types.TypeIdent
	is_mut bool
}

pub struct Fn {
pub:
	name        string
	args        []Var
	// return_type types.Type
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
	t := &Table{}
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


// pub fn (t mut Table) register_type(typ types.Type) {
// 	t.types[typ.str()] = typ
// 	// known := typ.name in t.type_idxs
// 	// idx := if known { t.type_idxs[typ.name] } 
// 	// 	else { t.types.len }
// 	// if !known {
// 	// 	t.types << typ
// 	// } else {
// 	// 	t.types[idx] = typ
// 	// }
// 	// t.type_idxs[typ.name] = idx
// 	// return idx
// }


// pub fn (t mut Table) register_type(typ types.Type) int {
// 	idx := t.types.len
// 	t2 := {typ| idx: idx}
// 	t.type_idxs[name] = idx
// 	t.types << t2
// 	return idx
// }

pub fn (t mut Table) find_or_register_map(typ types.Map) int {
	name := typ.str()
	// existing
	if name in t.type_idxs {
		return t.type_idxs[name]
	}
	// register
	idx := t.types.len
	t2 := {typ| idx: idx}
	t.type_idxs[name] = idx
	t.types << t2
	return idx
}

pub fn (t &Table) find_type_idx(name string) int {
	if name in t.type_idxs {
		return t.type_idxs[name]
	}
	return -1
}

pub fn (t mut Table) add_placeholder_type(name string) int {
	idx := t.types.len
	t.type_idxs[name] = t.types.len
	pt := types.Placeholder{
		idx: idx
		name: name
	}
	t.types << pt
	return idx
}

pub fn (t &Table) find_type(name string) ?types.Type {
	// typ := t.types[name]
	// if isnil(typ.name.str) || typ.name == '' {
	// 	return none
	// }
	// return typ
	if name in t.type_idxs {
		return t.types[t.type_idxs[name]]
	}
	return none
}

pub fn (t mut Table) new_tmp_var() string {
	t.tmp_cnt++
	return 'tmp$t.tmp_cnt'
}
