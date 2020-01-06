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
	name        string
	args        []Var
	return_ti   types.TypeIdent
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


pub fn (t mut Table) new_tmp_var() string {
	t.tmp_cnt++
	return 'tmp$t.tmp_cnt'
}
