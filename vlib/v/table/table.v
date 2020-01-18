module table

import (
	v.ast
	v.types
)

pub struct Table {
	// struct_fields map[string][]string
pub mut:
	types         []Type
	// type_idxs Hashmap
	type_idxs     map[string]int
	local_vars    []Var
	// fns Hashmap
	fns           map[string]Fn
	tmp_cnt       int
	imports       []string
}

pub struct Fn {
pub:
	name      string
	args      []Var
	return_ti types.TypeIdent
}

pub struct Var {
pub:
	name   string
	is_mut bool
	expr   ast.Expr
mut:
	ti     types.TypeIdent
}

pub struct Type {
pub:
	parent_idx int
mut:
	info    types.TypeInfo
	kind    types.Kind
	name    string
	methods []Fn
}

pub fn (t Type) str() string {
	return t.name
}

pub fn new_table() &Table {
	mut t := &Table{}
	t.register_builtin_types()
	return t
}

pub fn (t mut Table) register_builtin_types() {
	// reserve index 0 so nothing can go there
	// save index check, 0 will mean not found
	t.register_type(Type{kind: .placeholder, name: 'reserved_0'})
	t.register_type(Type{kind: .void, name: 'void'})
	t.register_type(Type{kind: .voidptr, name: 'voidptr'})
	t.register_type(Type{kind: .charptr, name: 'charptr'})
	t.register_type(Type{kind: .byteptr, name: 'byteptr'})
	t.register_type(Type{kind: .i8, name: 'i8'})
	t.register_type(Type{kind: .i16, name: 'i16'})
	t.register_type(Type{kind: .int, name: 'int'})
	t.register_type(Type{kind: .i64, name: 'i64'})
	t.register_type(Type{kind: .u16, name: 'u16'})
	t.register_type(Type{kind: .u32, name: 'u32'})
	t.register_type(Type{kind: .u64, name: 'u64'})
	t.register_type(Type{kind: .f32, name: 'f32'})
	t.register_type(Type{kind: .f64, name: 'f64'})
	t.register_type(Type{kind: .string, name: 'string'})
	t.register_type(Type{kind: .char, name: 'char'})
	t.register_type(Type{kind: .byte, name: 'byte'})
	t.register_type(Type{kind: .bool, name: 'bool'})
}

pub fn (t &Table) refresh_ti(ti types.TypeIdent) types.TypeIdent {
	if ti.idx == 0 {
		return ti
	}
	if ti.kind in [.placeholder, .unresolved] {
		typ := t.types[ti.idx]
		return { ti| 
			kind: typ.kind,
			name: typ.name
		}
	}
	return ti
}

pub fn (t &Table) get_type(idx int) Type {
	if idx == 0 {
		panic('get_type: idx 0')
	}
	return t.types[idx]
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

pub fn (t mut Table) register_method(ti &types.TypeIdent, new_fn Fn) bool {
	idx := ti.idx
	println('register method `$new_fn.name` type=$ti.name idx=$ti.idx')
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

pub fn (t &Table) struct_find_field(s &Type, name string) ?types.Field {
	println('struct_find_field($s.name, $name) types.len=$t.types.len s.parent_idx=$s.parent_idx')
	info := s.info as types.Struct
	for field in info.fields {
		if field.name == name {
			return field
		}
	}
	if s.parent_idx != 0 {
		parent := t.types[s.parent_idx]
		parent_info := s.info as types.Struct
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
				t.types[existing_idx] = {typ|
					methods: ex_type.methods
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

pub fn (t mut Table) find_or_register_map(key_ti &types.TypeIdent, value_ti &types.TypeIdent) (int,string) {
	name := 'map_${key_ti.name}_${value_ti.name}'
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx,name
	}
	// register
	map_type := Type{
		kind: .map
		name: name
		info: types.Map{
			key_type_idx: key_ti.idx
			value_type_idx: value_ti.idx
		}
	}
	idx := t.register_type(map_type)
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
	array_type := Type{
		parent_idx: parent_idx
		kind: .array
		name: name
		info: types.Array{
			elem_type_idx: elem_ti.idx
			elem_is_ptr: elem_ti.is_ptr()
			nr_dims: nr_dims
		}
	}
	idx := t.register_type(array_type)
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
	array_fixed_type := Type{
		kind: .array_fixed
		name: name
		info: types.ArrayFixed{
			elem_type_idx: elem_ti.idx
			elem_is_ptr: elem_ti.is_ptr()
			size: size
			nr_dims: nr_dims
		}
	}
	idx := t.register_type(array_fixed_type)
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
	mr_type := Type{
		kind: .multi_return
		name: name
		info: types.MultiReturn{
			tis: mr_tis
		}
	}
	idx := t.register_type(mr_type)
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
	variadic_type := Type{
		kind: .variadic
		name: name
		info: types.Variadic{
			ti: variadic_ti
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
// pub fn (t &Table) update_ti(ti &types.TypeIdent) types.TypeIdent {
// 	if ti.kind == .unresolved {
		
// 	}
// }

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

[inline]
pub fn (t &Table) get_expr_ti(expr ast.Expr) types.TypeIdent {
	match expr {
		ast.ArrayInit{
			return it.ti
		}
		ast.IndexExpr{
			return t.get_expr_ti(it.left)
		}
		ast.CallExpr {
			func := t.find_fn(it.name) or {
				return types.void_ti
			}
			return func.return_ti
		}
		ast.MethodCallExpr {
			ti := t.get_expr_ti(it.expr)
			func := t.find_method(ti.idx, it.name) or {
				return types.void_ti
			}
			return func.return_ti
		}
		ast.Ident {
			if it.kind == .variable {
				info := it.info as ast.IdentVar
				if info.ti.kind != .unresolved {
					return info.ti
				}
				return t.get_expr_ti(info.expr)
			}
			return types.void_ti
		}
		ast.StructInit {
			return it.ti
		}
		ast.StringLiteral {
			return types.string_ti
		}
		ast.IntegerLiteral {
			return types.int_ti
		}
		ast.SelectorExpr {
			ti := t.get_expr_ti(it.expr)
			kind := t.types[ti.idx].kind
			if ti.kind == .placeholder {
				println(' ##### PH $ti.name')
			}
			if !(kind in [.placeholder, .struct_]) {
				return types.void_ti
			}
			struct_ := t.types[ti.idx]
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
			return types.void_ti
		}
		ast.BinaryExpr {
			return t.get_expr_ti(it.left)
		}
		else {
			return types.void_ti
		}
	}
}
