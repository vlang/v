// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module table

pub struct Table {
	// struct_fields map[string][]string
pub mut:
	types     []TypeSymbol
	// type_idxs Hashmap
	type_idxs map[string]int
	// fns Hashmap
	fns       map[string]Fn
	consts    map[string]Var
	imports   []string // List of all imports
	modules   []string // List of all modules registered by the application
}

pub struct Fn {
pub:
	name        string
	args        []Var
	return_type Type
	is_variadic bool
	is_c        bool
}

pub struct Var {
pub:
	name        string
	idx         int
	is_mut      bool
	is_const    bool
	is_global   bool
	scope_level int
mut:
	typ         Type
}

pub fn new_table() &Table {
	mut t := &Table{}
	t.register_builtin_type_symbols()
	return t
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

pub fn (t &Table) register_method(typ &TypeSymbol, new_fn Fn) bool {
	// println('register method `$new_fn.name` type=$typ.name idx=$typ.idx')
	// println('register method `$new_fn.name` type=$typ.name')
	// TODO mut << bug
	mut t1 := typ
	mut methods := typ.methods
	methods << new_fn
	t1.methods = methods
	return true
}

pub fn (t &TypeSymbol) has_method(name string) bool {
	t.find_method(name) or {
		return false
	}
	return true
}

pub fn (t &TypeSymbol) find_method(name string) ?Fn {
	for method in t.methods {
		if method.name == name {
			return method
		}
	}
	return none
}

pub fn (s &TypeSymbol) has_field(name string) bool {
	s.find_field(name) or {
		return false
	}
	return true
}

pub fn (s &TypeSymbol) find_field(name string) ?Field {
	match s.info {
		Struct {
			for field in it.fields {
				if field.name == name {
					return field
				}
			}
		}
		else {}
	}
	return none
}

pub fn (t &Table) struct_has_field(s &TypeSymbol, name string) bool {
	if s.parent_idx != 0 {
		parent := &t.types[s.parent_idx]
		println('struct_has_field($s.name, $name) types.len=$t.types.len s.parent=$parent.name')
	}
	else {
		println('struct_has_field($s.name, $name) types.len=$t.types.len s.parent=none')
	}
	if _ := t.struct_find_field(s, name) {
		return true
	}
	return false
}

pub fn (t &Table) struct_find_field(s &TypeSymbol, name string) ?Field {
	if s.parent_idx != 0 {
		parent := &t.types[s.parent_idx]
		println('struct_find_field($s.name, $name) types.len=$t.types.len s.parent=$parent.name')
	}
	else {
		println('struct_find_field($s.name, $name) types.len=$t.types.len s.parent=none')
	}
	if field := s.find_field(name) {
		return field
	}
	if s.parent_idx != 0 {
		parent := &t.types[s.parent_idx]
		if field := parent.find_field(name) {
			println('got parent $parent.name')
			return field
		}
	}
	return none
}

[inline]
pub fn (t &Table) find_type_idx(name string) int {
	return t.type_idxs[name]
}

[inline]
pub fn (t &Table) find_type(name string) ?TypeSymbol {
	idx := t.type_idxs[name]
	if idx > 0 {
		return t.types[idx]
	}
	return none
}

[inline]
pub fn (t &Table) get_type_symbol(typ Type) &TypeSymbol {
	// println('get_type_symbol $typ')
	idx := type_idx(typ)
	if idx > 0 {
		return &t.types[idx]
	}
	// this should never happen
	panic('get_type_symbol: invalid type (typ=$typ idx=${idx}). This should neer happen')
}

// this will override or register builtin type
// allows prexisitng types added in register_builtins
// to be overriden with their real type info
[inline]
pub fn (t mut Table) register_builtin_type_symbol(typ TypeSymbol) int {
	existing_idx := t.type_idxs[typ.name]
	if existing_idx > 0 {
		if existing_idx >= string_type_idx {
			if existing_idx == string_type_idx {
				existing_type := &t.types[existing_idx]
				t.types[existing_idx] = {
					typ |
					kind:existing_type.kind
				}
			}
			else {
				t.types[existing_idx] = typ
			}
		}
		return existing_idx
	}
	return t.register_type_symbol(typ)
}

[inline]
pub fn (t mut Table) register_type_symbol(typ TypeSymbol) int {
	// println('register_type_symbol( $typ.name )')
	existing_idx := t.type_idxs[typ.name]
	if existing_idx > 0 {
		ex_type := t.types[existing_idx]
		match ex_type.kind {
			.placeholder {
				// override placeholder
				// println('overriding type placeholder `$typ.name`')
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
				// panic('cannot register type `$typ.name`, another type with this name exists')
				return -1
			}
	}
	}
	typ_idx := t.types.len
	t.types << typ
	t.type_idxs[typ.name] = typ_idx
	return typ_idx
}

pub fn (t &Table) known_type(name string) bool {
	_ = t.find_type(name) or {
		return false
	}
	return true
}

[inline]
pub fn (t &Table) array_name(elem_type Type, nr_dims int) string {
	elem_type_sym := t.get_type_symbol(elem_type)
	return 'array_${elem_type_sym.name}' + if type_is_ptr(elem_type) { '_ptr' } else { '' } + if nr_dims > 1 { '_${nr_dims}d' } else { '' }
}

[inline]
pub fn (t &Table) array_fixed_name(elem_type Type, size int, nr_dims int) string {
	elem_type_sym := t.get_type_symbol(elem_type)
	return 'array_fixed_${elem_type_sym.name}_${size}' + if type_is_ptr(elem_type) { '_ptr' } else { '' } + if nr_dims > 1 { '_${nr_dims}d' } else { '' }
}

[inline]
pub fn (t &Table) map_name(key_type Type, value_type Type) string {
	key_type_sym := t.get_type_symbol(key_type)
	value_type_sym := t.get_type_symbol(value_type)
	return 'map_${key_type_sym.name}_${value_type_sym.name}' + if type_is_ptr(value_type) { '_ptr' } else { '' }
}

pub fn (t mut Table) find_or_register_map(key_type, value_type Type) int {
	name := t.map_name(key_type, value_type)
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx
	}
	// register
	map_typ := TypeSymbol{
		parent_idx: map_type_idx
		kind: .map
		name: name
		info: Map{
			key_type: key_type
			value_type: value_type
		}
	}
	return t.register_type_symbol(map_typ)
}

pub fn (t mut Table) find_or_register_array(elem_type Type, nr_dims int) int {
	name := t.array_name(elem_type, nr_dims)
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx
	}
	// register
	array_type := TypeSymbol{
		parent_idx: array_type_idx
		kind: .array
		name: name
		info: Array{
			elem_type: elem_type
			nr_dims: nr_dims
		}
	}
	return t.register_type_symbol(array_type)
}

pub fn (t mut Table) find_or_register_array_fixed(elem_type Type, size int, nr_dims int) int {
	name := t.array_fixed_name(elem_type, size, nr_dims)
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx
	}
	// register
	array_fixed_type := TypeSymbol{
		kind: .array_fixed
		name: name
		info: ArrayFixed{
			elem_type: elem_type
			size: size
			nr_dims: nr_dims
		}
	}
	return t.register_type_symbol(array_fixed_type)
}

pub fn (t mut Table) find_or_register_multi_return(mr_typs []Type) int {
	mut name := 'multi_return'
	for mr_typ in mr_typs {
		mr_type_sym := t.get_type_symbol(mr_typ)
		name += '_$mr_type_sym.name'
	}
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx
	}
	// register
	mr_type := TypeSymbol{
		kind: .multi_return
		name: name
		info: MultiReturn{
			types: mr_typs
		}
	}
	return t.register_type_symbol(mr_type)
}

pub fn (t mut Table) add_placeholder_type(name string) int {
	ph_type := TypeSymbol{
		kind: .placeholder
		name: name
	}
	// println('added placeholder: $name - $ph_type.idx')
	return t.register_type_symbol(ph_type)
}

pub fn (t &Table) check(got, expected Type) bool {
	got_type_sym := t.get_type_symbol(got)
	exp_type_sym := t.get_type_symbol(expected)
	got_idx := type_idx(got)
	exp_idx := type_idx(expected)
	// got_is_ptr := type_is_ptr(got)
	exp_is_ptr := type_is_ptr(expected)
	// println('check: $got_type_sym.name, $exp_type_sym.name')
	if got_type_sym.kind == .none_ {
		// TODO
		return true
	}
	if exp_type_sym.kind == .voidptr {
		return true
	}
	// if got_type_sym.kind == .array_fixed {
	// return true
	// }
	if got_type_sym.kind in [.voidptr, .byteptr, .charptr, .int] && exp_type_sym.kind in [.voidptr, .byteptr, .charptr] {
		return true
	}
	if got_type_sym.is_int() && exp_type_sym.is_int() {
		return true
	}
	// allow pointers to be initialized with 0. TODO: use none instead
	if exp_is_ptr && got_idx == int_type_idx {
		return true
	}
	// allow enum value to be used as int
	if (got_type_sym.is_int() && exp_type_sym.kind == .enum_) || (exp_type_sym.is_int() && got_type_sym.kind == .enum_) {
		return true
	}
	// TODO
	if got_type_sym.is_number() && exp_type_sym.is_number() {
		return true
	}
	// TODO: actually check for & handle pointers with name_expr
	// see hack in checker IndexExpr line #691
	if (got_type_sym.kind == .byte && exp_type_sym.kind == .byteptr) || (exp_type_sym.kind == .byte && got_type_sym.kind == .byteptr) {
		return true
	}
	// TODO
	// if got_type_sym.kind == .array && exp_type_sym.kind == .array {
	// return true
	// }
	if got_type_sym.kind == .array_fixed && exp_type_sym.kind == .byteptr {
		info := got_type_sym.info as ArrayFixed
		if type_idx(info.elem_type) == byte_type_idx {
			return true
		}
	}
	// TODO
	if exp_type_sym.name == 'array' || got_type_sym.name == 'array' {
		return true
	}
	// TODO
	// accept [] when an expected type is an array
	if got_type_sym.kind == .array && got_type_sym.name == 'array_void' && exp_type_sym.kind == .array {
		return true
	}
	// type alias
	if (got_type_sym.kind == .alias && got_type_sym.parent_idx == exp_idx) || (exp_type_sym.kind == .alias && exp_type_sym.parent_idx == got_idx) {
		return true
	}
	// sum type
	if got_type_sym.kind == .sum_type {
		sum_info := got_type_sym.info as SumType
		if expected in sum_info.variants {
			return true
		}
	}
	else if exp_type_sym.kind == .sum_type {
		sum_info := exp_type_sym.info as SumType
		if got in sum_info.variants {
			return true
		}
	}
	if got_idx != exp_idx {
		// && got.typ.name != expected.typ.name*/
		return false
	}
	return true
}
