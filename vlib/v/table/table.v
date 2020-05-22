// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module table

import os
import v.cflag

pub struct Table {
pub mut:
	types         []TypeSymbol
	type_idxs     map[string]int
	fns           map[string]Fn
	imports       []string // List of all imports
	modules       []string // List of all modules registered by the application
	cflags        []cflag.CFlag
	redefined_fns []string
	fn_gen_types  map[string][]Type // for generic functions
}

pub struct Fn {
pub:
	args        []Arg
	return_type Type
	is_variadic bool
	language    Language
	is_generic  bool
	is_pub      bool
	mod         string
	ctdefine    string // compile time define. myflag, when [if myflag] tag
pub mut:
	name        string
}

pub struct Arg {
pub:
	name      string
	is_mut    bool
	typ       Type
	is_hidden bool // interface first arg
}

pub struct Var {
pub:
	name   string
	is_mut bool
mut:
	typ    Type
}

pub fn new_table() &Table {
	mut t := &Table{}
	t.register_builtin_type_symbols()
	return t
}

// used to compare fn's & for naming anon fn's
pub fn (f &Fn) signature() string {
	mut sig := ''
	for i, arg in f.args {
		// TODO: for now ignore mut/pts in sig for now
		typ := arg.typ.set_nr_muls(0)
		// if arg.is_mut {
		// sig += 'mut_'
		// }
		// sig += '$arg.typ'
		sig += '$typ'
		if i < f.args.len - 1 {
			sig += '_'
		}
	}
	sig += '_$f.return_type'
	return sig
}

pub fn (f &Fn) is_same_method_as(func &Fn) bool {
	if f.return_type != func.return_type {
		return false
	}
	if f.args.len != func.args.len {
		return false
	}
	for i in 1 .. f.args.len {
		if f.args[i].typ != func.args[i].typ {
			return false
		}
	}
	return true
}

pub fn (t &Table) find_fn(name string) ?Fn {
	f := t.fns[name]
	if f.name.str != 0 {
		// TODO
		return f
	}
	return none
}

pub fn (t &Table) known_fn(name string) bool {
	t.find_fn(name) or {
		return false
	}
	return true
}

pub fn (mut t Table) register_fn(new_fn Fn) {
	// println('reg fn $new_fn.name nr_args=$new_fn.args.len')
	t.fns[new_fn.name] = new_fn
}

pub fn (mut t TypeSymbol) register_method(new_fn Fn) {
	t.methods << new_fn
}

pub fn (t &Table) type_has_method(s &TypeSymbol, name string) bool {
	// println('type_has_method($s.name, $name) types.len=$t.types.len s.parent_idx=$s.parent_idx')
	if _ := t.type_find_method(s, name) {
		return true
	}
	return false
}

// search from current type up through each parent looking for method
pub fn (t &Table) type_find_method(s &TypeSymbol, name string) ?Fn {
	// println('type_find_method($s.name, $name) types.len=$t.types.len s.parent_idx=$s.parent_idx')
	mut ts := s
	for {
		if method := ts.find_method(name) {
			return method
		}
		if ts.parent_idx == 0 {
			break
		}
		ts = &t.types[ts.parent_idx]
	}
	return none
}

pub fn (t &Table) struct_has_field(s &TypeSymbol, name string) bool {
	// println('struct_has_field($s.name, $name) types.len=$t.types.len s.parent_idx=$s.parent_idx')
	if _ := t.struct_find_field(s, name) {
		return true
	}
	return false
}

// search from current type up through each parent looking for field
pub fn (t &Table) struct_find_field(s &TypeSymbol, name string) ?Field {
	// println('struct_find_field($s.name, $name) types.len=$t.types.len s.parent_idx=$s.parent_idx')
	mut ts := s
	for {
		if ts.info is Struct {
			struct_info := ts.info as Struct
			if field := struct_info.find_field(name) {
				return field
			}
		}
		if ts.parent_idx == 0 {
			break
		}
		ts = &t.types[ts.parent_idx]
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
	idx := typ.idx()
	if idx > 0 {
		return &t.types[idx]
	}
	// this should never happen
	panic('get_type_symbol: invalid type (typ=$typ idx=${idx}). Compiler bug. This should never happen')
}

[inline]
pub fn (t &Table) get_type_name(typ Type) string {
	typ_sym := t.get_type_symbol(typ)
	return typ_sym.name
}

[inline]
pub fn (t &Table) unalias_num_type(typ Type) Type {
	sym := t.get_type_symbol(typ)
	if sym.kind == .alias {
		pt := (sym.info as Alias).parent_typ
		if pt <= f64_type && pt >= void_type {
			return pt
		}
	}
	return typ
}

// this will override or register builtin type
// allows prexisitng types added in register_builtins
// to be overriden with their real type info
[inline]
pub fn (mut t Table) register_builtin_type_symbol(typ TypeSymbol) int {
	existing_idx := t.type_idxs[typ.name]
	if existing_idx > 0 {
		if existing_idx >= string_type_idx {
			if existing_idx == string_type_idx {
				existing_type := t.types[existing_idx]
				t.types[existing_idx] = {
					typ |
					kind: existing_type.kind
				}
			} else {
				t.types[existing_idx] = typ
			}
		}
		return existing_idx
	}
	return t.register_type_symbol(typ)
}

[inline]
pub fn (mut t Table) register_type_symbol(typ TypeSymbol) int {
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
					methods: ex_type.methods
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
	return 'array_${elem_type_sym.name}' + if elem_type.is_ptr() {
		'_ptr'
	} else {
		''
	} + if nr_dims > 1 {
		'_${nr_dims}d'
	} else {
		''
	}
}

[inline]
pub fn (t &Table) array_fixed_name(elem_type Type, size, nr_dims int) string {
	elem_type_sym := t.get_type_symbol(elem_type)
	return 'array_fixed_${elem_type_sym.name}_${size}' + if elem_type.is_ptr() {
		'_ptr'
	} else {
		''
	} + if nr_dims > 1 {
		'_${nr_dims}d'
	} else {
		''
	}
}

[inline]
pub fn (t &Table) map_name(key_type, value_type Type) string {
	key_type_sym := t.get_type_symbol(key_type)
	value_type_sym := t.get_type_symbol(value_type)
	suffix := if value_type.is_ptr() { '_ptr' } else { '' }
	return 'map_${key_type_sym.name}_${value_type_sym.name}' + suffix
	// return 'map_${value_type_sym.name}' + suffix
}

pub fn (mut t Table) find_or_register_map(key_type, value_type Type) int {
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

pub fn (mut t Table) find_or_register_array(elem_type Type, nr_dims int) int {
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

pub fn (mut t Table) find_or_register_array_fixed(elem_type Type, size, nr_dims int) int {
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

pub fn (mut t Table) find_or_register_multi_return(mr_typs []Type) int {
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

pub fn (mut t Table) find_or_register_fn_type(mod string, f Fn, is_anon, has_decl bool) int {
	name := if f.name.len == 0 { 'anon_fn_$f.signature()' } else { f.name }
	anon := f.name.len == 0 || is_anon
	return t.register_type_symbol(TypeSymbol{
		kind: .function
		name: name
		mod: mod
		info: FnType{
			is_anon: anon
			has_decl: has_decl
			func: f
		}
	})
}

pub fn (mut t Table) add_placeholder_type(name string) int {
	ph_type := TypeSymbol{
		kind: .placeholder
		name: name
	}
	// println('added placeholder: $name - $ph_type.idx')
	return t.register_type_symbol(ph_type)
}

[inline]
pub fn (t &Table) value_type(typ Type) Type {
	typ_sym := t.get_type_symbol(typ)
	if typ.flag_is(.variadic) {
		// ...string => string
		return typ.set_flag(.unset)
	}
	if typ_sym.kind == .array {
		// Check index type
		info := typ_sym.info as Array
		return info.elem_type
	}
	if typ_sym.kind == .array_fixed {
		info := typ_sym.info as ArrayFixed
		return info.elem_type
	}
	if typ_sym.kind == .map {
		info := typ_sym.info as Map
		return info.value_type
	}
	if typ_sym.kind == .string && typ.is_ptr() {
		// (&string)[i] => string
		return string_type
	}
	if typ_sym.kind in [.byteptr, .string] {
		return byte_type
	}
	if typ.is_ptr() {
		// byte* => byte
		// bytes[0] is a byte, not byte*
		return typ.deref()
	}
	// TODO: remove when map_string is removed
	if typ_sym.name == 'map_string' {
		return string_type
	}
	return void_type
}

[inline]
pub fn (t &Table) mktyp(typ Type) Type {
	match typ {
		any_flt_type {
			return f64_type
		}
		any_int_type {
			return int_type
		}
		else {
			return typ
		}
	}
}

pub fn (t &Table) promote(left_type, right_type Type) Type {
	if left_type.is_ptr() || left_type.is_pointer() {
		if right_type.is_int() {
			return left_type
		} else {
			return void_type
		}
	} else if right_type.is_ptr() || right_type.is_pointer() {
		if left_type.is_int() {
			return right_type
		} else {
			return void_type
		}
	}
	if left_type == right_type {
		return left_type // strings, self defined operators
	}
	if right_type.is_number() && left_type.is_number() {
		// sort the operands to save time
		mut type_hi := left_type
		mut type_lo := right_type
		if type_hi.idx() < type_lo.idx() {
			tmp := type_hi
			type_hi = type_lo
			type_lo = tmp
		}
		idx_hi := type_hi.idx()
		idx_lo := type_lo.idx()
		// the following comparisons rely on the order of the indices in atypes.v
		if idx_hi == any_int_type_idx {
			return type_lo
		} else if idx_hi == any_flt_type_idx {
			if idx_lo in float_type_idxs {
				return type_lo
			} else {
				return void_type
			}
		} else if type_hi.is_float() {
			return type_hi
		} else if idx_lo >= byte_type_idx { // both operands are unsigned
			return type_hi
		} else if idx_hi - idx_lo < (byte_type_idx - i8_type_idx) {
			return type_lo // conversion unsigned -> signed if signed type is larger
		} else {
			return void_type // conversion signed -> unsigned not allowed
		}
	} else {
		return left_type // default to left if not automatic promotion possible
	}
}

// TODO: promote(), assign_check() and check() overlap - should be rearanged
pub fn (t &Table) assign_check(got, expected Type) bool {
	exp_idx := expected.idx()
	got_idx := got.idx()
	if exp_idx == got_idx {
		return true
	}
	if exp_idx == voidptr_type_idx || exp_idx == byteptr_type_idx {
		if got.is_ptr() || got.is_pointer() {
			return true
		}
	}
	// allow direct int-literal assignment for pointers for now
	if expected.is_ptr() || expected.is_pointer() {
		if got == any_int_type {
			return true
		}
	}
	if got_idx == voidptr_type_idx || got_idx == byteptr_type_idx {
		if expected.is_ptr() || expected.is_pointer() {
			return true
		}
	}
	if !t.check(got, expected) { // TODO: this should go away...
		return false
	}
	if t.promote(expected, got) != expected {
		return false
	}
	return true
}

pub fn (t &Table) check(got, expected Type) bool {
	got_idx := t.unalias_num_type(got).idx()
	exp_idx := t.unalias_num_type(expected).idx()
	// got_is_ptr := got.is_ptr()
	exp_is_ptr := expected.is_ptr()
	// println('check: $got_type_sym.name, $exp_type_sym.name')
	// # NOTE: use idxs here, and symbols below for perf
	if got_idx == exp_idx {
		// this is returning true even if one type is a ptr
		// and the other is not, is this correct behaviour?
		return true
	}
	if got_idx == none_type_idx && expected.flag_is(.optional) {
		return true
	}
	// allow pointers to be initialized with 0. TODO: use none instead
	if exp_is_ptr && got_idx == int_type_idx {
		return true
	}
	if exp_idx == voidptr_type_idx || got_idx == voidptr_type_idx {
		return true
	}
	if exp_idx == any_type_idx || got_idx == any_type_idx {
		return true
	}
	// TODO i64 as int etc
	if (exp_idx in pointer_type_idxs || exp_idx in number_type_idxs) && (got_idx in pointer_type_idxs ||
		got_idx in number_type_idxs) {
		return true
	}
	// if exp_idx in pointer_type_idxs && got_idx in pointer_type_idxs {
	// return true
	// }
	// see hack in checker IndexExpr line #691
	if (got_idx == byte_type_idx && exp_idx == byteptr_type_idx) || (exp_idx == byte_type_idx &&
		got_idx == byteptr_type_idx) {
		return true
	}
	if (got_idx == char_type_idx && exp_idx == charptr_type_idx) || (exp_idx == char_type_idx &&
		got_idx == charptr_type_idx) {
		return true
	}
	// # NOTE: use symbols from this point on for perf
	got_type_sym := t.get_type_symbol(got)
	exp_type_sym := t.get_type_symbol(expected)
	//
	if exp_type_sym.kind == .function && got_type_sym.kind == .int {
		// TODO temporary
		// fn == 0
		return true
	}
	// allow enum value to be used as int
	if (got_type_sym.is_int() && exp_type_sym.kind == .enum_) || (exp_type_sym.is_int() &&
		got_type_sym.kind == .enum_) {
		return true
	}
	// TODO
	// if got_type_sym.kind == .array && exp_type_sym.kind == .array {
	// return true
	// }
	if got_type_sym.kind == .array_fixed && exp_type_sym.kind == .byteptr {
		info := got_type_sym.info as ArrayFixed
		if info.elem_type.idx() == byte_type_idx {
			return true
		}
	}
	// TODO
	if exp_type_sym.name == 'array' || got_type_sym.name == 'array' {
		return true
	}
	// TODO
	// accept [] when an expected type is an array
	if got_type_sym.kind == .array && got_type_sym.name == 'array_void' && exp_type_sym.kind ==
		.array {
		return true
	}
	// type alias
	if (got_type_sym.kind == .alias && got_type_sym.parent_idx == exp_idx) || (exp_type_sym.kind ==
		.alias && exp_type_sym.parent_idx == got_idx) {
		return true
	}
	// sum type
	if got_type_sym.kind == .sum_type {
		sum_info := got_type_sym.info as SumType
		// TODO: handle `match SumType { &PtrVariant {} }` currently just checking base
		if expected.set_nr_muls(0) in sum_info.variants {
			return true
		}
	}
	if exp_type_sym.kind == .sum_type {
		sum_info := exp_type_sym.info as SumType
		// TODO: handle `match SumType { &PtrVariant {} }` currently just checking base
		if got.set_nr_muls(0) in sum_info.variants {
			return true
		}
	}
	// fn type
	if got_type_sym.kind == .function && exp_type_sym.kind == .function {
		got_info := got_type_sym.info as FnType
		exp_info := exp_type_sym.info as FnType
		got_fn := got_info.func
		exp_fn := exp_info.func
		// we are using check() to compare return type & args as they might include
		// functions themselves. TODO: optimize, only use check() when needed
		if got_fn.args.len == exp_fn.args.len && t.check(got_fn.return_type, exp_fn.return_type) {
			for i, got_arg in got_fn.args {
				exp_arg := exp_fn.args[i]
				if !t.check(got_arg.typ, exp_arg.typ) {
					return false
				}
			}
			return true
		}
	}
	return false
}

// Once we have a module format we can read from module file instead
// this is not optimal
pub fn (table &Table) qualify_module(mod, file_path string) string {
	for m in table.imports {
		if m.contains('.') && m.contains(mod) {
			m_parts := m.split('.')
			m_path := m_parts.join(os.path_separator)
			if mod == m_parts[m_parts.len - 1] && file_path.contains(m_path) {
				return m
			}
		}
	}
	return mod
}

pub fn (table &Table) register_fn_gen_type(fn_name string, typ Type) {
	mut a := table.fn_gen_types[fn_name]
	if typ in a {
		return
	}
	a << typ
	table.fn_gen_types[fn_name] = a
}
