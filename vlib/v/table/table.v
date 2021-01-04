// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module table

import os
import v.cflag
import v.token
import v.util

pub struct Table {
pub mut:
	types         []TypeSymbol
	type_idxs     map[string]int
	fns           map[string]Fn
	imports       []string // List of all imports
	modules       []string // Topologically sorted list of all modules registered by the application
	cflags        []cflag.CFlag
	redefined_fns []string
	fn_gen_types  map[string][]Type // for generic functions
	cmod_prefix   string // needed for table.type_to_str(Type) while vfmt; contains `os.`
	is_fmt        bool
}

pub struct Fn {
pub:
	params         []Param
	return_type    Type
	is_variadic    bool
	language       Language
	is_generic     bool
	is_pub         bool
	is_deprecated  bool
	is_unsafe      bool
	is_placeholder bool
	mod            string
	ctdefine       string // compile time define. myflag, when [if myflag] tag
	attrs          []Attr
pub mut:
	name           string
	source_fn      voidptr // set in the checker, while processing fn declarations
}

fn (f &Fn) method_equals(o &Fn) bool {
	return f.params[1..].equals(o.params[1..]) && f.return_type == o.return_type && f.is_variadic ==
		o.is_variadic && f.language == o.language && f.is_generic == o.is_generic && f.is_pub == o.is_pub &&
		f.mod == o.mod && f.name == o.name
}

pub struct Param {
pub:
	pos       token.Position
	name      string
	is_mut    bool
	typ       Type
	is_hidden bool // interface first arg
}

fn (p &Param) equals(o &Param) bool {
	return p.name == o.name && p.is_mut == o.is_mut && p.typ == o.typ && p.is_hidden == o.is_hidden
}

fn (p []Param) equals(o []Param) bool {
	if p.len != o.len {
		return false
	}
	for i in 0 .. p.len {
		if !p[i].equals(o[i]) {
			return false
		}
	}
	return true
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
	t.is_fmt = true
	return t
}

// used to compare fn's & for naming anon fn's
pub fn (t &Table) fn_type_signature(f &Fn) string {
	mut sig := ''
	for i, arg in f.params {
		// TODO: for now ignore mut/pts in sig for now
		typ := arg.typ.set_nr_muls(0)
		// if arg.is_mut {
		// sig += 'mut_'
		// }
		// sig += '$arg.typ'
		sig += '$typ'
		if i < f.params.len - 1 {
			sig += '_'
		}
	}
	sig += '_$f.return_type'
	return sig
}

// source_signature generates the signature of a function which looks like in the V source
pub fn (t &Table) fn_type_source_signature(f &Fn) string {
	mut sig := '('
	for i, arg in f.params {
		if arg.is_mut {
			sig += 'mut '
		}
		arg_type_sym := t.get_type_symbol(arg.typ)
		sig += '$arg_type_sym.name'
		if i < f.params.len - 1 {
			sig += ', '
		}
	}
	sig += ')'
	if f.return_type == ovoid_type {
		sig += ' ?'
	} else if f.return_type != void_type {
		return_type_sym := t.get_type_symbol(f.return_type)
		sig += ' $return_type_sym.name'
	}
	return sig
}

pub fn (f &Fn) is_same_method_as(func &Fn) bool {
	if f.return_type != func.return_type {
		return false
	}
	if f.params.len != func.params.len {
		return false
	}
	for i in 1 .. f.params.len {
		if f.params[i].typ != func.params[i].typ {
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
	t.find_fn(name) or { return false }
	return true
}

pub fn (mut t Table) register_fn(new_fn Fn) {
	// println('reg fn $new_fn.name nr_args=$new_fn.args.len')
	t.fns[new_fn.name] = new_fn
}

pub fn (mut t TypeSymbol) register_method(new_fn Fn) int {
	// returns a method index, stored in the ast.FnDecl
	// for faster lookup in the checker's fn_decl method
	// println('reg me $new_fn.name nr_args=$new_fn.args.len')
	t.methods << new_fn
	return t.methods.len - 1
}

pub fn (t &Table) register_aggregate_method(mut sym TypeSymbol, name string) ?Fn {
	if sym.kind != .aggregate {
		panic('Unexpected type symbol: $sym.kind')
	}
	agg_info := sym.info as Aggregate
	// an aggregate always has at least 2 types
	mut found_once := false
	mut new_fn := Fn{}
	for typ in agg_info.types {
		ts := t.get_type_symbol(typ)
		if type_method := ts.find_method(name) {
			if !found_once {
				found_once = true
				new_fn = type_method
			} else if !new_fn.method_equals(type_method) {
				return error('method `${t.type_to_str(typ)}.$name` signature is different')
			}
		} else {
			return error('unknown method: `${t.type_to_str(typ)}.$name`')
		}
	}
	// register the method in the aggregate, so lookup is faster next time
	sym.register_method(new_fn)
	return new_fn
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
		if ts.kind == .aggregate {
			method := t.register_aggregate_method(mut ts, name) ?
			return method
		}
		if ts.parent_idx == 0 {
			break
		}
		ts = unsafe { &t.types[ts.parent_idx] }
	}
	return none
}

fn (t &Table) register_aggregate_field(mut sym TypeSymbol, name string) ?Field {
	if sym.kind != .aggregate {
		panic('Unexpected type symbol: $sym.kind')
	}
	mut agg_info := sym.info as Aggregate
	// an aggregate always has at least 2 types
	mut found_once := false
	mut new_field := Field{}
	for typ in agg_info.types {
		ts := t.get_type_symbol(typ)
		if type_field := t.struct_find_field(ts, name) {
			if !found_once {
				found_once = true
				new_field = type_field
			} else if !new_field.equals(type_field) {
				return error('field `${t.type_to_str(typ)}.$name` type is different')
			}
		} else {
			return error('type `${t.type_to_str(typ)}` has no field or method `$name`')
		}
	}
	agg_info.fields << new_field
	return new_field
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
		if mut ts.info is Struct {
			if field := ts.info.find_field(name) {
				return field
			}
		} else if mut ts.info is Aggregate {
			if field := ts.info.find_field(name) {
				return field
			}
			field := t.register_aggregate_field(mut ts, name) or { return error(err) }
			return field
		}
		if ts.parent_idx == 0 {
			break
		}
		ts = unsafe { &t.types[ts.parent_idx] }
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
		return unsafe { &t.types[idx] }
	}
	// this should never happen
	panic('get_type_symbol: invalid type (typ=$typ idx=$idx). Compiler bug. This should never happen. Please create a GitHub issue.
')
}

// get_final_type_symbol follows aliases until it gets to a "real" Type
[inline]
pub fn (t &Table) get_final_type_symbol(typ Type) &TypeSymbol {
	idx := typ.idx()
	if idx > 0 {
		current_type := t.types[idx]
		if current_type.kind == .alias {
			alias_info := current_type.info as Alias
			return t.get_final_type_symbol(alias_info.parent_type)
		}
		return unsafe { &t.types[idx] }
	}
	// this should never happen
	panic('get_final_type_symbol: invalid type (typ=$typ idx=$idx). Compiler bug. This should never happen. Please create a GitHub issue.')
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
		pt := (sym.info as Alias).parent_type
		if pt <= f64_type && pt >= void_type {
			return pt
		}
	}
	return typ
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
				// builtin
				// this will override the already registered builtin types
				// with the actual v struct declaration in the source
				if existing_idx >= string_type_idx && existing_idx <= map_type_idx {
					if existing_idx == string_type_idx {
						// existing_type := t.types[existing_idx]
						t.types[existing_idx] = {
							typ |
							kind: ex_type.kind
						}
					} else {
						t.types[existing_idx] = typ
					}
					return existing_idx
				}
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
	t.find_type(name) or { return false }
	return true
}

// array_source_name generates the original name for the v source.
// e. g. []int
[inline]
pub fn (t &Table) array_name(elem_type Type, nr_dims int) string {
	elem_type_sym := t.get_type_symbol(elem_type)
	ptr := if elem_type.is_ptr() { '&'.repeat(elem_type.nr_muls()) } else { '' }
	dims := '[]'.repeat(nr_dims)
	return '$dims$ptr$elem_type_sym.name'
}

[inline]
pub fn (t &Table) array_cname(elem_type Type, nr_dims int) string {
	elem_type_sym := t.get_type_symbol(elem_type)
	mut res := ''
	if elem_type.is_ptr() {
		res = '_ptr'.repeat(elem_type.nr_muls())
	}
	if nr_dims > 1 {
		res += '_${nr_dims}d'
	}
	return 'array_$elem_type_sym.cname' + res
}

// array_fixed_source_name generates the original name for the v source.
// e. g. [16][8]int
[inline]
pub fn (t &Table) array_fixed_name(elem_type Type, size int) string {
	elem_type_sym := t.get_type_symbol(elem_type)
	ptr := if elem_type.is_ptr() { '&'.repeat(elem_type.nr_muls()) } else { '' }
	return '[$size]$ptr$elem_type_sym.name'
}

[inline]
pub fn (t &Table) array_fixed_cname(elem_type Type, size int, nr_dims int) string {
	elem_type_sym := t.get_type_symbol(elem_type)
	mut res := ''
	if elem_type.is_ptr() {
		res = '_ptr'
	}
	if nr_dims > 1 {
		res += '_${nr_dims}d'
	}
	return 'array_fixed_${elem_type_sym.cname}_$size' + res
}

[inline]
pub fn (t &Table) chan_name(elem_type Type, is_mut bool) string {
	elem_type_sym := t.get_type_symbol(elem_type)
	mut ptr := ''
	if is_mut {
		ptr = 'mut '
	} else if elem_type.is_ptr() {
		ptr = '&'
	}
	return 'chan $ptr$elem_type_sym.name'
}

[inline]
pub fn (t &Table) chan_cname(elem_type Type, is_mut bool) string {
	elem_type_sym := t.get_type_symbol(elem_type)
	mut suffix := ''
	if is_mut {
		suffix = '_mut'
	} else if elem_type.is_ptr() {
		suffix = '_ptr'
	}
	return 'chan_$elem_type_sym.cname' + suffix
}

// map_source_name generates the original name for the v source.
// e. g. map[string]int
[inline]
pub fn (t &Table) map_name(key_type Type, value_type Type) string {
	key_type_sym := t.get_type_symbol(key_type)
	value_type_sym := t.get_type_symbol(value_type)
	ptr := if value_type.is_ptr() { '&' } else { '' }
	return 'map[$key_type_sym.name]$ptr$value_type_sym.name'
}

[inline]
pub fn (t &Table) map_cname(key_type Type, value_type Type) string {
	key_type_sym := t.get_type_symbol(key_type)
	value_type_sym := t.get_type_symbol(value_type)
	suffix := if value_type.is_ptr() { '_ptr' } else { '' }
	return 'map_${key_type_sym.cname}_$value_type_sym.cname' + suffix
	// return 'map_${value_type_sym.name}' + suffix
}

pub fn (mut t Table) find_or_register_chan(elem_type Type, is_mut bool) int {
	name := t.chan_name(elem_type, is_mut)
	cname := t.chan_cname(elem_type, is_mut)
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx
	}
	// register
	chan_typ := TypeSymbol{
		parent_idx: chan_type_idx
		kind: .chan
		name: name
		cname: cname
		info: Chan{
			elem_type: elem_type
			is_mut: is_mut
		}
	}
	return t.register_type_symbol(chan_typ)
}

pub fn (mut t Table) find_or_register_map(key_type Type, value_type Type) int {
	name := t.map_name(key_type, value_type)
	cname := t.map_cname(key_type, value_type)
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
		cname: cname
		info: Map{
			key_type: key_type
			value_type: value_type
		}
	}
	return t.register_type_symbol(map_typ)
}

pub fn (mut t Table) find_or_register_array(elem_type Type, nr_dims int) int {
	name := t.array_name(elem_type, nr_dims)
	cname := t.array_cname(elem_type, nr_dims)
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
		cname: cname
		info: Array{
			elem_type: elem_type
			nr_dims: nr_dims
		}
	}
	return t.register_type_symbol(array_type)
}

pub fn (mut t Table) find_or_register_array_fixed(elem_type Type, size int, nr_dims int) int {
	name := t.array_fixed_name(elem_type, size)
	cname := t.array_fixed_cname(elem_type, size, nr_dims)
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx
	}
	// register
	array_fixed_type := TypeSymbol{
		kind: .array_fixed
		name: name
		cname: cname
		info: ArrayFixed{
			elem_type: elem_type
			size: size
			nr_dims: nr_dims
		}
	}
	return t.register_type_symbol(array_fixed_type)
}

pub fn (mut t Table) find_or_register_multi_return(mr_typs []Type) int {
	mut name := '('
	mut cname := 'multi_return'
	for i, mr_typ in mr_typs {
		mr_type_sym := t.get_type_symbol(mr_typ)
		name += mr_type_sym.name
		cname += '_$mr_type_sym.cname'
		if i < mr_typs.len - 1 {
			name += ', '
		}
	}
	name += ')'
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx
	}
	// register
	mr_type := TypeSymbol{
		kind: .multi_return
		name: name
		cname: cname
		info: MultiReturn{
			types: mr_typs
		}
	}
	return t.register_type_symbol(mr_type)
}

pub fn (mut t Table) find_or_register_fn_type(mod string, f Fn, is_anon bool, has_decl bool) int {
	name := if f.name.len == 0 { 'fn ${t.fn_type_source_signature(f)}' } else { f.name.clone() }
	cname := if f.name.len == 0 {
		'anon_fn_${t.fn_type_signature(f)}'
	} else {
		util.no_dots(f.name.clone())
	}
	anon := f.name.len == 0 || is_anon
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 && t.types[existing_idx].kind != .placeholder {
		return existing_idx
	}
	return t.register_type_symbol(
		kind: .function
		name: name
		cname: cname
		mod: mod
		info: FnType{
			is_anon: anon
			has_decl: has_decl
			func: f
		}
	)
}

pub fn (mut t Table) add_placeholder_type(name string, language Language) int {
	mut modname := ''
	if name.contains('.') {
		modname = name.all_before_last('.')
	}
	ph_type := TypeSymbol{
		kind: .placeholder
		name: name
		cname: util.no_dots(name)
		language: language
		mod: modname
	}
	// println('added placeholder: $name - $ph_type.idx')
	return t.register_type_symbol(ph_type)
}

[inline]
pub fn (t &Table) value_type(typ Type) Type {
	typ_sym := t.get_type_symbol(typ)
	if typ.has_flag(.variadic) {
		// ...string => string
		// return typ.clear_flag(.variadic)
		array_info := typ_sym.info as Array
		return array_info.elem_type
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
		any_flt_type { return f64_type }
		any_int_type { return int_type }
		else { return typ }
	}
}

// TODO: Once we have a module format we can read from module file instead
// this is not optimal. it depends on the full import being in table.imports
// already, we can instead lookup the module path and then work it out
pub fn (table &Table) qualify_module(mod string, file_path string) string {
	for m in table.imports {
		// if m.contains('gen') { println('qm=$m') }
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

pub fn (mut table Table) register_fn_gen_type(fn_name string, typ Type) {
	mut a := table.fn_gen_types[fn_name]
	if typ in a {
		return
	}
	a << typ
	// sym := table.get_type_symbol(typ)
	// println('registering fn ($fn_name) gen type $sym.name')
	table.fn_gen_types[fn_name] = a
}

// TODO: there is a bug when casting sumtype the other way if its pointer
// so until fixed at least show v (not C) error `x(variant) =  y(SumType*)`
pub fn (table &Table) sumtype_has_variant(parent Type, variant Type) bool {
	parent_sym := table.get_type_symbol(parent)
	if parent_sym.kind == .sum_type {
		parent_info := parent_sym.info as SumType
		for v in parent_info.variants {
			if v.idx() == variant.idx() {
				return true
			}
		}
	}
	return false
}

pub fn (table &Table) known_type_names() []string {
	mut res := []string{}
	for _, idx in table.type_idxs {
		// Skip `any_int_type_idx` and `any_flt_type_idx` because they shouldn't be visible to the User.
		if idx in [0, any_int_type_idx, any_flt_type_idx] {
			continue
		}
		res << table.type_to_str(idx)
	}
	return res
}
