// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
[has_globals]
module ast

import v.cflag
import v.token
import v.util

[heap; minify]
pub struct Table {
mut:
	parsing_type string // name of the type to enable recursive type parsing
pub mut:
	type_symbols       []&TypeSymbol
	type_idxs          map[string]int
	fns                map[string]Fn
	iface_types        map[string][]Type
	dumps              map[int]string // needed for efficiently generating all _v_dump_expr_TNAME() functions
	imports            []string       // List of all imports
	modules            []string       // Topologically sorted list of all modules registered by the application
	global_scope       &Scope = unsafe { nil }
	cflags             []cflag.CFlag
	redefined_fns      []string
	fn_generic_types   map[string][][]Type // for generic functions
	interfaces         map[int]InterfaceDecl
	cmod_prefix        string // needed for ast.type_to_str(Type) while vfmt; contains `os.`
	is_fmt             bool
	used_fns           map[string]bool // filled in by the checker, when pref.skip_unused = true;
	used_consts        map[string]bool // filled in by the checker, when pref.skip_unused = true;
	used_globals       map[string]bool // filled in by the checker, when pref.skip_unused = true;
	used_vweb_types    []Type // vweb context types, filled in by checker, when pref.skip_unused = true;
	used_maps          int    // how many times maps were used, filled in by checker, when pref.skip_unused = true;
	panic_handler      FnPanicHandler = default_table_panic_handler
	panic_userdata     voidptr        = unsafe { nil } // can be used to pass arbitrary data to panic_handler;
	panic_npanics      int
	cur_fn             &FnDecl = unsafe { nil } // previously stored in Checker.cur_fn and Gen.cur_fn
	cur_concrete_types []Type  // current concrete types, e.g. <int, string>
	gostmts            int     // how many `go` statements there were in the parsed files.
	// When table.gostmts > 0, __VTHREADS__ is defined, which can be checked with `$if threads {`
	enum_decls        map[string]EnumDecl
	module_deprecated map[string]bool
	module_attrs      map[string][]Attr // module attributes
	builtin_pub_fns   map[string]bool
	pointer_size      int
	// cache for type_to_str_using_aliases
	cached_type_to_str map[u64]string
	anon_struct_names  map[string]int // anon struct name -> struct sym idx
	// counter for anon struct, avoid name conflicts.
	anon_struct_counter int
}

// used by vls to avoid leaks
// TODO remove manual memory management
[unsafe]
pub fn (mut t Table) free() {
	unsafe {
		for s in t.type_symbols {
			s.free()
		}
		t.type_symbols.free()
		t.type_idxs.free()
		t.fns.free()
		t.dumps.free()
		t.imports.free()
		t.modules.free()
		t.cflags.free()
		t.redefined_fns.free()
		t.fn_generic_types.free()
		t.cmod_prefix.free()
		t.used_fns.free()
		t.used_consts.free()
		t.used_globals.free()
		t.used_vweb_types.free()
	}
}

pub const invalid_type_idx = -1

pub type FnPanicHandler = fn (&Table, string)

fn default_table_panic_handler(t &Table, message string) {
	panic(message)
}

pub fn (t &Table) panic(message string) {
	mut mt := unsafe { &Table(t) }
	mt.panic_npanics++
	t.panic_handler(t, message)
}

[minify]
pub struct Fn {
pub:
	is_variadic     bool
	language        Language
	is_pub          bool
	is_ctor_new     bool // `[use_new] fn JS.Array.prototype.constructor()`
	is_deprecated   bool // `[deprecated] fn abc(){}`
	is_noreturn     bool // `[noreturn] fn abc(){}`
	is_unsafe       bool // `[unsafe] fn abc(){}`
	is_placeholder  bool
	is_main         bool // `fn main(){}`
	is_test         bool // `fn test_abc(){}`
	is_keep_alive   bool // passed memory must not be freed (by GC) before function returns
	is_method       bool // true for `fn (x T) name()`, and for interface declarations (which are also for methods)
	no_body         bool // a pure declaration like `fn abc(x int)`; used in .vh files, C./JS. fns.
	mod             string
	file            string
	file_mode       Language
	pos             token.Pos
	return_type_pos token.Pos
pub mut:
	return_type    Type
	receiver_type  Type // != 0, when .is_method == true
	name           string
	params         []Param
	source_fn      voidptr // set in the checker, while processing fn declarations
	usages         int
	generic_names  []string
	dep_names      []string // globals or consts dependent names
	attrs          []Attr   // all fn attributes
	is_conditional bool     // true for `[if abc]fn(){}`
	ctdefine_idx   int      // the index of the attribute, containing the compile time define [if mytag]
}

fn (f &Fn) method_equals(o &Fn) bool {
	return f.params[1..].equals(o.params[1..]) && f.return_type == o.return_type
		&& f.is_variadic == o.is_variadic && f.language == o.language
		&& f.generic_names == o.generic_names && f.is_pub == o.is_pub && f.mod == o.mod
		&& f.name == o.name
}

[minify]
pub struct Param {
pub:
	pos         token.Pos
	name        string
	is_mut      bool
	is_auto_rec bool
	type_pos    token.Pos
	is_hidden   bool // interface first arg
pub mut:
	typ Type
}

pub fn (f &Fn) new_method_with_receiver_type(new_type Type) Fn {
	unsafe {
		mut new_method := f
		new_method.params = f.params.clone()
		for i in 1 .. new_method.params.len {
			if new_method.params[i].typ == new_method.params[0].typ {
				new_method.params[i].typ = new_type
			}
		}
		new_method.params[0].typ = new_type

		return *new_method
	}
}

pub fn (f &FnDecl) new_method_with_receiver_type(new_type Type) FnDecl {
	unsafe {
		mut new_method := f
		new_method.params = f.params.clone()
		for i in 1 .. new_method.params.len {
			if new_method.params[i].typ == new_method.params[0].typ {
				new_method.params[i].typ = new_type
			}
		}
		new_method.params[0].typ = new_type
		return *new_method
	}
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

pub fn new_table() &Table {
	mut t := &Table{
		global_scope: &Scope{
			parent: 0
		}
		cur_fn: 0
	}
	t.register_builtin_type_symbols()
	t.is_fmt = true
	set_global_table(t)
	return t
}

__global global_table = &Table(0)

pub fn set_global_table(t &Table) {
	global_table = t
}

// used to compare fn's & for naming anon fn's
pub fn (t &Table) fn_type_signature(f &Fn) string {
	mut sig := ''
	for i, arg in f.params {
		// TODO: for now ignore mut/pts in sig for now
		typ := arg.typ.set_nr_muls(0)
		arg_type_sym := t.sym(typ)
		if arg_type_sym.kind == .alias {
			sig += arg_type_sym.cname
		} else {
			sig += arg_type_sym.str().to_lower().replace_each(['.', '__', '&', '', '[', 'arr_',
				'chan ', 'chan_', 'map[', 'map_of_', ']', '_to_', '<', '_T_', ',', '_', ' ', '',
				'>', '', '(', '_', ')', '_'])
		}
		if i < f.params.len - 1 {
			sig += '_'
		}
	}
	if f.return_type != 0 && f.return_type != void_type {
		sym := t.sym(f.return_type)
		opt := if f.return_type.has_flag(.optional) { 'option_' } else { '' }
		res := if f.return_type.has_flag(.result) { 'result_' } else { '' }

		sig += '__$opt$res$sym.cname'
	}
	return sig
}

// fn_type_source_signature generates the signature of a function which looks like in the V source
pub fn (t &Table) fn_type_source_signature(f &Fn) string {
	mut sig := '('
	for i, arg in f.params {
		if arg.is_mut {
			sig += 'mut '
		}
		// Note: arg name is only added for fmt, else it would causes errors with generics
		if t.is_fmt && arg.name.len > 0 {
			sig += '$arg.name '
		}
		arg_type_sym := t.sym(arg.typ)
		sig += arg_type_sym.name
		if i < f.params.len - 1 {
			sig += ', '
		}
	}
	sig += ')'
	if f.return_type == ovoid_type {
		sig += ' ?'
	} else if f.return_type == rvoid_type {
		sig += ' !'
	} else if f.return_type != void_type {
		return_type_sym := t.sym(f.return_type)
		if f.return_type.has_flag(.optional) {
			sig += ' ?$return_type_sym.name'
		} else if f.return_type.has_flag(.result) {
			sig += ' !$return_type_sym.name'
		} else {
			sig += ' $return_type_sym.name'
		}
	}
	return sig
}

pub fn (t &Table) is_same_method(f &Fn, func &Fn) string {
	if f.return_type != func.return_type {
		s := t.type_to_str(f.return_type)
		return 'expected return type `$s`'
	}
	if f.params.len != func.params.len {
		return 'expected $f.params.len parameter(s), not $func.params.len'
	}

	// interface name() other mut name() : error

	for i in 0 .. f.params.len {
		// don't check receiver for `.typ`
		has_unexpected_type := i > 0 && f.params[i].typ != func.params[i].typ
		// temporary hack for JS ifaces
		lsym := t.sym(f.params[i].typ)
		rsym := t.sym(func.params[i].typ)
		if lsym.language == .js && rsym.language == .js {
			return ''
		}
		has_unexpected_mutability := !f.params[i].is_mut && func.params[i].is_mut

		if has_unexpected_type || has_unexpected_mutability {
			exps := t.type_to_str(f.params[i].typ)
			gots := t.type_to_str(func.params[i].typ)
			if has_unexpected_type {
				return 'expected `$exps`, not `$gots` for parameter $i'
			} else {
				return 'expected `$exps` which is immutable, not `mut $gots`'
			}
		}
	}
	return ''
}

pub fn (t &Table) find_fn(name string) ?Fn {
	if f := t.fns[name] {
		return f
	}
	return none
}

pub fn (t &Table) known_fn(name string) bool {
	t.find_fn(name) or { return false }
	return true
}

pub fn (mut t Table) register_fn(new_fn Fn) {
	t.fns[new_fn.name] = new_fn
	if new_fn.is_pub && new_fn.mod == 'builtin' {
		t.builtin_pub_fns[new_fn.name] = true
	}
}

pub fn (mut t Table) register_interface(idecl InterfaceDecl) {
	t.interfaces[idecl.typ] = idecl
}

pub fn (mut t TypeSymbol) register_method(new_fn Fn) int {
	// returns a method index, stored in the ast.FnDecl
	// for faster lookup in the checker's fn_decl method
	t.methods << new_fn
	return t.methods.len - 1
}

pub fn (t &Table) register_aggregate_method(mut sym TypeSymbol, name string) ?Fn {
	if sym.kind != .aggregate {
		t.panic('Unexpected type symbol: $sym.kind')
	}
	agg_info := sym.info as Aggregate
	// an aggregate always has at least 2 types
	mut found_once := false
	mut new_fn := Fn{}
	for typ in agg_info.types {
		ts := t.sym(typ)
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

pub fn (t &Table) has_method(s &TypeSymbol, name string) bool {
	t.find_method(s, name) or { return false }
	return true
}

// find_method searches from current type up through each parent looking for method
pub fn (t &Table) find_method(s &TypeSymbol, name string) ?Fn {
	mut ts := unsafe { s }
	for {
		if method := ts.find_method(name) {
			return method
		}
		if ts.kind == .aggregate {
			return t.register_aggregate_method(mut ts, name)
		}
		if ts.parent_idx == 0 {
			break
		}
		ts = t.type_symbols[ts.parent_idx]
	}
	return none
}

[params]
pub struct GetEmbedsOptions {
	preceding []Type
}

// get_embeds returns all nested embedded structs
// the hierarchy of embeds is returned as a list
pub fn (t &Table) get_embeds(sym &TypeSymbol, options GetEmbedsOptions) [][]Type {
	mut embeds := [][]Type{}
	unalias_sym := if sym.info is Alias { t.sym(sym.info.parent_type) } else { sym }
	if unalias_sym.info is Struct {
		for embed in unalias_sym.info.embeds {
			embed_sym := t.sym(embed)
			mut preceding := options.preceding.clone()
			preceding << embed
			embeds << t.get_embeds(embed_sym, preceding: preceding)
		}
		if unalias_sym.info.embeds.len == 0 && options.preceding.len > 0 {
			embeds << options.preceding
		}
	}
	return embeds
}

pub fn (t &Table) find_method_from_embeds(sym &TypeSymbol, method_name string) ?(Fn, []Type) {
	if sym.info is Struct {
		mut found_methods := []Fn{}
		mut embed_of_found_methods := []Type{}
		for embed in sym.info.embeds {
			embed_sym := t.sym(embed)
			if m := t.find_method(embed_sym, method_name) {
				found_methods << m
				embed_of_found_methods << embed
			} else {
				method, types := t.find_method_from_embeds(embed_sym, method_name) or { continue }
				found_methods << method
				embed_of_found_methods << embed
				embed_of_found_methods << types
			}
		}
		if found_methods.len == 1 {
			return found_methods[0], embed_of_found_methods
		} else if found_methods.len > 1 {
			return error('ambiguous method `$method_name`')
		}
	} else if sym.info is Interface {
		mut found_methods := []Fn{}
		mut embed_of_found_methods := []Type{}
		for embed in sym.info.embeds {
			embed_sym := t.sym(embed)
			if m := t.find_method(embed_sym, method_name) {
				found_methods << m
				embed_of_found_methods << embed
			} else {
				method, types := t.find_method_from_embeds(embed_sym, method_name) or { continue }
				found_methods << method
				embed_of_found_methods << embed
				embed_of_found_methods << types
			}
		}
		if found_methods.len == 1 {
			return found_methods[0], embed_of_found_methods
		} else if found_methods.len > 1 {
			return error('ambiguous method `$method_name`')
		}
	} else if sym.info is Aggregate {
		for typ in sym.info.types {
			agg_sym := t.sym(typ)
			method, embed_types := t.find_method_from_embeds(agg_sym, method_name) or { continue }
			if embed_types.len != 0 {
				return method, embed_types
			}
		}
	}
	return none
}

// find_method_with_embeds searches for a given method, also looking through embedded fields
pub fn (t &Table) find_method_with_embeds(sym &TypeSymbol, method_name string) ?Fn {
	if func := t.find_method(sym, method_name) {
		return func
	} else {
		// look for embedded field
		first_err := err
		func, _ := t.find_method_from_embeds(sym, method_name) or { return first_err }
		return func
	}
}

pub fn (t &Table) get_embed_methods(sym &TypeSymbol) []Fn {
	mut methods := []Fn{}
	if sym.info is Struct {
		for embed in sym.info.embeds {
			embed_sym := t.sym(embed)
			methods << embed_sym.methods
			methods << t.get_embed_methods(embed_sym)
		}
	}
	return methods
}

fn (t &Table) register_aggregate_field(mut sym TypeSymbol, name string) ?StructField {
	if sym.kind != .aggregate {
		t.panic('Unexpected type symbol: $sym.kind')
	}
	mut agg_info := sym.info as Aggregate
	// an aggregate always has at least 2 types
	mut found_once := false
	mut new_field := StructField{}
	for typ in agg_info.types {
		ts := t.sym(typ)
		if type_field := t.find_field(ts, name) {
			if !found_once {
				found_once = true
				new_field = type_field
			} else if new_field.typ != type_field.typ {
				return error('field `${t.type_to_str(typ)}.$name` type is different')
			}
			new_field = StructField{
				...new_field
				is_mut: new_field.is_mut && type_field.is_mut
				is_pub: new_field.is_pub && type_field.is_pub
			}
		} else {
			return error('type `${t.type_to_str(typ)}` has no field or method `$name`')
		}
	}
	agg_info.fields << new_field
	return new_field
}

pub fn (t &Table) struct_has_field(struct_ &TypeSymbol, name string) bool {
	t.find_field(struct_, name) or { return false }
	return true
}

// struct_fields returns all fields including fields from embeds
// use this instead symbol.info.fields to get all fields
pub fn (t &Table) struct_fields(sym &TypeSymbol) []StructField {
	mut fields := []StructField{}
	if sym.info is Struct {
		fields << sym.info.fields
		for embed in sym.info.embeds {
			embed_sym := t.sym(embed)
			fields << t.struct_fields(embed_sym)
		}
	}
	return fields
}

// search from current type up through each parent looking for field
pub fn (t &Table) find_field(s &TypeSymbol, name string) ?StructField {
	mut ts := unsafe { s }
	for {
		match mut ts.info {
			Struct {
				if field := ts.info.find_field(name) {
					return field
				}
			}
			Aggregate {
				if field := ts.info.find_field(name) {
					return field
				}
				field := t.register_aggregate_field(mut ts, name) or { return err }
				return field
			}
			Interface {
				if field := ts.info.find_field(name) {
					return field
				}
			}
			SumType {
				t.resolve_common_sumtype_fields(ts)
				if field := ts.info.find_field(name) {
					return field
				}
				// mut info := ts.info as SumType
				// TODO a more detailed error so that it's easier to fix?
				return error('field `$name` does not exist or have the same type in all sumtype variants')
			}
			else {}
		}
		if ts.parent_idx == 0 {
			break
		}
		ts = t.type_symbols[ts.parent_idx]
	}
	return none
}

// find_field_from_embeds tries to find a field in the nested embeds
pub fn (t &Table) find_field_from_embeds(sym &TypeSymbol, field_name string) ?(StructField, []Type) {
	if sym.info is Struct {
		mut found_fields := []StructField{}
		mut embeds_of_found_fields := []Type{}
		for embed in sym.info.embeds {
			embed_sym := t.sym(embed)
			if field := t.find_field(embed_sym, field_name) {
				found_fields << field
				embeds_of_found_fields << embed
			} else {
				field, types := t.find_field_from_embeds(embed_sym, field_name) or { continue }
				found_fields << field
				embeds_of_found_fields << embed
				embeds_of_found_fields << types
			}
		}
		if found_fields.len == 1 {
			return found_fields[0], embeds_of_found_fields
		} else if found_fields.len > 1 {
			return error('ambiguous field `$field_name`')
		}
	} else if sym.info is Aggregate {
		for typ in sym.info.types {
			agg_sym := t.sym(typ)
			field, embed_types := t.find_field_from_embeds(agg_sym, field_name) or { continue }
			if embed_types.len > 0 {
				return field, embed_types
			}
		}
	} else if sym.info is Alias {
		unalias_sym := t.sym(sym.info.parent_type)
		return t.find_field_from_embeds(unalias_sym, field_name)
	}
	return none
}

// find_field_with_embeds searches for a given field, also looking through embedded fields
pub fn (t &Table) find_field_with_embeds(sym &TypeSymbol, field_name string) ?StructField {
	if field := t.find_field(sym, field_name) {
		return field
	} else {
		// look for embedded field
		first_err := err
		field, _ := t.find_field_from_embeds(sym, field_name) or { return first_err }
		return field
	}
}

pub fn (t &Table) resolve_common_sumtype_fields(sym_ &TypeSymbol) {
	mut sym := unsafe { sym_ }
	mut info := sym.info as SumType
	if info.found_fields {
		return
	}
	mut field_map := map[string]StructField{}
	mut field_usages := map[string]int{}
	for variant in info.variants {
		mut v_sym := t.final_sym(variant)
		fields := match mut v_sym.info {
			Struct {
				t.struct_fields(v_sym)
			}
			SumType {
				t.resolve_common_sumtype_fields(v_sym)
				v_sym.info.fields
			}
			else {
				[]StructField{}
			}
		}
		for field in fields {
			if field.name !in field_map {
				field_map[field.name] = field
				field_usages[field.name]++
			} else if field.equals(field_map[field.name]) {
				field_usages[field.name]++
			}
		}
	}
	for field, nr_definitions in field_usages {
		if nr_definitions == info.variants.len {
			info.fields << field_map[field]
		}
	}
	info.found_fields = true
	sym.info = info
}

[inline]
pub fn (t &Table) find_type_idx(name string) int {
	return t.type_idxs[name]
}

[inline]
pub fn (t &Table) find_sym(name string) ?&TypeSymbol {
	idx := t.type_idxs[name]
	if idx > 0 {
		return t.type_symbols[idx]
	}
	return none
}

[inline]
pub fn (t &Table) find_sym_and_type_idx(name string) (&TypeSymbol, int) {
	idx := t.type_idxs[name]
	if idx > 0 {
		return t.type_symbols[idx], idx
	}
	return ast.invalid_type_symbol, idx
}

pub const invalid_type_symbol = &TypeSymbol{
	idx: invalid_type_idx
	parent_idx: invalid_type_idx
	language: .v
	mod: 'builtin'
	kind: .placeholder
	name: 'InvalidType'
	cname: 'InvalidType'
}

[inline]
pub fn (t &Table) sym_by_idx(idx int) &TypeSymbol {
	return t.type_symbols[idx]
}

pub fn (t &Table) sym(typ Type) &TypeSymbol {
	idx := typ.idx()
	if idx > 0 {
		return t.type_symbols[idx]
	}
	// this should never happen
	t.panic('sym: invalid type (typ=$typ idx=$idx). Compiler bug. This should never happen. Please report the bug using `v bug file.v`.
')
	return ast.invalid_type_symbol
}

// final_sym follows aliases until it gets to a "real" Type
[inline]
pub fn (t &Table) final_sym(typ Type) &TypeSymbol {
	mut idx := typ.idx()
	if idx > 0 {
		current_symbol := t.type_symbols[idx]
		if current_symbol.kind == .alias {
			idx = (current_symbol.info as Alias).parent_type.idx()
		}
		return t.type_symbols[idx]
	}
	// this should never happen
	t.panic('final_sym: invalid type (typ=$typ idx=$idx). Compiler bug. This should never happen. Please report the bug using `v bug file.v`.')
	return ast.invalid_type_symbol
}

[inline]
pub fn (t &Table) get_type_name(typ Type) string {
	sym := t.sym(typ)
	return sym.name
}

[inline]
pub fn (t &Table) unalias_num_type(typ Type) Type {
	sym := t.sym(typ)
	if sym.kind == .alias {
		pt := (sym.info as Alias).parent_type
		if pt <= char_type && pt >= void_type {
			return pt
		}
	}
	return typ
}

[inline]
pub fn (t &Table) unaliased_type(typ Type) Type {
	sym := t.sym(typ)
	if sym.kind == .alias {
		pt := (sym.info as Alias).parent_type
		return pt
	}
	return typ
}

fn (mut t Table) rewrite_already_registered_symbol(typ TypeSymbol, existing_idx int) int {
	existing_symbol := t.type_symbols[existing_idx]
	$if trace_rewrite_already_registered_symbol ? {
		eprintln('>> rewrite_already_registered_symbol sym: $typ.name | existing_idx: $existing_idx | existing_symbol: $existing_symbol.name')
	}
	if existing_symbol.kind == .placeholder {
		// override placeholder
		t.type_symbols[existing_idx] = &TypeSymbol{
			...typ
			methods: existing_symbol.methods
			idx: existing_idx
		}
		return existing_idx
	}
	// Override the already registered builtin types with the actual
	// v struct declarations in the vlib/builtin module sources:
	if (existing_idx >= string_type_idx && existing_idx <= map_type_idx)
		|| existing_idx == error_type_idx {
		if existing_idx == string_type_idx {
			// existing_type := t.type_symbols[existing_idx]
			unsafe {
				*existing_symbol = &TypeSymbol{
					...typ
					kind: existing_symbol.kind
					idx: existing_idx
				}
			}
		} else {
			t.type_symbols[existing_idx] = &TypeSymbol{
				...typ
				idx: existing_idx
			}
		}
		return existing_idx
	}
	return ast.invalid_type_idx
}

[inline]
pub fn (mut t Table) register_sym(sym TypeSymbol) int {
	mut idx := -2
	$if trace_register_sym ? {
		defer {
			eprintln('>> register_sym: ${sym.name:-60} | idx: $idx')
		}
	}
	mut existing_idx := t.type_idxs[sym.name]
	if existing_idx > 0 {
		idx = t.rewrite_already_registered_symbol(sym, existing_idx)
		if idx != -2 {
			return idx
		}
	}
	if sym.mod == 'main' {
		existing_idx = t.type_idxs[sym.name.trim_string_left('main.')]
		if existing_idx > 0 {
			idx = t.rewrite_already_registered_symbol(sym, existing_idx)
			if idx != -2 {
				return idx
			}
		}
	}
	idx = t.type_symbols.len
	t.type_symbols << &TypeSymbol{
		...sym
	}
	t.type_symbols[idx].idx = idx
	t.type_idxs[sym.name] = idx
	return idx
}

[inline]
pub fn (mut t Table) register_enum_decl(enum_decl EnumDecl) {
	t.enum_decls[enum_decl.name] = enum_decl
}

[inline]
pub fn (mut t Table) register_anon_struct(name string, sym_idx int) {
	t.anon_struct_names[name] = sym_idx
}

pub fn (t &Table) known_type(name string) bool {
	return t.find_type_idx(name) != 0 || t.parsing_type == name
}

// start_parsing_type open the scope during the parsing of a type
// where the type name must include the module prefix
pub fn (mut t Table) start_parsing_type(type_name string) {
	t.parsing_type = type_name
}

pub fn (mut t Table) reset_parsing_type() {
	t.parsing_type = ''
}

pub fn (t &Table) known_type_idx(typ Type) bool {
	if typ == 0 {
		return false
	}
	sym := t.sym(typ)
	match sym.kind {
		.placeholder {
			return sym.language != .v || sym.name.starts_with('C.')
		}
		.array {
			return t.known_type_idx((sym.info as Array).elem_type)
		}
		.array_fixed {
			return t.known_type_idx((sym.info as ArrayFixed).elem_type)
		}
		.map {
			info := sym.info as Map
			return t.known_type_idx(info.key_type) && t.known_type_idx(info.value_type)
		}
		else {}
	}
	return true
}

// array_source_name generates the original name for the v source.
// e. g. []int
[inline]
pub fn (t &Table) array_name(elem_type Type) string {
	elem_type_sym := t.sym(elem_type)
	ptr := if elem_type.is_ptr() { '&'.repeat(elem_type.nr_muls()) } else { '' }
	return '[]$ptr$elem_type_sym.name'
}

[inline]
pub fn (t &Table) array_cname(elem_type Type) string {
	elem_type_sym := t.sym(elem_type)
	mut res := ''
	if elem_type.is_ptr() {
		res = '_ptr'.repeat(elem_type.nr_muls())
	}
	if elem_type_sym.cname.contains('<') {
		type_name := elem_type_sym.cname.replace_each(['<', '_T_', ', ', '_', '>', ''])
		return 'Array_$type_name' + res
	} else {
		return 'Array_$elem_type_sym.cname' + res
	}
}

// array_fixed_source_name generates the original name for the v source.
// e. g. [16][8]int
[inline]
pub fn (t &Table) array_fixed_name(elem_type Type, size int, size_expr Expr) string {
	elem_type_sym := t.sym(elem_type)
	ptr := if elem_type.is_ptr() { '&'.repeat(elem_type.nr_muls()) } else { '' }
	size_str := if size_expr is EmptyExpr || size != 987654321 {
		size.str()
	} else {
		size_expr.str()
	}
	return '[$size_str]$ptr$elem_type_sym.name'
}

[inline]
pub fn (t &Table) array_fixed_cname(elem_type Type, size int) string {
	elem_type_sym := t.sym(elem_type)
	mut res := ''
	if elem_type.is_ptr() {
		res = '_ptr$elem_type.nr_muls()'
	}
	return 'Array_fixed_$elem_type_sym.cname${res}_$size'
}

[inline]
pub fn (t &Table) chan_name(elem_type Type, is_mut bool) string {
	elem_type_sym := t.sym(elem_type)
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
	elem_type_sym := t.sym(elem_type)
	mut suffix := ''
	if is_mut {
		suffix = '_mut'
	} else if elem_type.is_ptr() {
		suffix = '_ptr'
	}
	return 'chan_$elem_type_sym.cname' + suffix
}

[inline]
pub fn (t &Table) promise_name(return_type Type) string {
	if return_type.idx() == void_type_idx {
		return 'Promise<JS.Any,JS.Any>'
	}

	return_type_sym := t.sym(return_type)
	return 'Promise<$return_type_sym.name, JS.Any>'
}

[inline]
pub fn (t &Table) promise_cname(return_type Type) string {
	if return_type == void_type {
		return 'Promise_Any_Any'
	}

	return_type_sym := t.sym(return_type)
	return 'Promise_${return_type_sym.name}_Any'
}

[inline]
pub fn (t &Table) thread_name(return_type Type) string {
	if return_type.idx() == void_type_idx {
		if return_type.has_flag(.optional) {
			return 'thread ?'
		} else {
			return 'thread'
		}
	}
	return_type_sym := t.sym(return_type)
	ptr := if return_type.is_ptr() { '&' } else { '' }
	opt := if return_type.has_flag(.optional) { '?' } else { '' }
	return 'thread $opt$ptr$return_type_sym.name'
}

[inline]
pub fn (t &Table) thread_cname(return_type Type) string {
	if return_type == void_type {
		if return_type.has_flag(.optional) {
			return '__v_thread_Option_void'
		} else {
			return '__v_thread'
		}
	}
	return_type_sym := t.sym(return_type)
	suffix := if return_type.is_ptr() { '_ptr' } else { '' }
	prefix := if return_type.has_flag(.optional) { '_option_' } else { '' }
	return '__v_thread_$prefix$return_type_sym.cname$suffix'
}

// map_source_name generates the original name for the v source.
// e. g. map[string]int
[inline]
pub fn (t &Table) map_name(key_type Type, value_type Type) string {
	key_type_sym := t.sym(key_type)
	value_type_sym := t.sym(value_type)
	ptr := if value_type.is_ptr() { '&' } else { '' }
	return 'map[$key_type_sym.name]$ptr$value_type_sym.name'
}

[inline]
pub fn (t &Table) map_cname(key_type Type, value_type Type) string {
	key_type_sym := t.sym(key_type)
	value_type_sym := t.sym(value_type)
	suffix := if value_type.is_ptr() { '_ptr' } else { '' }
	return 'Map_${key_type_sym.cname}_$value_type_sym.cname' + suffix
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
	return t.register_sym(chan_typ)
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
	return t.register_sym(map_typ)
}

pub fn (mut t Table) find_or_register_thread(return_type Type) int {
	name := t.thread_name(return_type)
	cname := t.thread_cname(return_type)
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx
	}
	// register
	thread_typ := TypeSymbol{
		parent_idx: thread_type_idx
		kind: .thread
		name: name
		cname: cname
		info: Thread{
			return_type: return_type
		}
	}
	return t.register_sym(thread_typ)
}

pub fn (mut t Table) find_or_register_promise(return_type Type) int {
	name := t.promise_name(return_type)

	cname := t.promise_cname(return_type)
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx
	}

	promise_type := TypeSymbol{
		parent_idx: t.type_idxs['Promise']
		kind: .struct_
		name: name
		cname: cname
		info: Struct{
			concrete_types: [return_type, t.type_idxs['JS.Any']]
		}
	}

	// register
	return t.register_sym(promise_type)
}

pub fn (mut t Table) find_or_register_array(elem_type Type) int {
	name := t.array_name(elem_type)
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx
	}
	cname := t.array_cname(elem_type)
	// register
	array_type_ := TypeSymbol{
		parent_idx: array_type_idx
		kind: .array
		name: name
		cname: cname
		info: Array{
			nr_dims: 1
			elem_type: elem_type
		}
	}
	return t.register_sym(array_type_)
}

pub fn (mut t Table) find_or_register_array_with_dims(elem_type Type, nr_dims int) int {
	if nr_dims == 1 {
		return t.find_or_register_array(elem_type)
	}
	return t.find_or_register_array(t.find_or_register_array_with_dims(elem_type, nr_dims - 1))
}

pub fn (mut t Table) find_or_register_array_fixed(elem_type Type, size int, size_expr Expr) int {
	name := t.array_fixed_name(elem_type, size, size_expr)
	// existing
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 {
		return existing_idx
	}
	cname := t.array_fixed_cname(elem_type, size)
	// register
	array_fixed_type := TypeSymbol{
		kind: .array_fixed
		name: name
		cname: cname
		info: ArrayFixed{
			elem_type: elem_type
			size: size
			size_expr: size_expr
		}
	}
	return t.register_sym(array_fixed_type)
}

pub fn (mut t Table) find_or_register_multi_return(mr_typs []Type) int {
	mut name := '('
	mut cname := 'multi_return'
	for i, mr_typ in mr_typs {
		mr_type_sym := t.sym(mktyp(mr_typ))
		ref, cref := if mr_typ.is_ptr() { '&', 'ref_' } else { '', '' }
		name += '$ref$mr_type_sym.name'
		cname += '_$cref$mr_type_sym.cname'
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
	return t.register_sym(mr_type)
}

pub fn (mut t Table) find_or_register_fn_type(f Fn, is_anon bool, has_decl bool) int {
	name := if f.name.len == 0 { 'fn ${t.fn_type_source_signature(f)}' } else { f.name.clone() }
	cname := if f.name.len == 0 {
		'anon_fn_${t.fn_type_signature(f)}'
	} else {
		util.no_dots(f.name.clone()).replace_each([' ', '', '(', '_', ')', ''])
	}
	anon := f.name.len == 0 || is_anon
	existing_idx := t.type_idxs[name]
	if existing_idx > 0 && t.type_symbols[existing_idx].kind != .placeholder {
		return existing_idx
	}
	return t.register_sym(
		kind: .function
		name: name
		cname: cname
		mod: f.mod
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
	return t.register_sym(ph_type)
}

[inline]
pub fn (t &Table) value_type(typ Type) Type {
	sym := t.final_sym(typ)
	if typ.has_flag(.variadic) {
		// ...string => string
		// return typ.clear_flag(.variadic)
		array_info := sym.info as Array
		return array_info.elem_type
	}
	if sym.kind == .array {
		// Check index type
		info := sym.info as Array
		return info.elem_type
	}
	if sym.kind == .array_fixed {
		info := sym.info as ArrayFixed
		return info.elem_type
	}
	if sym.kind == .map {
		info := sym.info as Map
		return info.value_type
	}
	if sym.kind == .string && typ.is_ptr() {
		// (&string)[i] => string
		return string_type
	}
	if sym.kind in [.byteptr, .string] {
		return u8_type
	}
	if typ.is_ptr() {
		// byte* => byte
		// bytes[0] is a byte, not byte*
		return typ.deref()
	}
	return void_type
}

pub fn (mut t Table) register_fn_generic_types(fn_name string) {
	t.fn_generic_types[fn_name] = [][]Type{}
}

pub fn (mut t Table) register_fn_concrete_types(fn_name string, types []Type) bool {
	mut a := t.fn_generic_types[fn_name] or { return false }
	if types in a {
		return false
	}
	a << types
	t.fn_generic_types[fn_name] = a
	return true
}

// TODO: there is a bug when casting sumtype the other way if its pointer
// so until fixed at least show v (not C) error `x(variant) =  y(SumType*)`
pub fn (t &Table) sumtype_has_variant(parent Type, variant Type, is_as bool) bool {
	parent_sym := t.sym(parent)
	if parent_sym.kind == .sum_type {
		parent_info := parent_sym.info as SumType
		var_sym := t.sym(variant)
		match var_sym.kind {
			.aggregate {
				return t.sumtype_check_aggregate_variant(parent, variant, is_as)
			}
			.alias {
				return t.sumtype_check_alias_variant(parent, variant, is_as)
			}
			.function {
				return t.sumtype_check_function_variant(parent_info, variant, is_as)
			}
			else {
				return t.sumtype_check_variant_in_type(parent_info, variant, is_as)
			}
		}
	}
	return false
}

fn (t &Table) sumtype_check_function_variant(parent_info SumType, variant Type, is_as bool) bool {
	variant_fn := (t.sym(variant).info as FnType).func
	variant_fn_sig := t.fn_type_source_signature(variant_fn)

	for v in parent_info.variants {
		v_sym := t.sym(v)
		if v_sym.info is FnType {
			if t.fn_type_source_signature(v_sym.info.func) == variant_fn_sig
				&& (!is_as || v.nr_muls() == variant.nr_muls()) {
				return true
			}
		}
	}
	return false
}

fn (t &Table) sumtype_check_variant_in_type(parent_info SumType, variant Type, is_as bool) bool {
	for v in parent_info.variants {
		if v.idx() == variant.idx() && (!is_as || v.nr_muls() == variant.nr_muls()) {
			return true
		}
	}
	return false
}

fn (t &Table) sumtype_check_aggregate_variant(parent_type Type, aggregate_type &Type, is_as bool) bool {
	aggregate_sym := t.sym(aggregate_type).info as Aggregate
	for var_type in aggregate_sym.types {
		if !t.sumtype_has_variant(parent_type, var_type, is_as) {
			return false
		}
	}
	return true
}

fn (t &Table) sumtype_check_alias_variant(parent_type Type, alias_type Type, is_as bool) bool {
	parent_sym := t.sym(parent_type).info as SumType
	if !t.sumtype_check_variant_in_type(parent_sym, alias_type, is_as) {
		alias_info := t.sym(alias_type).info as Alias
		// The alias is an alias or of the same sumtype parent, or one
		// of the SumType variant. e.g: alias of another sum type.
		// https://github.com/vlang/v/issues/14029
		return parent_type == alias_info.parent_type
			|| t.sumtype_has_variant(parent_type, alias_info.parent_type, is_as)
	}
	// the alias_type is inside one of the variant of the sum type
	return true
}

pub fn (t &Table) is_sumtype_or_in_variant(parent Type, typ Type) bool {
	if typ == 0 {
		return false
	}
	if t.sym(typ).kind == .sum_type && parent.idx() == typ.idx()
		&& parent.nr_muls() == typ.nr_muls() {
		return true
	}
	return t.sumtype_has_variant(parent, typ, false)
}

// only used for debugging V compiler type bugs
pub fn (t &Table) known_type_names() []string {
	mut res := []string{cap: t.type_idxs.len}
	for _, idx in t.type_idxs {
		// Skip `int_literal_type_idx` and `float_literal_type_idx` because they shouldn't be visible to the User.
		if idx !in [0, int_literal_type_idx, float_literal_type_idx] && t.known_type_idx(idx)
			&& t.sym(idx).kind != .function {
			res << t.type_to_str(idx)
		}
	}
	return res
}

// has_deep_child_no_ref returns true if type is struct and has any child or nested child with the type of the given name
// the given name consists of module and name (`mod.Name`)
// it doesn't care about childs that are references
pub fn (t &Table) has_deep_child_no_ref(ts &TypeSymbol, name string) bool {
	if ts.info is Struct {
		for field in ts.info.fields {
			sym := t.sym(field.typ)
			if !field.typ.is_ptr() && (sym.name == name || t.has_deep_child_no_ref(sym, name)) {
				return true
			}
		}
	}
	return false
}

// complete_interface_check does a MxN check for all M interfaces vs all N types, to determine what types implement what interfaces.
// It short circuits most checks when an interface can not possibly be implemented by a type.
pub fn (mut t Table) complete_interface_check() {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
	for tk, mut tsym in t.type_symbols {
		if tsym.kind != .struct_ {
			continue
		}
		for _, mut idecl in t.interfaces {
			if idecl.typ == 0 {
				continue
			}
			// empty interface only generate type cast functions of the current module
			if idecl.methods.len == 0 && idecl.fields.len == 0 && tsym.mod != t.sym(idecl.typ).mod {
				continue
			}
			if t.does_type_implement_interface(tk, idecl.typ) {
				$if trace_types_implementing_each_interface ? {
					eprintln('>>> tsym.mod: $tsym.mod | tsym.name: $tsym.name | tk: $tk | idecl.name: $idecl.name | idecl.typ: $idecl.typ')
				}
				t.iface_types[idecl.name] << tk
			}
		}
	}
}

// bitsize_to_type returns a type corresponding to the bit_size
// Examples:
//
// `8 > i8`
//
// `32 > int`
//
// `123 > panic()`
//
// `128 > [16]u8`
//
// `608 > [76]u8`
pub fn (mut t Table) bitsize_to_type(bit_size int) Type {
	match bit_size {
		8 {
			return i8_type
		}
		16 {
			return i16_type
		}
		32 {
			return int_type
		}
		64 {
			return i64_type
		}
		else {
			if bit_size % 8 != 0 { // there is no way to do `i2131(32)` so this should never be reached
				t.panic('compiler bug: bitsizes must be multiples of 8')
			}
			return new_type(t.find_or_register_array_fixed(u8_type, bit_size / 8, empty_expr))
		}
	}
}

pub fn (t Table) does_type_implement_interface(typ Type, inter_typ Type) bool {
	if typ.idx() == inter_typ.idx() {
		// same type -> already casted to the interface
		return true
	}
	if inter_typ.idx() == error_type_idx && typ.idx() == none_type_idx {
		// `none` "implements" the Error interface
		return true
	}
	sym := t.sym(typ)
	if sym.language != .v {
		return false
	}
	// generic struct don't generate cast interface fn
	if sym.info is Struct {
		if sym.info.is_generic {
			return false
		}
	}
	mut inter_sym := t.sym(inter_typ)
	if sym.kind == .interface_ && inter_sym.kind == .interface_ {
		return false
	}
	if mut inter_sym.info is Interface {
		attrs := t.interfaces[inter_typ].attrs
		for attr in attrs {
			if attr.name == 'single_impl' {
				return false
			}
		}
		// do not check the same type more than once
		for tt in inter_sym.info.types {
			if tt.idx() == typ.idx() {
				return true
			}
		}
		// verify methods
		for imethod in inter_sym.info.methods {
			if method := t.find_method_with_embeds(sym, imethod.name) {
				msg := t.is_same_method(imethod, method)
				if msg.len > 0 {
					return false
				}
				continue
			}
			return false
		}
		// verify fields
		for ifield in inter_sym.info.fields {
			if ifield.typ == voidptr_type {
				// Allow `voidptr` fields in interfaces for now. (for example
				// to enable .db check in vweb)
				if t.struct_has_field(sym, ifield.name) {
					continue
				} else {
					return false
				}
			}
			if field := t.find_field_with_embeds(sym, ifield.name) {
				if ifield.typ != field.typ {
					return false
				} else if ifield.is_mut && !(field.is_mut || field.is_global) {
					return false
				}
				continue
			}
			return false
		}
		inter_sym.info.types << typ
		if !inter_sym.info.types.contains(voidptr_type) {
			inter_sym.info.types << voidptr_type
		}
		return true
	}
	return false
}

// resolve_generic_to_concrete resolves generics to real types T => int.
// Even map[string]map[string]T can be resolved.
// This is used for resolving the generic return type of CallExpr white `unwrap_generic` is used to resolve generic usage in FnDecl.
pub fn (mut t Table) resolve_generic_to_concrete(generic_type Type, generic_names []string, concrete_types []Type) ?Type {
	if generic_names.len != concrete_types.len {
		return none
	}
	mut sym := t.sym(generic_type)
	if sym.name in generic_names {
		index := generic_names.index(sym.name)
		if index >= concrete_types.len {
			return none
		}
		typ := concrete_types[index]
		if typ == 0 {
			return none
		}
		if typ.has_flag(.generic) {
			return typ.derive_add_muls(generic_type).set_flag(.generic)
		} else {
			return typ.derive_add_muls(generic_type).clear_flag(.generic)
		}
	}
	match mut sym.info {
		Array {
			mut elem_type := sym.info.elem_type
			mut elem_sym := t.sym(elem_type)
			mut dims := 1
			for mut elem_sym.info is Array {
				elem_type = elem_sym.info.elem_type
				elem_sym = t.sym(elem_type)
				dims++
			}
			if typ := t.resolve_generic_to_concrete(elem_type, generic_names, concrete_types) {
				idx := t.find_or_register_array_with_dims(typ, dims)
				if typ.has_flag(.generic) {
					return new_type(idx).derive_add_muls(generic_type).set_flag(.generic)
				} else {
					return new_type(idx).derive_add_muls(generic_type).clear_flag(.generic)
				}
			}
		}
		ArrayFixed {
			if typ := t.resolve_generic_to_concrete(sym.info.elem_type, generic_names,
				concrete_types)
			{
				idx := t.find_or_register_array_fixed(typ, sym.info.size, None{})
				if typ.has_flag(.generic) {
					return new_type(idx).derive_add_muls(generic_type).set_flag(.generic)
				} else {
					return new_type(idx).derive_add_muls(generic_type).clear_flag(.generic)
				}
			}
		}
		Chan {
			if typ := t.resolve_generic_to_concrete(sym.info.elem_type, generic_names,
				concrete_types)
			{
				idx := t.find_or_register_chan(typ, typ.nr_muls() > 0)
				if typ.has_flag(.generic) {
					return new_type(idx).derive_add_muls(generic_type).set_flag(.generic)
				} else {
					return new_type(idx).derive_add_muls(generic_type).clear_flag(.generic)
				}
			}
		}
		FnType {
			mut func := sym.info.func
			mut has_generic := false
			if func.return_type.has_flag(.generic) {
				if typ := t.resolve_generic_to_concrete(func.return_type, generic_names,
					concrete_types)
				{
					func.return_type = typ
					if typ.has_flag(.generic) {
						has_generic = true
					}
				}
			}
			func.params = func.params.clone()
			for mut param in func.params {
				if param.typ.has_flag(.generic) {
					if typ := t.resolve_generic_to_concrete(param.typ, generic_names,
						concrete_types)
					{
						param.typ = typ
						if typ.has_flag(.generic) {
							has_generic = true
						}
					}
				}
			}
			func.name = ''
			idx := t.find_or_register_fn_type(func, true, false)
			if has_generic {
				return new_type(idx).derive_add_muls(generic_type).set_flag(.generic)
			} else {
				return new_type(idx).derive_add_muls(generic_type).clear_flag(.generic)
			}
		}
		MultiReturn {
			mut types := []Type{}
			mut type_changed := false
			for ret_type in sym.info.types {
				if typ := t.resolve_generic_to_concrete(ret_type, generic_names, concrete_types) {
					types << typ
					type_changed = true
				} else {
					types << ret_type
				}
			}
			if type_changed {
				idx := t.find_or_register_multi_return(types)
				if types.any(it.has_flag(.generic)) {
					return new_type(idx).derive_add_muls(generic_type).set_flag(.generic)
				} else {
					return new_type(idx).derive_add_muls(generic_type).clear_flag(.generic)
				}
			}
		}
		Map {
			mut type_changed := false
			mut unwrapped_key_type := sym.info.key_type
			mut unwrapped_value_type := sym.info.value_type
			if typ := t.resolve_generic_to_concrete(sym.info.key_type, generic_names,
				concrete_types)
			{
				unwrapped_key_type = typ
				type_changed = true
			}
			if typ := t.resolve_generic_to_concrete(sym.info.value_type, generic_names,
				concrete_types)
			{
				unwrapped_value_type = typ
				type_changed = true
			}
			if type_changed {
				idx := t.find_or_register_map(unwrapped_key_type, unwrapped_value_type)
				if unwrapped_key_type.has_flag(.generic) || unwrapped_value_type.has_flag(.generic) {
					return new_type(idx).derive_add_muls(generic_type).set_flag(.generic)
				} else {
					return new_type(idx).derive_add_muls(generic_type).clear_flag(.generic)
				}
			}
		}
		Struct, Interface, SumType {
			if sym.info.is_generic {
				mut nrt := '$sym.name<'
				for i in 0 .. sym.info.generic_types.len {
					if ct := t.resolve_generic_to_concrete(sym.info.generic_types[i],
						generic_names, concrete_types)
					{
						gts := t.sym(ct)
						nrt += gts.name
						if i != sym.info.generic_types.len - 1 {
							nrt += ', '
						}
					}
				}
				nrt += '>'
				mut idx := t.type_idxs[nrt]
				if idx == 0 {
					idx = t.add_placeholder_type(nrt, .v)
				}
				return new_type(idx).derive_add_muls(generic_type).clear_flag(.generic)
			}
		}
		else {}
	}
	return none
}

fn generic_names_push_with_filter(mut to_names []string, from_names []string) {
	for name in from_names {
		if name !in to_names {
			to_names << name
		}
	}
}

pub fn (mut t Table) generic_type_names(generic_type Type) []string {
	mut names := []string{}
	mut sym := t.sym(generic_type)
	if sym.name.len == 1 && sym.name[0].is_capital() {
		names << sym.name
		return names
	}
	match mut sym.info {
		Array {
			mut elem_type := sym.info.elem_type
			mut elem_sym := t.sym(elem_type)
			mut dims := 1
			for mut elem_sym.info is Array {
				elem_type = elem_sym.info.elem_type
				elem_sym = t.sym(elem_type)
				dims++
			}
			names << t.generic_type_names(elem_type)
		}
		ArrayFixed {
			names << t.generic_type_names(sym.info.elem_type)
		}
		Chan {
			names << t.generic_type_names(sym.info.elem_type)
		}
		FnType {
			mut func := sym.info.func
			if func.return_type.has_flag(.generic) {
				names << t.generic_type_names(func.return_type)
			}
			func.params = func.params.clone()
			for mut param in func.params {
				if param.typ.has_flag(.generic) {
					generic_names_push_with_filter(mut names, t.generic_type_names(param.typ))
				}
			}
		}
		MultiReturn {
			for ret_type in sym.info.types {
				generic_names_push_with_filter(mut names, t.generic_type_names(ret_type))
			}
		}
		Map {
			names << t.generic_type_names(sym.info.key_type)
			generic_names_push_with_filter(mut names, t.generic_type_names(sym.info.value_type))
		}
		Struct, Interface, SumType {
			if sym.info.is_generic {
				names << sym.info.generic_types.map(t.sym(it).name)
			}
		}
		else {}
	}
	return names
}

pub fn (mut t Table) unwrap_generic_type(typ Type, generic_names []string, concrete_types []Type) Type {
	mut final_concrete_types := []Type{}
	mut fields := []StructField{}
	mut needs_unwrap_types := []Type{}
	mut nrt := ''
	mut c_nrt := ''
	ts := t.sym(typ)
	match ts.info {
		Array {
			mut elem_type := ts.info.elem_type
			mut elem_sym := t.sym(elem_type)
			mut dims := 1
			for mut elem_sym.info is Array {
				elem_type = elem_sym.info.elem_type
				elem_sym = t.sym(elem_type)
				dims++
			}
			unwrap_typ := t.unwrap_generic_type(elem_type, generic_names, concrete_types)
			idx := t.find_or_register_array_with_dims(unwrap_typ, dims)
			return new_type(idx).derive_add_muls(typ).clear_flag(.generic)
		}
		ArrayFixed {
			unwrap_typ := t.unwrap_generic_type(ts.info.elem_type, generic_names, concrete_types)
			idx := t.find_or_register_array_fixed(unwrap_typ, ts.info.size, None{})
			return new_type(idx).derive_add_muls(typ).clear_flag(.generic)
		}
		Chan {
			unwrap_typ := t.unwrap_generic_type(ts.info.elem_type, generic_names, concrete_types)
			idx := t.find_or_register_chan(unwrap_typ, unwrap_typ.nr_muls() > 0)
			return new_type(idx).derive_add_muls(typ).clear_flag(.generic)
		}
		Map {
			unwrap_key_type := t.unwrap_generic_type(ts.info.key_type, generic_names,
				concrete_types)
			unwrap_value_type := t.unwrap_generic_type(ts.info.value_type, generic_names,
				concrete_types)
			idx := t.find_or_register_map(unwrap_key_type, unwrap_value_type)
			return new_type(idx).derive_add_muls(typ).clear_flag(.generic)
		}
		Struct, Interface, SumType {
			if !ts.info.is_generic {
				return typ
			}
			nrt = '$ts.name<'
			c_nrt = '${ts.cname}_T_'
			for i in 0 .. ts.info.generic_types.len {
				if ct := t.resolve_generic_to_concrete(ts.info.generic_types[i], generic_names,
					concrete_types)
				{
					gts := t.sym(ct)
					nrt += gts.name
					c_nrt += gts.cname
					if i != ts.info.generic_types.len - 1 {
						nrt += ', '
						c_nrt += '_'
					}
				}
			}
			nrt += '>'
			idx := t.type_idxs[nrt]
			if idx != 0 && t.type_symbols[idx].kind != .placeholder {
				return new_type(idx).derive(typ).clear_flag(.generic)
			} else {
				// fields type translate to concrete type
				fields = ts.info.fields.clone()
				for i in 0 .. fields.len {
					if fields[i].typ.has_flag(.generic) {
						sym := t.sym(fields[i].typ)
						if sym.kind == .struct_ && fields[i].typ.idx() != typ.idx() {
							fields[i].typ = t.unwrap_generic_type(fields[i].typ, generic_names,
								concrete_types)
						} else {
							if t_typ := t.resolve_generic_to_concrete(fields[i].typ, generic_names,
								concrete_types)
							{
								fields[i].typ = t_typ
							}
						}
					}
				}
				// update concrete types
				for i in 0 .. ts.info.generic_types.len {
					if t_typ := t.resolve_generic_to_concrete(ts.info.generic_types[i],
						generic_names, concrete_types)
					{
						final_concrete_types << t_typ
					}
				}
				if final_concrete_types.len > 0 {
					for method in ts.methods {
						for i in 1 .. method.params.len {
							if method.params[i].typ.has_flag(.generic)
								&& method.params[i].typ != method.params[0].typ {
								if method.params[i].typ !in needs_unwrap_types {
									needs_unwrap_types << method.params[i].typ
								}
							}
							if method.return_type.has_flag(.generic)
								&& method.return_type != method.params[0].typ {
								if method.return_type !in needs_unwrap_types {
									needs_unwrap_types << method.return_type
								}
							}
						}
						t.register_fn_concrete_types(method.fkey(), final_concrete_types)
					}
				}
			}
		}
		else {}
	}
	match ts.info {
		Struct {
			mut info := ts.info
			info.is_generic = false
			info.concrete_types = final_concrete_types
			info.parent_type = typ
			info.fields = fields
			new_idx := t.register_sym(
				kind: .struct_
				name: nrt
				cname: util.no_dots(c_nrt)
				mod: ts.mod
				info: info
			)
			for typ_ in needs_unwrap_types {
				t.unwrap_generic_type(typ_, generic_names, concrete_types)
			}
			return new_type(new_idx).derive(typ).clear_flag(.generic)
		}
		SumType {
			mut variants := ts.info.variants.clone()
			for i in 0 .. variants.len {
				if variants[i].has_flag(.generic) {
					sym := t.sym(variants[i])
					if sym.kind in [.struct_, .sum_type, .interface_] {
						variants[i] = t.unwrap_generic_type(variants[i], generic_names,
							concrete_types)
					} else {
						if t_typ := t.resolve_generic_to_concrete(variants[i], generic_names,
							concrete_types)
						{
							variants[i] = t_typ
						}
					}
				}
			}
			mut info := ts.info
			info.is_generic = false
			info.concrete_types = final_concrete_types
			info.parent_type = typ
			info.fields = fields
			info.variants = variants
			new_idx := t.register_sym(
				kind: .sum_type
				name: nrt
				cname: util.no_dots(c_nrt)
				mod: ts.mod
				info: info
			)
			for typ_ in needs_unwrap_types {
				t.unwrap_generic_type(typ_, generic_names, concrete_types)
			}
			return new_type(new_idx).derive(typ).clear_flag(.generic)
		}
		Interface {
			// resolve generic types inside methods
			mut imethods := ts.info.methods.clone()
			for mut method in imethods {
				if unwrap_typ := t.resolve_generic_to_concrete(method.return_type, generic_names,
					concrete_types)
				{
					method.return_type = unwrap_typ
				}
				for mut param in method.params {
					if unwrap_typ := t.resolve_generic_to_concrete(param.typ, generic_names,
						concrete_types)
					{
						param.typ = unwrap_typ
					}
				}
			}
			mut all_methods := unsafe { ts.methods }
			for imethod in imethods {
				for mut method in all_methods {
					if imethod.name == method.name {
						method = imethod
					}
				}
			}
			mut info := ts.info
			info.is_generic = false
			info.concrete_types = final_concrete_types
			info.parent_type = typ
			info.fields = fields
			info.methods = imethods
			new_idx := t.register_sym(
				kind: .interface_
				name: nrt
				cname: util.no_dots(c_nrt)
				mod: ts.mod
				info: info
			)
			mut ts_copy := t.sym(new_idx)
			for method in all_methods {
				ts_copy.register_method(method)
			}
			return new_type(new_idx).derive(typ).clear_flag(.generic)
		}
		else {}
	}
	return typ
}

// Foo<U>{ bar: U } to Foo<T>{ bar: T }
pub fn (mut t Table) replace_generic_type(typ Type, generic_types []Type) {
	mut ts := t.sym(typ)
	match mut ts.info {
		Array {
			mut elem_type := ts.info.elem_type
			mut elem_sym := t.sym(elem_type)
			mut dims := 1
			for mut elem_sym.info is Array {
				elem_type = elem_sym.info.elem_type
				elem_sym = t.sym(elem_type)
				dims++
			}
			t.replace_generic_type(elem_type, generic_types)
		}
		ArrayFixed {
			t.replace_generic_type(ts.info.elem_type, generic_types)
		}
		Chan {
			t.replace_generic_type(ts.info.elem_type, generic_types)
		}
		Map {
			t.replace_generic_type(ts.info.key_type, generic_types)
			t.replace_generic_type(ts.info.value_type, generic_types)
		}
		Struct, Interface, SumType {
			generic_names := ts.info.generic_types.map(t.sym(it).name)
			for i in 0 .. ts.info.fields.len {
				if ts.info.fields[i].typ.has_flag(.generic) {
					if t_typ := t.resolve_generic_to_concrete(ts.info.fields[i].typ, generic_names,
						generic_types)
					{
						ts.info.fields[i].typ = t_typ
					}
				}
			}
			ts.info.generic_types = generic_types
		}
		else {}
	}
}

// generic struct instantiations to concrete types
pub fn (mut t Table) generic_insts_to_concrete() {
	for mut sym in t.type_symbols {
		if sym.kind == .generic_inst {
			info := sym.info as GenericInst
			parent := t.type_symbols[info.parent_idx]
			if parent.kind == .placeholder {
				sym.kind = .placeholder
				continue
			}
			match parent.info {
				Struct {
					mut parent_info := parent.info as Struct
					if !parent_info.is_generic {
						util.verror('generic error', 'struct `$parent.name` is not a generic struct, cannot instantiate to the concrete types')
						continue
					}
					mut fields := parent_info.fields.clone()
					if parent_info.generic_types.len == info.concrete_types.len {
						generic_names := parent_info.generic_types.map(t.sym(it).name)
						for i in 0 .. fields.len {
							if fields[i].typ.has_flag(.generic) {
								if fields[i].typ.idx() != info.parent_idx {
									fields[i].typ = t.unwrap_generic_type(fields[i].typ,
										generic_names, info.concrete_types)
								}
								if t_typ := t.resolve_generic_to_concrete(fields[i].typ,
									generic_names, info.concrete_types)
								{
									fields[i].typ = t_typ
								}
							}
						}
						parent_info.is_generic = false
						parent_info.concrete_types = info.concrete_types.clone()
						parent_info.fields = fields
						parent_info.parent_type = new_type(info.parent_idx).set_flag(.generic)
						sym.info = Struct{
							...parent_info
							is_generic: false
							concrete_types: info.concrete_types.clone()
							fields: fields
							parent_type: new_type(info.parent_idx).set_flag(.generic)
						}
						sym.is_pub = true
						sym.kind = parent.kind

						parent_sym := t.sym(parent_info.parent_type)
						for method in parent_sym.methods {
							if method.generic_names.len == info.concrete_types.len {
								t.register_fn_concrete_types(method.fkey(), info.concrete_types)
							}
						}
					} else {
						util.verror('generic error', 'the number of generic types of struct `$parent.name` is inconsistent with the concrete types')
					}
				}
				Interface {
					mut parent_info := parent.info as Interface
					if !parent_info.is_generic {
						util.verror('generic error', 'interface `$parent.name` is not a generic interface, cannot instantiate to the concrete types')
						continue
					}
					if parent_info.generic_types.len == info.concrete_types.len {
						mut fields := parent_info.fields.clone()
						generic_names := parent_info.generic_types.map(t.sym(it).name)
						for i in 0 .. fields.len {
							if t_typ := t.resolve_generic_to_concrete(fields[i].typ, generic_names,
								info.concrete_types)
							{
								fields[i].typ = t_typ
							}
						}
						mut imethods := parent_info.methods.clone()
						for mut method in imethods {
							method.generic_names.clear()
							if pt := t.resolve_generic_to_concrete(method.return_type,
								generic_names, info.concrete_types)
							{
								method.return_type = pt
							}
							method.params = method.params.clone()
							for mut param in method.params {
								if pt := t.resolve_generic_to_concrete(param.typ, generic_names,
									info.concrete_types)
								{
									param.typ = pt
								}
							}
							sym.register_method(method)
						}
						mut all_methods := unsafe { parent.methods }
						for imethod in imethods {
							for mut method in all_methods {
								if imethod.name == method.name {
									method = imethod
								}
							}
						}
						sym.info = Interface{
							...parent_info
							is_generic: false
							concrete_types: info.concrete_types.clone()
							fields: fields
							methods: imethods
							parent_type: new_type(info.parent_idx).set_flag(.generic)
						}
						sym.is_pub = true
						sym.kind = parent.kind
						sym.methods = all_methods
					} else {
						util.verror('generic error', 'the number of generic types of interface `$parent.name` is inconsistent with the concrete types')
					}
				}
				SumType {
					mut parent_info := parent.info as SumType
					if !parent_info.is_generic {
						util.verror('generic error', 'sumtype `$parent.name` is not a generic sumtype, cannot instantiate to the concrete types')
						continue
					}
					if parent_info.generic_types.len == info.concrete_types.len {
						mut fields := parent_info.fields.clone()
						mut variants := parent_info.variants.clone()
						generic_names := parent_info.generic_types.map(t.sym(it).name)
						for i in 0 .. fields.len {
							if t_typ := t.resolve_generic_to_concrete(fields[i].typ, generic_names,
								info.concrete_types)
							{
								fields[i].typ = t_typ
							}
						}
						for i in 0 .. variants.len {
							if variants[i].has_flag(.generic) {
								t_sym := t.sym(variants[i])
								if t_sym.kind == .struct_ && variants[i].idx() != info.parent_idx {
									variants[i] = t.unwrap_generic_type(variants[i], generic_names,
										info.concrete_types)
								} else {
									if t_typ := t.resolve_generic_to_concrete(variants[i],
										generic_names, info.concrete_types)
									{
										variants[i] = t_typ
									}
								}
							}
						}
						sym.info = SumType{
							...parent_info
							is_generic: false
							concrete_types: info.concrete_types.clone()
							fields: fields
							variants: variants
							parent_type: new_type(info.parent_idx).set_flag(.generic)
						}
						sym.is_pub = true
						sym.kind = parent.kind
					} else {
						util.verror('generic error', 'the number of generic types of sumtype `$parent.name` is inconsistent with the concrete types')
					}
				}
				else {}
			}
		}
	}
}

pub fn (t &Table) is_comptime_type(x Type, y ComptimeType) bool {
	x_kind := t.type_kind(x)
	match y.kind {
		.map_ {
			return x_kind == .map
		}
		.int {
			return x_kind in [.i8, .i16, .int, .i64, .u8, .u16, .u32, .u64, .usize, .isize,
				.int_literal]
		}
		.float {
			return x_kind in [.f32, .f64, .float_literal]
		}
		.struct_ {
			return x_kind == .struct_
		}
		.iface {
			return x_kind == .interface_
		}
		.array {
			return x_kind in [.array, .array_fixed]
		}
		.sum_type {
			return x_kind == .sum_type
		}
		.enum_ {
			return x_kind == .enum_
		}
	}
}

pub fn (t &Table) dependent_names_in_expr(expr Expr) []string {
	mut names := []string{}
	match expr {
		ArrayInit {
			for elem_expr in expr.exprs {
				names << t.dependent_names_in_expr(elem_expr)
			}
		}
		CallExpr {
			for arg in expr.args {
				names << t.dependent_names_in_expr(arg.expr)
			}
			if func := t.find_fn(expr.name) {
				names << func.dep_names
			}
		}
		CastExpr {
			names << t.dependent_names_in_expr(expr.expr)
			names << t.dependent_names_in_expr(expr.arg)
		}
		Ident {
			if expr.kind in [.global, .constant] {
				names << util.no_dots(expr.name)
			}
		}
		IfExpr {
			for branch in expr.branches {
				names << t.dependent_names_in_expr(branch.cond)
				for stmt in branch.stmts {
					names << t.dependent_names_in_stmt(stmt)
				}
			}
		}
		InfixExpr {
			names << t.dependent_names_in_expr(expr.left)
			names << t.dependent_names_in_expr(expr.right)
		}
		MapInit {
			for val in expr.vals {
				names << t.dependent_names_in_expr(val)
			}
		}
		MatchExpr {
			names << t.dependent_names_in_expr(expr.cond)
			for branch in expr.branches {
				for stmt in branch.stmts {
					names << t.dependent_names_in_stmt(stmt)
				}
			}
		}
		ParExpr {
			names << t.dependent_names_in_expr(expr.expr)
		}
		PostfixExpr {
			names << t.dependent_names_in_expr(expr.expr)
		}
		PrefixExpr {
			names << t.dependent_names_in_expr(expr.right)
		}
		StructInit {
			for field in expr.fields {
				names << t.dependent_names_in_expr(field.expr)
			}
		}
		else {}
	}
	return names
}

pub fn (t &Table) dependent_names_in_stmt(stmt Stmt) []string {
	mut names := []string{}
	match stmt {
		AssignStmt {
			for expr in stmt.left {
				names << t.dependent_names_in_expr(expr)
			}
			for expr in stmt.right {
				names << t.dependent_names_in_expr(expr)
			}
		}
		ExprStmt {
			names << t.dependent_names_in_expr(stmt.expr)
		}
		ForInStmt {
			names << t.dependent_names_in_expr(stmt.cond)
			for stmt_ in stmt.stmts {
				names << t.dependent_names_in_stmt(stmt_)
			}
		}
		ForStmt {
			for stmt_ in stmt.stmts {
				names << t.dependent_names_in_stmt(stmt_)
			}
		}
		ForCStmt {
			names << t.dependent_names_in_stmt(stmt.init)
			names << t.dependent_names_in_expr(stmt.cond)
			names << t.dependent_names_in_stmt(stmt.inc)
			for stmt_ in stmt.stmts {
				names << t.dependent_names_in_stmt(stmt_)
			}
		}
		Return {
			for expr in stmt.exprs {
				names << t.dependent_names_in_expr(expr)
			}
		}
		else {}
	}
	return names
}
