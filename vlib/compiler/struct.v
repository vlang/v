// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

import (
	strings
)
// also unions and interfaces


fn (p mut Parser) struct_decl(generic_param_types []string) {
	decl_tok_idx := p.cur_tok_index()
	is_pub := p.tok == .key_pub
	if is_pub {
		p.next()
		p.fspace()
	}
	// V can generate Objective C for integration with Cocoa
	// `[objc_interface:ParentInterface]`
	is_objc := p.attr.starts_with('objc_interface')
	objc_parent := if is_objc { p.attr[15..] } else { '' }
	// interface, union, struct
	is_interface := p.tok == .key_interface
	is_union := p.tok == .key_union
	is_struct := p.tok == .key_struct
	mut cat := key_to_type_cat(p.tok)
	if is_objc {
		cat = .objc_interface
	}
	p.next()
	p.fspace()
	// Get type name
	mut name := p.check_name()
	if name.contains('_') && !p.pref.translated {
		p.error('type names cannot contain `_`')
	}
	if !p.builtin_mod && !name[0].is_capital() {
		p.error('mod=$p.mod struct names must be capitalized: use `struct ${name.capitalize()}`')
	}
	if is_interface && !name.ends_with('er') && name[0] != `I` {
		p.error('interface names temporarily have to end with `er` (e.g. `Speaker`, `Reader`)')
	}
	mut generic_types := map[string]string
	mut is_generic := false
	if p.tok == .lt {
		p.check(.lt)
		for i := 0; ; i++ {
			if generic_param_types.len > 0 && i > generic_param_types.len - 1 {
				p.error('mismatched generic type params')
			}
			type_param := p.check_name()
			generic_types[type_param] = if generic_param_types.len > 0 { generic_param_types[i] } else { '' }
			if p.tok != .comma {
				break
			}
			p.check(.comma)
		}
		p.check(.gt)
		is_generic = true
	}
	is_generic_instance := is_generic && generic_param_types.len > 0
	is_c := name == 'C' && p.tok == .dot
	if is_c {
		/*
		if !p.pref.building_v && !p.fileis('vlib') {
			p.warn('Virtual C structs will soon be removed from the language' +
			'\ndefine the C structs and functions in V')
		}
		*/
		p.check(.dot)
		name = p.check_name()
		cat = .c_struct
		if p.attr == 'typedef' {
			cat = .c_typedef
		}
	}
	if name.len == 1 && !p.pref.building_v && !p.pref.is_repl {
		p.warn('struct names must have more than one character')
	}
	if !is_c && !good_type_name(name) {
		p.error('bad struct name, e.g. use `HttpRequest` instead of `HTTPRequest`')
	}
	// Specify full type name
	if !is_c && !p.builtin_mod && p.mod != 'main' {
		name = p.prepend_mod(name)
	}
	mut typ := p.table.find_type(name)
	if p.pass == .decl && p.table.known_type_fast(typ) {
		// if name in reserved_type_param_names {
		// p.error('name `$name` is reserved for type parameters')
		// } else {
		p.error('type `$name` redeclared')
		// }
	}
	if is_objc {
		// Forward declaration of an Objective-C interface with `@class` :)
		p.gen_typedef('@class $name;')
	}
	else if !is_c {
		kind := if is_union { 'union' } else { 'struct' }
		p.gen_typedef('typedef $kind $name $name;')
	}
	// TODO: handle error
	parser_idx := p.v.get_file_parser_index(p.file_path) or {
		0
	}
	// if !p.scanner.is_vh {
	// parser_idx = p.v.get_file_parser_index(p.file_path) or { panic('cant find parser idx for $p.file_path') }
	// }
	// Register the type
	mut is_ph := false
	if typ.is_placeholder {
		// Update the placeholder
		is_ph = true
		typ.name = name
		typ.mod = p.mod
		typ.is_c = is_c
		typ.is_placeholder = false
		typ.cat = cat
		typ.parent = objc_parent
		typ.is_public = is_pub || p.is_vh
		typ.is_generic = is_generic && !is_generic_instance
		typ.decl_tok_idx = decl_tok_idx
		typ.parser_idx = parser_idx
		p.table.rewrite_type(typ)
	}
	else {
		typ = Type{
			name: name
			mod: p.mod
			is_c: is_c
			cat: cat
			parent: objc_parent
			is_public: is_pub || p.is_vh
			is_generic: is_generic && !is_generic_instance
			decl_tok_idx: decl_tok_idx
			parser_idx: parser_idx
		}
	}
	// Struct `C.Foo` declaration, no body
	if is_c && is_struct && p.tok != .lcbr {
		p.table.register_type(typ)
		return
	}
	// generic struct
	if is_generic {
		// template
		if !is_generic_instance {
			p.table.register_type(typ)
			p.table.generic_struct_params[typ.name] = generic_types.keys()
			// NOTE: remove to store fields in generic struct template
			p.skip_block(false)
			return
		}
		// instance
		else {
			typ.rename_generic_struct(generic_types)
		}
	}
	p.fspace()
	p.check(.lcbr)
	// Struct fields
	mut access_mod := AccessMod.private
	// mut is_pub_field := false
	// mut is_mut := false
	mut names := []string // to avoid dup names TODO alloc perf
	mut fmt_max_len := p.table.max_field_len[name]
	// println('fmt max len = $max_len nrfields=$typ.fields.len pass=$p.pass')
	if (!is_ph && p.first_pass()) || is_generic {
		p.table.register_type(typ)
		// println('registering 1 nrfields=$typ.fields.len')
	}
	mut did_gen_something := false
	mut used := []AccessMod
	mut i := -1
	for p.tok != .rcbr {
		i++
		mut new_access_mod := access_mod
		if p.tok == .key_pub {
			p.fmt_dec()
			p.check(.key_pub)
			if p.tok == .key_mut {
				p.fspace()
				new_access_mod = .public_mut
				p.next() // skip `mut`
			}
			else {
				new_access_mod = .public
			}
			if new_access_mod in used {
				p.error('structs can only have one `pub:`/`pub mut:`, all public fields have to be grouped')
			}
			p.check(.colon)
			p.fmt_inc()
			p.fgen_nl()
		}
		else if p.tok == .key_mut {
			new_access_mod = .private_mut
			if new_access_mod in used {
				p.error('structs can only have one `mut:`, all private mutable fields have to be grouped')
			}
			p.fmt_dec()
			p.check(.key_mut)
			p.check(.colon)
			p.fmt_inc()
			p.fgen_nl()
		}
		else if p.tok == .key_global {
			new_access_mod = .global
			if new_access_mod in used {
				p.error('structs can only have one `__global:`, all global fields have to be grouped')
			}
			p.fmt_dec()
			p.check(.key_global)
			p.check(.colon)
			p.fmt_inc()
			p.fgen_nl()
		}
		if new_access_mod != access_mod {
			used << new_access_mod
		}
		access_mod = new_access_mod
		// (mut) user *User
		// if p.tok == .plus {
		// p.next()
		// }
		// Check if reserved name
		field_name_token_idx := p.cur_tok_index()
		field_name := if name != 'Option' && !is_interface { p.table.var_cgen_name(p.check_name()) } else { p.check_name() }
		if p.pass == .main {
			p.fgen(strings.repeat(` `, fmt_max_len - field_name.len))
		}
		// Check dups
		if field_name in names {
			p.error('duplicate field `$field_name`')
		}
		if p.scanner.is_fmt && p.pass == .decl && field_name.len > fmt_max_len {
			fmt_max_len = field_name.len
		}
		if !is_c && p.mod != 'os' && contains_capital(field_name) {
			p.error('struct fields cannot contain uppercase letters, use snake_case instead')
		}
		names << field_name
		// We are in an interface?
		// `run() string` => run is a method, not a struct field
		if is_interface {
			f := p.interface_method(field_name, name)
			if p.first_pass() {
				p.add_method(typ.name, f)
			}
			continue
		}
		// `pub` access mod
		// access_mod := if is_pub_field { AccessMod.public } else { AccessMod.private}
		p.fspace()
		defer {
			if is_generic_instance {
				p.generic_dispatch = TypeInst{
				}
			}
		}
		if is_generic_instance {
			p.generic_dispatch = TypeInst{
				inst: generic_types
			}
		}
		tt := p.get_type2()
		field_type := tt.name
		if field_type == name {
			p.error_with_token_index('cannot embed struct `$name` in itself (field `$field_name`)', field_name_token_idx)
		}
		// Register ?option type
		if field_type.starts_with('Option_') {
			p.gen_typedef('typedef Option $field_type;')
		}
		p.check_and_register_used_imported_type(field_type)
		is_atomic := p.tok == .key_atomic
		if is_atomic {
			p.next()
		}
		// `a int = 4`
		if p.tok == .assign {
			p.next()
			def_val_type,expr := p.tmp_expr()
			if def_val_type != field_type {
				p.error('expected `$field_type` but got `$def_val_type`')
			}
			// println('pass=$p.pass $typ.name ADDING field=$field_name "$def_val_type" "$expr"')
			if !p.first_pass() {
				p.table.add_default_val(i, typ.name, expr)
			}
		}
		// [ATTR]
		mut attr := ''
		if p.tok == .lsbr {
			p.fspace()
			p.next()
			attr = p.check_name()
			if p.tok == .colon {
				p.check(.colon)
				mut val := ''
				match p.tok {
					.name {
						val = p.check_name()
					}
					.str {
						val = p.check_string()
					}
					else {
						p.error('attribute value should be either name or string')
					}}
				attr += ':' + val
			}
			p.check(.rsbr)
		}
		if attr == 'raw' && field_type != 'string' {
			p.error('struct field with attribute "raw" should be of type "string" but got "$field_type"')
		}
		did_gen_something = true
		is_mut := access_mod in [.private_mut, .public_mut, .global]
		if p.first_pass() || is_generic {
			p.table.add_field(typ.name, field_name, field_type, is_mut, attr, access_mod)
		}
		p.fgen_nl() // newline between struct fields
	}
	if p.scanner.is_fmt && p.pass == .decl {
		p.table.max_field_len[typ.name] = fmt_max_len
	}
	// p.fgen_require_nl()
	p.check(.rcbr)
	if !is_c && !did_gen_something && p.first_pass() {
		p.table.add_field(typ.name, '', 'EMPTY_STRUCT_DECLARATION', false, '', .private)
	}
	p.fgen_nl()
	p.fgen_nl()
	// p.fgenln('//kek')
}
// `User{ foo: bar }`
// tok == struct name
fn (p mut Parser) struct_init(typ_ string) string {
	p.is_struct_init = true
	mut typ := typ_
	mut t := p.table.find_type(typ)
	if !t.is_public && t.mod != p.mod {
		p.error('struct `$t.name` is private')
	}
	// generic struct init
	if p.peek() == .lt {
		p.next()
		p.check(.lt)
		mut type_params := []string
		for {
			mut type_param := p.check_name()
			if type_param in p.generic_dispatch.inst {
				type_param = p.generic_dispatch.inst[type_param]
			}
			type_params << type_param
			if p.tok != .comma {
				break
			}
			p.check(.comma)
		}
		p.dispatch_generic_struct(mut t, type_params)
		t = p.table.find_type(t.name)
		typ = t.name
	}
	if p.gen_struct_init(typ, &t) {
		return typ
	}
	ptr := typ.contains('*')
	mut did_gen_something := false
	// Loop thru all struct init keys and assign values
	// u := User{age:20, name:'bob'}
	// Remember which fields were set, so that we dont have to zero them later
	mut inited_fields := []string
	peek := p.peek()
	if peek == .colon || p.tok == .rcbr {
		for p.tok != .rcbr {
			field := if typ != 'Option' { p.table.var_cgen_name(p.check_name()) } else { p.check_name() }
			if !p.first_pass() && !t.has_field(field) {
				p.error('`$t.name` has no field `$field`')
			}
			if field in inited_fields {
				p.error('already initialized field `$field` in `$t.name`')
			}
			f := t.find_field(field) or {
				p.error('no such field: "$field" in type $typ')
				break
			}
			tt := p.table.find_type(f.typ)
			if tt.is_flag {
				p.error(err_modify_bitfield)
			}
			inited_fields << field
			p.gen_struct_field_init(field)
			p.check(.colon)
			p.fspace()
			p.expected_type = f.typ
			p.check_types(p.bool_expression(), f.typ)
			if p.tok == .comma {
				p.next()
				p.fremove_last()
			}
			if p.tok != .rcbr {
				p.gen(',')
			}
			p.fspace()
			did_gen_something = true
			p.fgen_nl() // newline between struct fields
		}
		// If we already set some fields, need to prepend a comma
		if t.fields.len != inited_fields.len && inited_fields.len > 0 {
			p.gen(',')
		}
		// Zero values: init all fields (ints to 0, strings to '' etc)
		for i, field in t.fields {
			sanitized_name := if typ != 'Option' { p.table.var_cgen_name(field.name) } else { field.name }
			// println('### field.name')
			// Skip if this field has already been assigned to
			if sanitized_name in inited_fields {
				continue
			}
			field_typ := field.typ
			if !p.builtin_mod && field_typ.ends_with('*') && !p.is_c_struct_init && p.mod != 'os' &&
				p.mod != 'ui' {
				p.warn('reference field `${typ}.${field.name}` must be initialized')
			}
			// init map fields
			if field_typ.starts_with('map_') {
				p.gen_struct_field_init(sanitized_name)
				p.gen_empty_map(parse_pointer(field_typ[4..]))
				inited_fields << sanitized_name
				if i != t.fields.len - 1 {
					p.gen(',')
				}
				did_gen_something = true
				continue
			}
			// Did the user provide a default value for this struct field?
			// Use it. Otherwise zero it.
			def_val := if t.default_vals.len > i && t.default_vals[i] != '' { t.default_vals[i] } else { type_default(field_typ) }
			if def_val != '' && def_val != '{0}' {
				p.gen_struct_field_init(sanitized_name)
				p.gen(def_val)
				if i != t.fields.len - 1 {
					p.gen(',')
				}
				did_gen_something = true
			}
		}
	}
	// Point{3,4} syntax
	else {
		mut T := p.table.find_type(typ)
		// Aliases (TODO Hack, implement proper aliases)
		if T.fields.len == 0 && T.parent != '' {
			T = p.table.find_type(T.parent)
		}
		for i, ffield in T.fields {
			expr_typ := p.bool_expression()
			if !p.check_types_no_throw(expr_typ, ffield.typ) {
				p.error('field value #${i+1} `$ffield.name` has type `$ffield.typ`, got `$expr_typ` ')
			}
			tt := p.table.find_type(ffield.typ)
			if tt.is_flag {
				p.error(err_modify_bitfield)
			}
			if i < T.fields.len - 1 {
				if p.tok != .comma {
					p.error('too few values in `$typ` literal (${i+1} instead of $T.fields.len)')
				}
				p.gen(',')
				p.next()
			}
		}
		// Allow `user := User{1,2,3,}`
		// The final comma will be removed by vfmt, since we are not calling `p.fgen()`
		if p.tok == .comma {
			p.next()
		}
		if p.tok != .rcbr {
			p.error('too many fields initialized: `$typ` has $T.fields.len field(s)')
		}
		did_gen_something = true
	}
	if !did_gen_something {
		p.gen('EMPTY_STRUCT_INITIALIZATION')
	}
	p.gen('}')
	if ptr && !p.is_js {
		p.gen(', sizeof($t.name))')
	}
	p.check(.rcbr)
	p.is_struct_init = false
	p.is_c_struct_init = false
	return typ
}

fn (t mut Type) rename_generic_struct(generic_types map[string]string) {
	t.name = t.name + '_T'
	for _, v in generic_types {
		t.name = t.name + '_' + type_to_safe_str(v)
	}
}

fn (p mut Parser) dispatch_generic_struct(t mut Type, type_params []string) {
	mut generic_types := map[string]string
	if t.name in p.table.generic_struct_params {
		mut i := 0
		for _, v in p.table.generic_struct_params[t.name] {
			generic_types[v] = type_params[i]
			i++
		}
		t.rename_generic_struct(generic_types)
		if p.table.known_type(t.name) {
			return
		}
		p.cgen.typedefs << 'typedef struct $t.name $t.name;\n'
	}
	mut gp := p.v.parsers[t.parser_idx]
	gp.is_vgen = true
	saved_state := p.save_state()
	p.clear_state(false, true)
	gp.token_idx = t.decl_tok_idx
	// FIXME: TODO: why are tokens cleared?
	if gp.tokens.len == 0 {
		gp.scanner.pos = 0
		gp.scan_tokens()
	}
	gp.next()
	gp.struct_decl(type_params)
	p.cgen.lines_extra << p.cgen.lines
	p.restore_state(saved_state, false, true)
}

