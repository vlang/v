// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module compiler

// also unions and interfaces
fn (p mut Parser) struct_decl() {
	is_pub := p.tok == .key_pub
	if is_pub {
		p.next()
	}	
	// V can generate Objective C for integration with Cocoa
	// `[objc_interface:ParentInterface]`
	is_objc := p.attr.starts_with('objc_interface')
	objc_parent := if is_objc { p.attr.right(15) } else { '' }
	// interface, union, struct
	is_interface := p.tok == .key_interface
	is_union := p.tok == .key_union
	is_struct := p.tok == .key_struct
	mut cat := key_to_type_cat(p.tok)
	if is_objc {
		cat = .objc_interface
	}
	p.fgen(p.tok.str() + ' ')
	// Get type name
	p.next()
	mut name := p.check_name()
	if name.contains('_') && !p.pref.translated {
		p.error('type names cannot contain `_`')
	}
	if !p.builtin_mod && !name[0].is_capital() {
		p.error('struct names must be capitalized: use `struct ${name.capitalize()}`')
	}
	if is_interface && !name.ends_with('er') {
		p.error('interface names temporarily have to end with `er` (e.g. `Speaker`, `Reader`)')
	}
	is_c := name == 'C' && p.tok == .dot
	if is_c {
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
		//if name in reserved_type_param_names {
			//p.error('name `$name` is reserved for type parameters')
		//} else {
		p.error('type `$name` redeclared')
		//}
	}
	if is_objc {
		// Forward declaration of an Objective-C interface with `@class` :)
		p.gen_typedef('@class $name;')
	}
	else if !is_c {
		kind := if is_union {'union'} else {'struct'}
		p.gen_typedef('typedef $kind $name $name;')
	}
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
		p.table.rewrite_type(typ)
	}
	else {
		typ = Type {
			name: name
			mod: p.mod
			is_c: is_c
			cat: cat
			parent: objc_parent
			is_public: is_pub
		}
	}
	// Struct `C.Foo` declaration, no body
	if is_c && is_struct && p.tok != .lcbr {
		p.table.register_type2(typ)
		return
	}
	p.fgen(' ')
	p.check(.lcbr)
	// Struct fields
	mut is_pub_field := false
	mut is_mut := false
	mut names := []string// to avoid dup names TODO alloc perf
/*
	mut fmt_max_len := 0
	for field in typ.fields  {
		if field.name.len > max_len {
			fmt_max_len = field.name.len
		}
	}
	println('fmt max len = $max_len nrfields=$typ.fields.len pass=$p.pass')
*/

	if !is_ph && p.first_pass() {
		p.table.register_type2(typ)
		//println('registering 1 nrfields=$typ.fields.len')
	}

	mut did_gen_something := false
	for p.tok != .rcbr {
		if p.tok == .key_pub {
			if is_pub_field {
				p.error('structs can only have one `pub:`, all public fields have to be grouped')
			}
			is_pub_field = true
			p.fmt_dec()
			p.check(.key_pub)
			if p.tok != .key_mut {
				p.check(.colon)
			}
			p.fmt_inc()
			p.fgenln('')
		}
		if p.tok == .key_mut {
			if is_mut {
				p.error('structs can only have one `mut:`, all private mutable fields have to be grouped')
			}
			is_mut = true
			p.fmt_dec()
			p.check(.key_mut)
			if p.tok != .key_mut {
				p.check(.colon)
			}
			p.fmt_inc()
			p.fgenln('')
		}
		// if is_pub {
		// }
		// (mut) user *User
		// if p.tok == .plus {
		// p.next()
		// }
		// Check if reserved name
		field_name_token_idx := p.cur_tok_index()
		field_name := if name != 'Option' { p.table.var_cgen_name(p.check_name()) } else { p.check_name() }
		// Check dups
		if field_name in names {
			p.error('duplicate field `$field_name`')
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
		access_mod := if is_pub_field { AccessMod.public } else { AccessMod.private}
		p.fgen(' ')
		field_type := p.get_type()
		if field_type == name {
			p.error_with_token_index( 'cannot embed struct `$name` in itself (field `$field_name`)', field_name_token_idx)
		}
		p.check_and_register_used_imported_type(field_type)
		is_atomic := p.tok == .key_atomic
		if is_atomic {
			p.next()
		}
		// [ATTR]
		mut attr := ''
		if p.tok == .lsbr {
			p.next()
			attr = p.check_name()
			if p.tok == .colon {
				p.check(.colon)
				mut val := ''
				match p.tok {
					.name { val = p.check_name() }
					.str { val = p.check_string() }
					else {
						p.error('attribute value should be either name or string')
					}
				}
				attr += ':' + val
			}
			p.check(.rsbr)
		}
		if attr == 'raw' && field_type != 'string' {
			p.error('struct field with attribute "raw" should be of type "string" but got "$field_type"')
		}

		did_gen_something = true
		if p.first_pass() {
			p.table.add_field(typ.name, field_name, field_type, is_mut, attr, access_mod)
		}
		p.fgenln('')
	}
	p.check(.rcbr)
	if !is_c {
		if !did_gen_something {
			if p.first_pass() {
				p.table.add_field(typ.name, '', 'EMPTY_STRUCT_DECLARATION', false, '', .private)
			}
		}
	}
	p.fgenln('\n')
}

// `User{ foo: bar }`
fn (p mut Parser) struct_init(typ string) string {
	p.is_struct_init = true
	t := p.table.find_type(typ)
	if !t.is_public && t.mod != p.mod {
		p.warn('type `$t.name` is private')
	}	
	if p.gen_struct_init(typ, t) { return typ }
	p.scanner.fmt_out.cut(typ.len)
	ptr := typ.contains('*')
	mut did_gen_something := false
	// Loop thru all struct init keys and assign values
	// u := User{age:20, name:'bob'}
	// Remember which fields were set, so that we dont have to zero them later
	mut inited_fields := []string
	peek := p.peek()
	if peek == .colon || p.tok == .rcbr {
		for p.tok != .rcbr {
			field := if typ != 'Option' { p.table.var_cgen_name( p.check_name() ) } else { p.check_name() }
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
			inited_fields << field
			p.gen_struct_field_init(field)
			p.check(.colon)
			p.fspace()
			p.check_types(p.bool_expression(),  f.typ)
			if p.tok == .comma {
				p.next()
			}
			if p.tok != .rcbr {
				p.gen(',')
			}
			p.fgenln('')
			did_gen_something = true
		}
		// If we already set some fields, need to prepend a comma
		if t.fields.len != inited_fields.len && inited_fields.len > 0 {
			p.gen(',')
		}
		// Zero values: init all fields (ints to 0, strings to '' etc)
		for i, field in t.fields {
			sanitized_name := if typ != 'Option' { p.table.var_cgen_name( field.name ) } else { field.name }
			// println('### field.name')
			// Skip if this field has already been assigned to
			if sanitized_name in inited_fields {
				continue
			}
			field_typ := field.typ
			if !p.builtin_mod && field_typ.ends_with('*') && field_typ.contains('Cfg') {
				p.error('pointer field `${typ}.${field.name}` must be initialized')
			}
			// init map fields
			if field_typ.starts_with('map_') {
				p.gen_struct_field_init(sanitized_name)
				p.gen_empty_map(field_typ.right(4))
				inited_fields << sanitized_name
				if i != t.fields.len - 1 {
					p.gen(',')
				}
				did_gen_something = true
				continue
			}
			def_val := type_default(field_typ)
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


