// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.token
import v.util
import os

fn (mut p Parser) struct_decl(is_anon bool) ast.StructDecl {
	p.top_level_statement_start()
	// save attributes, they will be changed later in fields
	attrs := p.attrs
	p.attrs = []
	start_pos := p.tok.pos()
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	is_union := p.tok.kind == .key_union
	if p.tok.kind == .key_struct {
		p.next()
	} else {
		p.check(.key_union)
	}
	language := if p.tok.lit == 'C' && p.peek_tok.kind == .dot {
		ast.Language.c
	} else if p.tok.lit == 'JS' && p.peek_tok.kind == .dot {
		ast.Language.js
	} else {
		ast.Language.v
	}
	if language != .v {
		p.next() // C || JS
		p.next() // .
	}
	name_pos := p.tok.pos()
	p.check_for_impure_v(language, name_pos)
	if p.disallow_declarations_in_script_mode() {
		return ast.StructDecl{}
	}
	mut name := if is_anon {
		p.table.anon_struct_counter++
		'_VAnonStruct$p.table.anon_struct_counter'
	} else {
		p.check_name()
	}
	if name.len == 1 && name[0].is_capital() {
		p.error_with_pos('single letter capital names are reserved for generic template types.',
			name_pos)
		return ast.StructDecl{}
	}
	if name == 'IError' && p.mod != 'builtin' {
		p.error_with_pos('cannot register struct `IError`, it is builtin interface type',
			name_pos)
	}
	// append module name before any type of parsing to enable recursion parsing
	p.table.start_parsing_type(p.prepend_mod(name))
	defer {
		p.table.reset_parsing_type()
	}
	generic_types, _ := p.parse_generic_types()
	no_body := p.tok.kind != .lcbr
	if language == .v && no_body {
		p.error('`$p.tok.lit` lacks body')
		return ast.StructDecl{}
	}
	if language == .v && !p.builtin_mod && !p.is_translated && name.len > 0 && !name[0].is_capital()
		&& !p.pref.translated && !p.is_translated && !is_anon {
		p.error_with_pos('struct name `$name` must begin with capital letter', name_pos)
		return ast.StructDecl{}
	}
	if name.len == 1 {
		p.error_with_pos('struct names must have more than one character', name_pos)
		return ast.StructDecl{}
	}
	if name in p.imported_symbols {
		p.error_with_pos('cannot register struct `$name`, this type was already imported',
			name_pos)
		return ast.StructDecl{}
	}
	mut orig_name := name
	if language == .c {
		name = 'C.$name'
		orig_name = name
	} else if language == .js {
		name = 'JS.$name'
		orig_name = name
	} else {
		name = p.prepend_mod(name)
	}
	mut ast_fields := []ast.StructField{}
	mut fields := []ast.StructField{}
	mut embed_types := []ast.Type{}
	mut embeds := []ast.Embed{}
	mut embed_field_names := []string{}
	mut mut_pos := -1
	mut pub_pos := -1
	mut pub_mut_pos := -1
	mut global_pos := -1
	mut module_pos := -1
	mut is_field_mut := false
	mut is_field_pub := false
	mut is_field_global := false
	mut last_line := p.prev_tok.pos().line_nr + 1
	mut end_comments := []ast.Comment{}
	if !no_body {
		p.check(.lcbr)
		mut i := 0
		for p.tok.kind != .rcbr {
			mut comments := []ast.Comment{}
			for p.tok.kind == .comment {
				comments << p.comment()
				if p.tok.kind == .rcbr {
					break
				}
			}
			if p.tok.kind == .rcbr {
				end_comments = comments.clone()
				break
			}
			if p.tok.kind == .key_pub {
				p.next()
				if p.tok.kind == .key_mut {
					if pub_mut_pos != -1 {
						p.error('redefinition of `pub mut` section')
						return ast.StructDecl{}
					}
					p.next()
					pub_mut_pos = ast_fields.len
					is_field_pub = true
					is_field_mut = true
					is_field_global = false
				} else {
					if pub_pos != -1 {
						p.error('redefinition of `pub` section')
						return ast.StructDecl{}
					}
					pub_pos = ast_fields.len
					is_field_pub = true
					is_field_mut = false
					is_field_global = false
				}
				p.check(.colon)
			} else if p.tok.kind == .key_mut {
				if mut_pos != -1 {
					p.error('redefinition of `mut` section')
					return ast.StructDecl{}
				}
				p.next()
				p.check(.colon)
				mut_pos = ast_fields.len
				is_field_pub = false
				is_field_mut = true
				is_field_global = false
			} else if p.tok.kind == .key_global {
				if global_pos != -1 {
					p.error('redefinition of `global` section')
					return ast.StructDecl{}
				}
				p.next()
				p.check(.colon)
				global_pos = ast_fields.len
				is_field_pub = true
				is_field_mut = true
				is_field_global = true
			} else if p.tok.kind == .key_module {
				if module_pos != -1 {
					p.error('redefinition of `module` section')
					return ast.StructDecl{}
				}
				p.next()
				p.check(.colon)
				module_pos = ast_fields.len
				is_field_pub = false
				is_field_mut = false
				is_field_global = false
			}
			for p.tok.kind == .comment {
				comments << p.comment()
				if p.tok.kind == .rcbr {
					break
				}
			}
			field_start_pos := p.tok.pos()
			mut is_field_volatile := false
			mut is_field_deprecated := false
			if p.tok.kind == .key_volatile {
				p.next()
				is_field_volatile = true
			}
			is_embed := ((p.tok.lit.len > 1 && p.tok.lit[0].is_capital()
				&& (p.peek_tok.line_nr != p.tok.line_nr || p.peek_tok.kind !in [.name, .amp])
				&& (p.peek_tok.kind != .lsbr || p.peek_token(2).kind != .rsbr))
				|| p.peek_tok.kind == .dot) && language == .v && p.peek_tok.kind != .key_fn
			is_on_top := ast_fields.len == 0 && !(is_field_mut || is_field_global)
			mut field_name := ''
			mut typ := ast.Type(0)
			mut type_pos := token.Pos{}
			mut field_pos := token.Pos{}
			mut anon_struct_decl := ast.StructDecl{}
			if is_embed {
				// struct embedding
				type_pos = p.tok.pos()
				typ = p.parse_type()
				comments << p.eat_comments()
				type_pos = type_pos.extend(p.prev_tok.pos())
				if !is_on_top {
					p.error_with_pos('struct embedding must be declared at the beginning of the struct body',
						type_pos)
					return ast.StructDecl{}
				}
				sym := p.table.sym(typ)
				if typ in embed_types {
					p.error_with_pos('cannot embed `$sym.name` more than once', type_pos)
					return ast.StructDecl{}
				}
				field_name = sym.embed_name()
				if field_name in embed_field_names {
					p.error_with_pos('duplicate field `$field_name`', type_pos)
					return ast.StructDecl{}
				}
				embed_field_names << field_name
				embed_types << typ
				embeds << ast.Embed{
					typ: typ
					pos: type_pos
					comments: comments
				}
			} else {
				// struct field
				field_name = p.check_name()
				for p.tok.kind == .comment {
					comments << p.comment()
					if p.tok.kind == .rcbr {
						break
					}
				}
				p.inside_struct_field_decl = true
				if p.tok.kind == .key_struct {
					// Anon structs
					if p.tok.kind == .key_struct {
						anon_struct_decl = p.struct_decl(true)
						// Find the registered anon struct type, it was registered above in `p.struct_decl()`
						typ = p.table.find_type_idx(anon_struct_decl.name)
					}
				} else {
					typ = p.parse_type()
				}
				p.inside_struct_field_decl = false
				if typ.idx() == 0 {
					// error is set in parse_type
					return ast.StructDecl{}
				}
				type_pos = p.prev_tok.pos()
				field_pos = field_start_pos.extend(type_pos)
			}
			// Comments after type (same line)
			comments << p.eat_comments()
			if p.tok.kind == .lsbr {
				// attrs are stored in `p.attrs`
				p.attributes()
				for fa in p.attrs {
					if fa.name == 'deprecated' {
						is_field_deprecated = true
					}
				}
			}
			mut default_expr := ast.empty_expr
			mut has_default_expr := false
			if !is_embed {
				if p.tok.kind == .assign {
					// Default value
					p.next()
					default_expr = p.expr(0)
					match mut default_expr {
						ast.EnumVal { default_expr.typ = typ }
						// TODO: implement all types??
						else {}
					}
					has_default_expr = true
					comments << p.eat_comments()
				}
				ast_fields << ast.StructField{
					name: field_name
					typ: typ
					pos: field_pos
					type_pos: type_pos
					comments: comments
					i: i
					default_expr: default_expr
					has_default_expr: has_default_expr
					attrs: p.attrs
					is_pub: is_embed || is_field_pub
					is_mut: is_embed || is_field_mut
					is_global: is_field_global
					is_volatile: is_field_volatile
					is_deprecated: is_field_deprecated
					anon_struct_decl: anon_struct_decl
				}
			}
			// save embeds as table fields too, it will be used in generation phase
			fields << ast.StructField{
				name: field_name
				typ: typ
				pos: field_pos
				type_pos: type_pos
				comments: comments
				i: i
				default_expr: default_expr
				has_default_expr: has_default_expr
				attrs: p.attrs
				is_pub: is_embed || is_field_pub
				is_mut: is_embed || is_field_mut
				is_global: is_field_global
				is_volatile: is_field_volatile
				is_deprecated: is_field_deprecated
			}
			p.attrs = []
			i++
		}
		p.top_level_statement_end()
		last_line = p.tok.line_nr
		p.check(.rcbr)
	}
	is_minify := attrs.contains('minify')
	mut sym := ast.TypeSymbol{
		kind: .struct_
		language: language
		name: name
		cname: util.no_dots(name)
		mod: p.mod
		info: ast.Struct{
			embeds: embed_types
			fields: fields
			is_typedef: attrs.contains('typedef')
			is_union: is_union
			is_heap: attrs.contains('heap')
			is_minify: is_minify
			is_generic: generic_types.len > 0
			generic_types: generic_types
			attrs: attrs
			is_anon: is_anon
		}
		is_pub: is_pub
	}
	if p.table.has_deep_child_no_ref(&sym, name) {
		p.error_with_pos('invalid recursive struct `$orig_name`', name_pos)
		return ast.StructDecl{}
	}
	mut ret := p.table.register_sym(sym)
	if is_anon {
		p.table.register_anon_struct(name, ret)
	}
	// allow duplicate c struct declarations
	if ret == -1 && language != .c {
		if _ := p.table.find_fn('main.main') {
			if '.' in os.args {
				p.error_with_pos('multiple `main` functions detected, and you ran `v .`
perhaps there are multiple V programs in this directory, and you need to
run them via `v file.v` instead',
					name_pos)
				return ast.StructDecl{}
			}
		}
		p.error_with_pos('cannot register struct `$name`, another type with this name exists',
			name_pos)
		return ast.StructDecl{}
	}
	p.expr_mod = ''
	return ast.StructDecl{
		name: name
		is_pub: is_pub
		fields: ast_fields
		pos: start_pos.extend_with_last_line(name_pos, last_line)
		mut_pos: mut_pos
		pub_pos: pub_pos
		pub_mut_pos: pub_mut_pos
		global_pos: global_pos
		module_pos: module_pos
		language: language
		is_union: is_union
		attrs: attrs
		end_comments: end_comments
		generic_types: generic_types
		embeds: embeds
	}
}

fn (mut p Parser) struct_init(typ_str string, kind ast.StructInitKind) ast.StructInit {
	first_pos := (if kind == .short_syntax && p.prev_tok.kind == .lcbr { p.prev_tok } else { p.tok }).pos()
	p.struct_init_generic_types = []ast.Type{}
	typ := if kind == .short_syntax { ast.void_type } else { p.parse_type() }
	p.expr_mod = ''
	if kind != .short_syntax {
		p.check(.lcbr)
	}
	pre_comments := p.eat_comments()
	mut fields := []ast.StructInitField{}
	mut i := 0
	no_keys := p.peek_tok.kind != .colon && p.tok.kind != .rcbr && p.tok.kind != .ellipsis // `Vec{a,b,c}
	saved_is_amp := p.is_amp
	p.is_amp = false
	mut update_expr := ast.empty_expr
	mut update_expr_comments := []ast.Comment{}
	mut has_update_expr := false
	for p.tok.kind !in [.rcbr, .rpar, .eof] {
		mut field_name := ''
		mut expr := ast.empty_expr
		mut field_pos := token.Pos{}
		mut first_field_pos := token.Pos{}
		mut comments := []ast.Comment{}
		mut nline_comments := []ast.Comment{}
		is_update_expr := fields.len == 0 && p.tok.kind == .ellipsis
		if no_keys {
			// name will be set later in checker
			expr = p.expr(0)
			field_pos = expr.pos()
			first_field_pos = field_pos
			comments = p.eat_comments(same_line: true)
		} else if is_update_expr {
			// struct updating syntax; f2 := Foo{ ...f, name: 'f2' }
			p.check(.ellipsis)
			update_expr = p.expr(0)
			update_expr_comments << p.eat_comments(same_line: true)
			has_update_expr = true
		} else {
			first_field_pos = p.tok.pos()
			field_name = p.check_name()
			p.check(.colon)
			expr = p.expr(0)
			comments = p.eat_comments(same_line: true)
			last_field_pos := expr.pos()
			field_len := if last_field_pos.len > 0 {
				last_field_pos.pos - first_field_pos.pos + last_field_pos.len
			} else {
				first_field_pos.len + 1
			}
			field_pos = token.Pos{
				line_nr: first_field_pos.line_nr
				pos: first_field_pos.pos
				len: field_len
				col: first_field_pos.col
			}
		}
		i++
		if p.tok.kind == .comma {
			p.next()
		}
		comments << p.eat_comments(same_line: true)
		nline_comments << p.eat_comments()
		if !is_update_expr {
			fields << ast.StructInitField{
				name: field_name
				expr: expr
				pos: field_pos
				name_pos: first_field_pos
				comments: comments
				next_comments: nline_comments
				parent_type: typ
			}
		}
	}
	if kind != .short_syntax {
		p.check(.rcbr)
	}
	p.is_amp = saved_is_amp
	return ast.StructInit{
		unresolved: typ.has_flag(.generic)
		typ_str: typ_str
		typ: typ
		fields: fields
		update_expr: update_expr
		update_expr_comments: update_expr_comments
		has_update_expr: has_update_expr
		name_pos: first_pos
		pos: first_pos.extend(if kind == .short_syntax { p.tok.pos() } else { p.prev_tok.pos() })
		no_keys: no_keys
		is_short_syntax: kind == .short_syntax
		is_anon: kind == .anon
		pre_comments: pre_comments
		generic_types: p.struct_init_generic_types
	}
}

fn (mut p Parser) interface_decl() ast.InterfaceDecl {
	p.top_level_statement_start()
	mut pos := p.tok.pos()
	attrs := p.attrs
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.next() // `interface`
	language := if p.tok.lit == 'C' && p.peek_tok.kind == .dot {
		ast.Language.c
	} else if p.tok.lit == 'JS' && p.peek_tok.kind == .dot {
		ast.Language.js
	} else {
		ast.Language.v
	}
	if language != .v {
		p.next() // C || JS
		p.next() // .
	}
	name_pos := p.tok.pos()
	p.check_for_impure_v(language, name_pos)
	if p.disallow_declarations_in_script_mode() {
		return ast.InterfaceDecl{}
	}
	modless_name := p.check_name()
	if modless_name.len == 1 && modless_name[0].is_capital() {
		p.error_with_pos('single letter capital names are reserved for generic template types.',
			name_pos)
		return ast.InterfaceDecl{}
	}
	if modless_name == 'IError' && p.mod != 'builtin' {
		p.error_with_pos('cannot register interface `IError`, it is builtin interface type',
			name_pos)
	}
	mut interface_name := ''
	if language == .js {
		interface_name = 'JS.' + modless_name
	} else {
		interface_name = p.prepend_mod(modless_name)
	}
	generic_types, _ := p.parse_generic_types()
	// println('interface decl $interface_name')
	p.check(.lcbr)
	pre_comments := p.eat_comments()
	if modless_name in p.imported_symbols {
		p.error_with_pos('cannot register interface `$interface_name`, this type was already imported',
			name_pos)
		return ast.InterfaceDecl{}
	}
	// Declare the type
	reg_idx := p.table.register_sym(
		is_pub: is_pub
		kind: .interface_
		name: interface_name
		cname: util.no_dots(interface_name)
		mod: p.mod
		info: ast.Interface{
			types: []
			is_generic: generic_types.len > 0
			generic_types: generic_types
		}
		language: language
	)
	if reg_idx == -1 {
		p.error_with_pos('cannot register interface `$interface_name`, another type with this name exists',
			name_pos)
		return ast.InterfaceDecl{}
	}
	typ := ast.new_type(reg_idx)
	mut ts := p.table.sym(typ)
	mut info := ts.info as ast.Interface
	// if methods were declared before, it's an error, ignore them
	ts.methods = []ast.Fn{cap: 20}
	// Parse fields or methods
	mut fields := []ast.StructField{cap: 20}
	mut methods := []ast.FnDecl{cap: 20}
	mut embeds := []ast.InterfaceEmbedding{}
	mut is_mut := false
	mut mut_pos := -1
	for p.tok.kind != .rcbr && p.tok.kind != .eof {
		if p.tok.kind == .name && p.tok.lit.len > 0 && p.tok.lit[0].is_capital()
			&& (p.peek_tok.line_nr != p.tok.line_nr
			|| p.peek_tok.kind !in [.name, .amp, .lsbr, .lpar]) {
			iface_pos := p.tok.pos()
			mut iface_name := p.tok.lit
			iface_type := p.parse_type()
			if iface_name == 'JS' {
				iface_name = p.table.sym(iface_type).name
			}
			comments := p.eat_comments()
			embeds << ast.InterfaceEmbedding{
				name: iface_name
				typ: iface_type
				pos: iface_pos
				comments: comments
			}
			if p.tok.kind == .rcbr {
				break
			}
			continue
		}

		// Check embedded interface from external module
		if p.tok.kind == .name && p.peek_tok.kind == .dot {
			if p.tok.lit !in p.imports {
				p.error_with_pos('mod `$p.tok.lit` not imported', p.tok.pos())
				break
			}
			mod_name := p.tok.lit
			from_mod_typ := p.parse_type()
			from_mod_name := '${mod_name}.$p.prev_tok.lit'
			if from_mod_name.is_lower() {
				p.error_with_pos('The interface name need to have the pascal case', p.prev_tok.pos())
				break
			}
			comments := p.eat_comments()
			embeds << ast.InterfaceEmbedding{
				name: from_mod_name
				typ: from_mod_typ
				pos: p.prev_tok.pos()
				comments: comments
			}
			if p.tok.kind == .rcbr {
				break
			}
		}

		if p.tok.kind == .key_mut {
			if is_mut {
				p.error_with_pos('redefinition of `mut` section', p.tok.pos())
				return ast.InterfaceDecl{}
			}
			p.next()
			p.check(.colon)
			is_mut = true
			mut_pos = fields.len
		}
		if p.peek_tok.kind == .lt {
			p.error_with_pos("no need to add generic type names in generic interface's method",
				p.peek_tok.pos())
			return ast.InterfaceDecl{}
		}
		if p.peek_tok.kind == .lpar {
			method_start_pos := p.tok.pos()
			line_nr := p.tok.line_nr
			name := p.check_name()

			if name in ['type_name', 'type_idx'] {
				p.error_with_pos('cannot override built-in method `$name`', method_start_pos)
				return ast.InterfaceDecl{}
			}
			if ts.has_method(name) {
				p.error_with_pos('duplicate method `$name`', method_start_pos)
				return ast.InterfaceDecl{}
			}
			// field_names << name
			args2, _, is_variadic := p.fn_args() // TODO merge ast.Param and ast.Arg to avoid this
			mut args := [
				ast.Param{
					name: 'x'
					is_mut: is_mut
					typ: typ
					is_hidden: true
				},
			]
			args << args2
			mut method := ast.FnDecl{
				name: name
				short_name: name
				mod: p.mod
				params: args
				file: p.file_name
				return_type: ast.void_type
				is_variadic: is_variadic
				is_pub: true
				pos: method_start_pos.extend(p.prev_tok.pos())
				scope: p.scope
			}
			if p.tok.kind.is_start_of_type() && p.tok.line_nr == line_nr {
				method.return_type_pos = p.tok.pos()
				method.return_type = p.parse_type()
				method.return_type_pos = method.return_type_pos.extend(p.tok.pos())
				method.pos = method.pos.extend(method.return_type_pos)
			}
			mcomments := p.eat_comments(same_line: true)
			mnext_comments := p.eat_comments()
			method.comments = mcomments
			method.next_comments = mnext_comments
			methods << method
			// println('register method $name')
			tmethod := ast.Fn{
				name: name
				params: args
				pos: method.pos
				return_type: method.return_type
				is_variadic: is_variadic
				is_pub: true
				is_method: true
				receiver_type: typ
			}
			ts.register_method(tmethod)
			info.methods << tmethod
		} else {
			// interface fields
			field_pos := p.tok.pos()
			field_name := p.check_name()
			mut type_pos := p.tok.pos()
			field_typ := p.parse_type()
			type_pos = type_pos.extend(p.prev_tok.pos())
			mut comments := []ast.Comment{}
			for p.tok.kind == .comment {
				comments << p.comment()
				if p.tok.kind == .rcbr {
					break
				}
			}
			fields << ast.StructField{
				name: field_name
				pos: field_pos
				type_pos: type_pos
				typ: field_typ
				comments: comments
				is_pub: true
			}
			info.fields << ast.StructField{
				name: field_name
				typ: field_typ
				is_pub: true
				is_mut: is_mut
			}
		}
	}
	info.embeds = embeds.map(it.typ)
	ts.info = info
	p.top_level_statement_end()
	p.check(.rcbr)
	pos = pos.extend_with_last_line(p.prev_tok.pos(), p.prev_tok.line_nr)
	res := ast.InterfaceDecl{
		name: interface_name
		language: language
		typ: typ
		fields: fields
		methods: methods
		embeds: embeds
		is_pub: is_pub
		attrs: attrs
		pos: pos
		pre_comments: pre_comments
		generic_types: generic_types
		mut_pos: mut_pos
		name_pos: name_pos
	}
	p.table.register_interface(res)
	return res
}
