// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table
import v.token
import v.util

fn (mut p Parser) struct_decl() ast.StructDecl {
	p.top_level_statement_start()
	// save attributes, they will be changed later in fields
	attrs := p.attrs
	p.attrs = []
	start_pos := p.tok.position()
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
		table.Language.c
	} else if p.tok.lit == 'JS' && p.peek_tok.kind == .dot {
		table.Language.js
	} else {
		table.Language.v
	}
	if language != .v {
		p.next() // C || JS
		p.next() // .
	}
	name_pos := p.tok.position()
	mut name := p.check_name()
	// defer {
	// if name.contains('App') {
	// println('end of struct decl $name')
	// }
	// }
	if name.len == 1 && name[0].is_capital() {
		p.error_with_pos('single letter capital names are reserved for generic template types.',
			name_pos)
	}
	mut generic_types := []table.Type{}
	if p.tok.kind == .lt {
		p.next()
		for {
			generic_types << p.parse_type()
			if p.tok.kind != .comma {
				break
			}
			p.next()
		}
		p.check(.gt)
	}
	no_body := p.tok.kind != .lcbr
	if language == .v && no_body {
		p.error('`$p.tok.lit` lacks body')
	}
	if language == .v &&
		p.mod != 'builtin' && name.len > 0 && !name[0].is_capital() && !p.pref.translated {
		p.error_with_pos('struct name `$name` must begin with capital letter', name_pos)
	}
	if name.len == 1 {
		p.error_with_pos('struct names must have more than one character', name_pos)
	}
	// println('struct decl $name')
	mut ast_fields := []ast.StructField{}
	mut fields := []table.Field{}
	mut embedded_structs := []table.Type{}
	mut mut_pos := -1
	mut pub_pos := -1
	mut pub_mut_pos := -1
	mut global_pos := -1
	mut is_field_mut := false
	mut is_field_pub := false
	mut is_field_global := false
	mut end_comments := []ast.Comment{}
	if !no_body {
		p.check(.lcbr)
		for p.tok.kind != .rcbr {
			mut comments := []ast.Comment{}
			for p.tok.kind == .comment {
				comments << p.comment()
				if p.tok.kind == .rcbr {
					break
				}
			}
			if p.tok.kind == .rcbr {
				end_comments = comments
				break
			}
			if p.tok.kind == .key_pub {
				p.next()
				if p.tok.kind == .key_mut {
					if pub_mut_pos != -1 {
						p.error('redefinition of `pub mut` section')
					}
					p.next()
					pub_mut_pos = fields.len
					is_field_pub = true
					is_field_mut = true
					is_field_global = false
				} else {
					if pub_pos != -1 {
						p.error('redefinition of `pub` section')
					}
					pub_pos = fields.len
					is_field_pub = true
					is_field_mut = false
					is_field_global = false
				}
				p.check(.colon)
			} else if p.tok.kind == .key_mut {
				if mut_pos != -1 {
					p.error('redefinition of `mut` section')
				}
				p.next()
				p.check(.colon)
				mut_pos = fields.len
				is_field_pub = false
				is_field_mut = true
				is_field_global = false
			} else if p.tok.kind == .key_global {
				if global_pos != -1 {
					p.error('redefinition of `global` section')
				}
				p.next()
				p.check(.colon)
				global_pos = fields.len
				is_field_pub = true
				is_field_mut = true
				is_field_global = true
			}
			for p.tok.kind == .comment {
				comments << p.comment()
				if p.tok.kind == .rcbr {
					break
				}
			}
			field_start_pos := p.tok.position()
			is_embed := ((p.tok.lit.len > 1 && p.tok.lit[0].is_capital()) ||
				p.peek_tok.kind == .dot) &&
				language == .v
			mut field_name := ''
			mut typ := table.Type(0)
			mut type_pos := token.Position{}
			mut field_pos := token.Position{}
			if is_embed {
				// struct embedding
				typ = p.parse_type()
				sym := p.table.get_type_symbol(typ)
				// main.Abc<int> => Abc
				mut symbol_name := sym.name.split('.')[1]
				// remove generic part from name
				if '<' in symbol_name {
					symbol_name = symbol_name.split('<')[0]
				}
				for p.tok.kind == .comment {
					comments << p.comment()
					if p.tok.kind == .rcbr {
						break
					}
				}
				type_pos = p.prev_tok.position()
				field_pos = p.prev_tok.position()
				field_name = symbol_name
				if typ in embedded_structs {
					p.error_with_pos('cannot embed `$field_name` more than once', type_pos)
				}
				embedded_structs << typ
			} else {
				// struct field
				field_name = p.check_name()
				for p.tok.kind == .comment {
					comments << p.comment()
					if p.tok.kind == .rcbr {
						break
					}
				}
				typ = p.parse_type()
				type_pos = p.prev_tok.position()
				field_pos = field_start_pos.extend(type_pos)
			}
			// println(p.tok.position())
			// Comments after type (same line)
			comments << p.eat_comments()
			if p.tok.kind == .lsbr {
				// attrs are stored in `p.attrs`
				p.attributes()
			}
			mut default_expr := ast.Expr{}
			mut has_default_expr := false
			if !is_embed {
				if p.tok.kind == .assign {
					// Default value
					p.next()
					// default_expr = p.tok.lit
					// p.expr(0)
					default_expr = p.expr(0)
					match mut default_expr {
						ast.EnumVal { default_expr.typ = typ }
						// TODO: implement all types??
						else {}
					}
					has_default_expr = true
				}
			}
			// TODO merge table and ast Fields?
			ast_fields << ast.StructField{
				name: field_name
				pos: field_pos
				type_pos: type_pos
				typ: typ
				comments: comments
				default_expr: default_expr
				has_default_expr: has_default_expr
				attrs: p.attrs
				is_public: is_field_pub
				is_embed: is_embed
			}
			fields << table.Field{
				name: field_name
				typ: typ
				default_expr: ast.ex2fe(default_expr)
				has_default_expr: has_default_expr
				is_pub: is_field_pub
				is_mut: is_field_mut
				is_global: is_field_global
				attrs: p.attrs
				is_embed: is_embed
			}
			p.attrs = []
		}
		p.top_level_statement_end()
		p.check(.rcbr)
	}
	if language == .c {
		name = 'C.$name'
	} else if language == .js {
		name = 'JS.$name'
	} else {
		name = p.prepend_mod(name)
	}
	t := table.TypeSymbol{
		kind: .struct_
		name: name
		language: language
		source_name: name
		mod: p.mod
		info: table.Struct{
			fields: fields
			is_typedef: attrs.contains('typedef')
			is_union: is_union
			is_ref_only: attrs.contains('ref_only')
			generic_types: generic_types
		}
		is_public: is_pub
	}
	mut ret := 0
	if p.builtin_mod && t.name in table.builtin_type_names {
		// this allows overiding the builtins type
		// with the real struct type info parsed from builtin
		ret = p.table.register_builtin_type_symbol(t)
	} else {
		// println('reg type symbol $name mod=$p.mod')
		ret = p.table.register_type_symbol(t)
	}
	if ret == -1 {
		p.error_with_pos('cannot register struct `$name`, another type with this name exists',
			name_pos)
	}
	p.expr_mod = ''
	return ast.StructDecl{
		name: name
		is_pub: is_pub
		fields: ast_fields
		pos: start_pos.extend(name_pos)
		mut_pos: mut_pos
		pub_pos: pub_pos
		pub_mut_pos: pub_mut_pos
		language: language
		is_union: is_union
		attrs: attrs
		end_comments: end_comments
		gen_types: generic_types
	}
}

fn (mut p Parser) struct_init(short_syntax bool) ast.StructInit {
	first_pos := p.tok.position()
	typ := if short_syntax { table.void_type } else { p.parse_type() }
	p.expr_mod = ''
	// sym := p.table.get_type_symbol(typ)
	// p.warn('struct init typ=$sym.name')
	if !short_syntax {
		p.check(.lcbr)
	}
	pre_comments := p.eat_comments()
	mut fields := []ast.StructInitField{}
	mut i := 0
	no_keys := p.peek_tok.kind != .colon && p.tok.kind != .rcbr // `Vec{a,b,c}
	// p.warn(is_short_syntax.str())
	saved_is_amp := p.is_amp
	p.is_amp = false
	for p.tok.kind != .rcbr && p.tok.kind != .rpar {
		mut field_name := ''
		mut expr := ast.Expr{}
		mut field_pos := token.Position{}
		mut comments := []ast.Comment{}
		if no_keys {
			// name will be set later in checker
			expr = p.expr(0)
			field_pos = expr.position()
			comments = p.eat_comments()
		} else {
			first_field_pos := p.tok.position()
			field_name = p.check_name()
			p.check(.colon)
			expr = p.expr(0)
			comments = p.eat_comments()
			last_field_pos := expr.position()
			field_pos = token.Position{
				line_nr: first_field_pos.line_nr
				pos: first_field_pos.pos
				len: last_field_pos.pos - first_field_pos.pos + last_field_pos.len
			}
		}
		i++
		if p.tok.kind == .comma {
			p.next()
		}
		comments << p.eat_comments()
		fields << ast.StructInitField{
			name: field_name
			expr: expr
			pos: field_pos
			comments: comments
		}
	}
	last_pos := p.tok.position()
	if !short_syntax {
		p.check(.rcbr)
	}
	p.is_amp = saved_is_amp
	node := ast.StructInit{
		typ: typ
		fields: fields
		pos: token.Position{
			line_nr: first_pos.line_nr
			pos: first_pos.pos
			len: last_pos.pos - first_pos.pos + last_pos.len
		}
		is_short: no_keys
		pre_comments: pre_comments
	}
	return node
}

fn (mut p Parser) interface_decl() ast.InterfaceDecl {
	p.top_level_statement_start()
	start_pos := p.tok.position()
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.next() // `interface`
	name_pos := p.tok.position()
	interface_name := p.prepend_mod(p.check_name())
	// println('interface decl $interface_name')
	p.check(.lcbr)
	pre_comments := p.eat_comments()
	// Declare the type
	reg_idx := p.table.register_type_symbol(table.TypeSymbol{
		kind: .interface_
		name: interface_name
		source_name: interface_name
		mod: p.mod
		info: table.Interface{
			types: []
		}
	})
	if reg_idx == -1 {
		p.error_with_pos('cannot register interface `$interface_name`, another type with this name exists',
			name_pos)
	}
	typ := table.new_type(reg_idx)
	mut ts := p.table.get_type_symbol(typ)
	// if methods were declared before, it's an error, ignore them
	ts.methods = []table.Fn{cap: 20}
	// Parse methods
	mut methods := []ast.FnDecl{cap: 20}
	for p.tok.kind != .rcbr && p.tok.kind != .eof {
		method_start_pos := p.tok.position()
		line_nr := p.tok.line_nr
		name := p.check_name()
		if ts.has_method(name) {
			p.error_with_pos('duplicate method `$name`', method_start_pos)
		}
		if util.contains_capital(name) {
			p.error('interface methods cannot contain uppercase letters, use snake_case instead')
		}
		// field_names << name
		args2, _, _ := p.fn_args() // TODO merge table.Param and ast.Arg to avoid this
		sym := p.table.get_type_symbol(typ)
		mut args := [table.Param{
			name: 'x'
			typ: typ
			type_source_name: sym.source_name
			is_hidden: true
		}]
		args << args2
		mut method := ast.FnDecl{
			name: name
			mod: p.mod
			params: args
			file: p.file_name
			return_type: table.void_type
			is_pub: true
			pos: method_start_pos.extend(p.prev_tok.position())
		}
		if p.tok.kind.is_start_of_type() && p.tok.line_nr == line_nr {
			method.return_type = p.parse_type()
		}
		mcomments := p.eat_comments()
		method.comments = mcomments
		methods << method
		// println('register method $name')
		return_type_sym := p.table.get_type_symbol(method.return_type)
		ts.register_method(table.Fn{
			name: name
			params: args
			return_type: method.return_type
			return_type_source_name: return_type_sym.source_name
			is_pub: true
		})
	}
	p.top_level_statement_end()
	p.check(.rcbr)
	return ast.InterfaceDecl{
		name: interface_name
		methods: methods
		is_pub: is_pub
		pos: start_pos
		pre_comments: pre_comments
	}
}
