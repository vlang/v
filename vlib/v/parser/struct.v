// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table
import v.token
import v.util

fn (mut p Parser) struct_decl() ast.StructDecl {
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
	is_typedef := p.attr == 'typedef'
	no_body := p.peek_tok.kind != .lcbr
	if language == .v && no_body {
		p.error('`$p.tok.lit` lacks body')
	}
	end_pos := p.tok.position()
	mut name := p.check_name()
	if language == .v && p.mod != 'builtin' && name.len > 0 && !name[0].is_capital() {
		p.error_with_pos('struct name `$name` must begin with capital letter', end_pos)
	}
	if name.len == 1 {
		p.error_with_pos('struct names must have more than one character', end_pos)
	}
	// println('struct decl $name')
	mut ast_fields := []ast.StructField{}
	mut fields := []table.Field{}
	mut mut_pos := -1
	mut pub_pos := -1
	mut pub_mut_pos := -1
	mut global_pos := -1
	mut is_field_mut := false
	mut is_field_pub := false
	mut is_field_global := false
	if !no_body {
		p.check(.lcbr)
		for p.tok.kind != .rcbr {
			mut comment := ast.Comment{}
			if p.tok.kind == .comment {
				comment = p.comment()
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
			field_start_pos := p.tok.position()
			field_name := p.check_name()
			// p.warn('field $field_name')
			typ := p.parse_type()
			field_pos := field_start_pos.extend(p.tok.position())
			/*
			if name == '_net_module_s' {
			s := p.table.get_type_symbol(typ)
			println('XXXX' + s.str())
		}
			*/
			mut attrs := []string{}
			if p.tok.kind == .lsbr {
				parsed_attrs := p.attributes()
				for attr in parsed_attrs {
					attrs << attr.name
				}
			}
			mut default_expr := ast.Expr{}
			mut has_default_expr := false
			if p.tok.kind == .assign {
				// Default value
				p.next()
				// default_expr = p.tok.lit
				// p.expr(0)
				default_expr = p.expr(0)
				match default_expr {
					ast.EnumVal { it.typ = typ }
					// TODO: implement all types??
					else {}
				}
				has_default_expr = true
			}
			if p.tok.kind == .comment {
				comment = p.comment()
			}
			// TODO merge table and ast Fields?
			ast_fields << ast.StructField{
				name: field_name
				pos: field_pos
				typ: typ
				comment: comment
				default_expr: default_expr
				has_default_expr: has_default_expr
				attrs: attrs
				is_public: is_field_pub
			}
			fields << table.Field{
				name: field_name
				typ: typ
				default_expr: ast.ex2fe(default_expr)
				has_default_expr: has_default_expr
				is_pub: is_field_pub
				is_mut: is_field_mut
				is_global: is_field_global
				attrs: attrs
			}
			// println('struct field $ti.name $field_name')
		}
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
		info: table.Struct{
			fields: fields
			is_typedef: is_typedef
			is_union: is_union
			is_ref_only: p.attr == 'ref_only'
		}
		mod: p.mod
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
		p.error('cannot register type `$name`, another type with this name exists')
	}
	p.expr_mod = ''
	return ast.StructDecl{
		name: name
		is_pub: is_pub
		fields: ast_fields
		pos: start_pos.extend(end_pos)
		mut_pos: mut_pos
		pub_pos: pub_pos
		pub_mut_pos: pub_mut_pos
		language: language
		is_union: is_union
		attr: p.attr
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
	mut fields := []ast.StructInitField{}
	mut i := 0
	no_keys := p.peek_tok.kind != .colon && p.tok.kind != .rcbr // `Vec{a,b,c}
	// p.warn(is_short_syntax.str())
	saved_is_amp := p.is_amp
	p.is_amp = false
	for p.tok.kind != .rcbr && p.tok.kind != .rpar {
		p.check_comment()
		mut field_name := ''
		if no_keys {
			expr := p.expr(0)
			// name will be set later in checker
			fields << ast.StructInitField{
				expr: expr
				pos: expr.position()
			}
		} else {
			first_field_pos := p.tok.position()
			field_name = p.check_name()
			p.check(.colon)
			expr := p.expr(0)
			last_field_pos := expr.position()
			field_pos := token.Position{
				line_nr: first_field_pos.line_nr
				pos: first_field_pos.pos
				len: last_field_pos.pos - first_field_pos.pos + last_field_pos.len
			}
			fields << ast.StructInitField{
				name: field_name
				expr: expr
				pos: field_pos
			}
		}
		i++
		if p.tok.kind == .comma {
			p.next()
		}
		p.check_comment()
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
	}
	return node
}

fn (mut p Parser) interface_decl() ast.InterfaceDecl {
	start_pos := p.tok.position()
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.next() // `interface`
	interface_name := p.prepend_mod(p.check_name())
	// println('interface decl $interface_name')
	p.check(.lcbr)
	// Declare the type
	t := table.TypeSymbol{
		kind: .interface_
		name: interface_name
		mod: p.mod
		info: table.Interface{
			types: []
		}
	}
	typ := table.new_type(p.table.register_type_symbol(t))
	ts := p.table.get_type_symbol(typ) // TODO t vs ts
	// Parse methods
	mut methods := []ast.FnDecl{}
	for p.tok.kind != .rcbr && p.tok.kind != .eof {
		method_start_pos := p.tok.position()
		line_nr := p.tok.line_nr
		name := p.check_name()
		if util.contains_capital(name) {
			p.error('interface methods cannot contain uppercase letters, use snake_case instead')
		}
		// field_names << name
		args2, _ := p.fn_args()
		mut args := [table.Arg{
			name: 'x'
			typ: typ
			is_hidden: true
		}]
		args << args2
		mut method := ast.FnDecl{
			name: name
			args: args
			file: p.file_name
			return_type: table.void_type
			is_pub: true
			pos: method_start_pos.extend(p.prev_tok.position())
		}
		if p.tok.kind.is_start_of_type() && p.tok.line_nr == line_nr {
			method.return_type = p.parse_type()
		}
		methods << method
		// println('register method $name')
		ts.register_method(table.Fn{
			name: name
			args: args
			return_type: method.return_type
			is_pub: true
		})
	}
	p.check(.rcbr)
	return ast.InterfaceDecl{
		name: interface_name
		methods: methods
		pos: start_pos
	}
}
