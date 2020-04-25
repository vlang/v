// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table
import v.token

fn (mut p Parser) struct_decl() ast.StructDecl {
	start_pos := p.tok.position()
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	is_union := p.tok.kind == .key_union
	if p.tok.kind == .key_struct {
		p.check(.key_struct)
	} else {
		p.check(.key_union)
	}
	is_c := p.tok.lit == 'C' && p.peek_tok.kind == .dot
	is_js := p.tok.lit == 'JS' && p.peek_tok.kind == .dot
	if is_c {
		p.next() // C || JS
		p.next() // .
	}
	is_typedef := p.attr == 'typedef'
	no_body := p.peek_tok.kind != .lcbr
	if !is_c && !is_js && no_body {
		p.error('`$p.tok.lit` lacks body')
	}
	end_pos := p.tok.position()
	mut name := p.check_name()
	// println('struct decl $name')
	mut ast_fields := []ast.StructField
	mut fields := []table.Field
	mut mut_pos := -1
	mut pub_pos := -1
	mut pub_mut_pos := -1
	if !no_body {
		p.check(.lcbr)
		for p.tok.kind != .rcbr {
			mut comment := ast.Comment{}
			if p.tok.kind == .comment {
				comment = p.comment()
			}
			if p.tok.kind == .key_pub {
				p.check(.key_pub)
				if p.tok.kind == .key_mut {
					p.check(.key_mut)
					pub_mut_pos = fields.len
				} else {
					pub_pos = fields.len
				}
				p.check(.colon)
			} else if p.tok.kind == .key_mut {
				p.check(.key_mut)
				p.check(.colon)
				mut_pos = fields.len
			} else if p.tok.kind == .key_global {
				p.check(.key_global)
				p.check(.colon)
			}
			field_name := p.check_name()
			field_pos := p.tok.position()
			// p.warn('field $field_name')
			typ := p.parse_type()
			/*
			if name == '_net_module_s' {
			s := p.table.get_type_symbol(typ)
			println('XXXX' + s.str())
		}
			*/
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
			mut attr := ast.Attr{}
			if p.tok.kind == .lsbr {
				attr = p.attribute()
			}
			if p.tok.kind == .comment {
				comment = p.comment()
			}
			ast_fields << ast.StructField{
				name: field_name
				pos: field_pos
				typ: typ
				comment: comment
				default_expr: default_expr
				has_default_expr: has_default_expr
				attr: attr.name
			}
			fields << table.Field{
				name: field_name
				typ: typ
				default_expr: default_expr
				has_default_expr: has_default_expr
			}
			// println('struct field $ti.name $field_name')
		}
		p.check(.rcbr)
	}
	if is_c {
		name = 'C.$name'
	} else if is_js {
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
		}
		mod: p.mod
	}
	mut ret := 0
	if p.builtin_mod && t.name in table.builtin_type_names {
		// this allows overiding the builtins type
		// with the real struct type info parsed from builtin
		ret = p.table.register_builtin_type_symbol(t)
	} else {
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
		is_c: is_c
		is_js: is_js
		is_union: is_union
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
	mut fields := []ast.StructInitField
	mut i := 0
	is_short_syntax := p.peek_tok.kind != .colon && p.tok.kind != .rcbr // `Vec{a,b,c}
	// p.warn(is_short_syntax.str())
	for p.tok.kind != .rcbr {
		p.check_comment()
		mut field_name := ''
		if is_short_syntax {
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
			p.check(.comma)
		}
		p.check_comment()
	}
	last_pos := p.tok.position()
	if !short_syntax {
		p.check(.rcbr)
	}
	node := ast.StructInit{
		typ: typ
		fields: fields
		pos: token.Position{
			line_nr: first_pos.line_nr
			pos: first_pos.pos
			len: last_pos.pos - first_pos.pos + last_pos.len
		}
		is_short: is_short_syntax
	}
	return node
}

fn (mut p Parser) interface_decl() ast.InterfaceDecl {
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.next() // `interface`
	interface_name := p.check_name()
	// println('interface decl $interface_name')
	p.check(.lcbr)
	// Declare the type
	t := table.TypeSymbol{
		kind: .interface_
		name: interface_name
		info: table.Interface{
			gen_types: []
			foo: 'foo'
		}
	}
	typ := table.new_type(p.table.register_type_symbol(t))
	ts := p.table.get_type_symbol(typ) // TODO t vs ts
	// Parse methods
	mut methods := []ast.FnDecl
	for p.tok.kind != .rcbr && p.tok.kind != .eof {
		line_nr := p.tok.line_nr
		name := p.check_name()
		println(name)
		// field_names << name
		args2, _ := p.fn_args()
		mut args := [table.Arg{
			name: 'x'
			typ: typ
		}]
		args << args2
		mut method := ast.FnDecl{
			name: name
			args: args
			return_type: table.void_type
		}
		if p.tok.kind == .name && p.tok.line_nr == line_nr {
			method.return_type = p.parse_type()
		}
		methods << method
		// println('register method $name')
		ts.register_method(table.Fn{
			name: name
			args: args
			return_type: method.return_type
		})
	}
	p.check(.rcbr)
	return ast.InterfaceDecl{
		name: interface_name
		methods: methods
	}
}
