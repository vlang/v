// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import (
	v.ast
	v.table
)

pub fn (p mut Parser) call_expr() (ast.CallExpr,table.Type) {
	tok := p.tok
	fn_name := p.check_name()
	p.check(.lpar)
	// mut return_ti := types.void_ti
	args := p.call_args()
	node := ast.CallExpr{
		name: fn_name
		args: args
		// tok: tok
		
		pos: tok.position()
	}
	if f := p.table.find_fn(fn_name) {
		return node,f.return_type
	}
	typ := p.add_unresolved(node)
	return node,typ
}

pub fn (p mut Parser) call_args() []ast.Expr {
	mut args := []ast.Expr
	for p.tok.kind != .rpar {
		if p.tok.kind == .key_mut {
			p.check(.key_mut)
		}
		e,_ := p.expr(0)
		args << e
		if p.tok.kind != .rpar {
			p.check(.comma)
		}
	}
	p.check(.rpar)
	return args // ,table.void_ti
}

fn (p mut Parser) fn_decl() ast.FnDecl {
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.table.clear_vars()
	p.check(.key_fn)
	// C.
	is_c := p.tok.kind == .name && p.tok.lit == 'C'
	if is_c {
		p.next()
		p.check(.dot)
	}
	// Receiver?
	mut rec_name := ''
	mut is_method := false
	mut rec_ti := table.void_type
	if p.tok.kind == .lpar {
		is_method = true
		p.next()
		rec_name = p.check_name()
		if p.tok.kind == .key_mut {
			p.next()
		}
		rec_ti = p.parse_type()
		p.table.register_var(table.Var{
			name: rec_name
			typ: rec_ti
		})
		p.check(.rpar)
	}
	mut name := ''
	if p.tok.kind == .name {
		// TODO high
		name = p.check_name()
	}
	// <T>
	if p.tok.kind == .lt {
		p.next()
		p.next()
		p.check(.gt)
	}
	// println('fn decl $name')
	p.check(.lpar)
	// Args
	mut args := []table.Var
	mut ast_args := []ast.Arg
	// `int, int, string` (no names, just types)
	types_only := p.tok.kind in [.amp] || (p.peek_tok.kind == .comma && p.table.known_type(p.tok.lit)) ||
	//
	p.peek_tok.kind == .rpar
	if types_only {
		p.warn('types only')
		for p.tok.kind != .rpar {
			p.parse_type()
			if p.tok.kind == .comma {
				p.next()
			}
		}
	}
	else {
		for p.tok.kind != .rpar {
			mut arg_names := [p.check_name()]
			// `a, b, c int`
			for p.tok.kind == .comma {
				p.check(.comma)
				arg_names << p.check_name()
			}
			if p.tok.kind == .key_mut {
				p.check(.key_mut)
			}
			ti := p.parse_type()
			for arg_name in arg_names {
				arg := table.Var{
					name: arg_name
					typ: ti
				}
				args << arg
				p.table.register_var(arg)
				ast_args << ast.Arg{
					ti: ti
					name: arg_name
				}
				if ti.kind == .variadic && p.tok.kind == .comma {
					p.error('cannot use ...(variadic) with non-final parameter $arg_name')
				}
			}
			if p.tok.kind != .rpar {
				p.check(.comma)
			}
		}
	}
	p.check(.rpar)
	// Return type
	mut typ := table.void_type
	if p.tok.kind in [.name, .lpar, .amp, .lsbr] {
		typ = p.parse_type()
		p.return_type = typ
	}
	else {
		p.return_type = table.void_type
	}
	if is_method {
		ok := p.table.register_method(rec_ti, table.Fn{
			name: name
			args: args
			return_type: typ
		})
		if !ok {
			p.error('expected Struct')
		}
	}
	else {
		p.table.register_fn(table.Fn{
			name: name
			args: args
			return_type: typ
		})
	}
	mut stmts := []ast.Stmt
	if p.tok.kind == .lcbr {
		stmts = p.parse_block()
	}
	return ast.FnDecl{
		name: name
		stmts: stmts
		ti: typ
		args: ast_args
		is_pub: is_pub
		receiver: ast.Field{
			name: rec_name
			typ: rec_ti
		}
	}
}

pub fn (p &Parser) check_fn_calls() {
	println('check fn calls2')
	/*
	for call in p.table.unknown_calls {
		f := p.table.find_fn(call.name) or {
			p.error_at_line('unknown function `$call.name`', call.tok.line_nr)
			return
		}
		println(f.name)
		// println(f.return_ti.name)
		// println('IN AST typ=' + call.typ.name)
	}
	*/

}
