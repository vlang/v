// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import (
	v.scanner
	v.ast
	v.token
	v.table
	v.types
	term
	os
)

pub fn (p mut Parser) call_expr() (ast.CallExpr,types.Type) {
	// println('got fn call')
	tok := p.tok
	fn_name := p.check_name()
	p.check(.lpar)
	mut is_unknown := false
	mut args := []ast.Expr
	if f := p.table.find_fn(fn_name) {
		for i, arg in f.args {
			e,typ := p.expr(0)
			if !types.check(arg.typ, typ) {
				p.error('cannot use type `$typ.name` as type `$arg.typ.name` in argument to `$fn_name`')
			}
			args << e
			if i < f.args.len - 1 {
				p.check(.comma)
			}
		}
		if p.tok.kind == .comma {
			p.error('too many arguments in call to `$fn_name`')
		}
	}else{
		is_unknown = true
		p.warn('unknown function `$fn_name`')
		for p.tok.kind != .rpar {
			e,_ := p.expr(0)
			args << e
			if p.tok.kind != .rpar {
				p.check(.comma)
			}
		}
	}
	p.check(.rpar)
	node := ast.CallExpr{
		name: fn_name
		args: args
		is_unknown: is_unknown
		tok: tok
	}
	if is_unknown {
		p.table.unknown_calls << node
	}
	return node,types.int_type
}

fn (p mut Parser) fn_decl() ast.FnDecl {
	p.table.clear_vars()
	p.check(.key_fn)
	name := p.check_name()
	// println('fn decl $name')
	p.check(.lpar)
	// Args
	mut args := []table.Var
	mut ast_args := []ast.Arg
	for p.tok.kind != .rpar {
		arg_name := p.check_name()
		typ := p.parse_type()
		args << table.Var{
			name: arg_name
			typ: typ
		}
		ast_args << ast.Arg{
			typ: typ
			name: arg_name
		}
		if p.tok.kind != .rpar {
			p.check(.comma)
		}
	}
	p.check(.rpar)
	// Return type
	mut typ := types.void_type
	if p.tok.kind == .name {
		typ = p.parse_type()
		p.return_type = typ
	}
	p.table.register_fn(table.Fn{
		name: name
		args: args
	})
	stmts := p.parse_block()
	return ast.FnDecl{
		name: name
		stmts: stmts
		typ: typ
		args: ast_args
	}
}

pub fn (p &Parser) check_fn_calls() {
	println('CHEKC FN CALLS')
	for call in p.table.unknown_calls {
		f := p.table.find_fn(call.name) or {
			p.error_at_line('unknown function `$call.name`', call.tok.line_nr)
			return
		}
		println(call.name)
	}
}
