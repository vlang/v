// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import (
	v.ast
	v.table
)

pub fn (p mut Parser) call_expr(is_c bool, mod string) ast.CallExpr {
	tok := p.tok
	name := p.check_name()
	fn_name := if is_c { 'C.$name' } else if mod.len > 0 { '${mod}.$name' } else { name }
	p.check(.lpar)
	args,muts := p.call_args()
	mut or_stmts := []ast.Stmt
	if p.tok.kind == .key_orelse {
		p.next()
		or_stmts = p.parse_block()
	}
	node := ast.CallExpr{
		name: fn_name
		args: args
		muts: muts
		// tok: tok
		
		pos: tok.position()
		is_c: is_c
		or_block: ast.OrExpr{
			stmts: or_stmts
		}
	}
	return node
}

pub fn (p mut Parser) call_args() ([]ast.Expr,[]bool) {
	mut args := []ast.Expr
	mut muts := []bool
	for p.tok.kind != .rpar {
		if p.tok.kind == .key_mut {
			p.check(.key_mut)
			muts << true
		}
		else {
			muts << false
		}
		e,_ := p.expr(0)
		args << e
		if p.tok.kind != .rpar {
			p.check(.comma)
		}
	}
	p.check(.rpar)
	return args,muts
}

fn (p mut Parser) fn_decl() ast.FnDecl {
	// p.table.clear_vars()
	p.open_scope()
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
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
	mut rec_type := table.void_type
	mut rec_mut := false
	if p.tok.kind == .lpar {
		is_method = true
		p.next()
		rec_name = p.check_name()
		if p.tok.kind == .key_mut {
			p.next()
			rec_mut = true
		}
		rec_type = p.parse_type()
		// p.table.register_var(table.Var{
		// name: rec_name
		// typ: rec_type
		// })
		p.scope.register_var(ast.VarDecl{
			name: rec_name
			typ: rec_type
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
	// Args
	mut args := []table.Var
	ast_args,is_variadic := p.fn_args()
	for ast_arg in ast_args {
		var := table.Var{
			name: ast_arg.name
			typ: ast_arg.typ
		}
		args << var
		p.scope.register_var(ast.VarDecl{
			name: ast_arg.name
			typ: ast_arg.typ
		})
		// p.table.register_var(var)
	}
	//
	/*

			arg := table.Var{
				name: arg_name
				typ: arg_type
			}
			args << arg
			// p.table.register_var(arg)
				arg := table.Var{
					name: arg_name
					typ: typ
				}
				args << arg
				p.table.register_var(arg)
				*/
	// Return type
	mut typ := table.void_type
	if p.tok.kind.is_start_of_type() {
		typ = p.parse_type()
	}
	p.return_type = typ
	// Register
	if is_method {
		type_sym := p.table.get_type_symbol(rec_type)
		// p.warn('reg method $type_sym.name . $name ()')
		ok := p.table.register_method(type_sym, table.Fn{
			name: name
			args: args
			return_type: typ
		})
		if !ok {
			p.error('expected Struct')
		}
	}
	else {
		if is_c {
			name = 'C.$name'
		}
		else {
			name = p.prepend_mod(name)
		}
		p.table.register_fn(table.Fn{
			name: name
			args: args
			return_type: typ
			is_variadic: is_variadic
			is_c: is_c
		})
	}
	mut stmts := []ast.Stmt
	if p.tok.kind == .lcbr {
		stmts = p.parse_block()
	}
	p.close_scope()
	return ast.FnDecl{
		name: name
		stmts: stmts
		typ: typ
		args: ast_args
		is_pub: is_pub
		is_variadic: is_variadic
		receiver: ast.Field{
			name: rec_name
			typ: rec_type
		}
		is_method: is_method
		rec_mut: rec_mut
		is_c: is_c
	}
}

fn (p mut Parser) fn_args() ([]ast.Arg,bool) {
	p.check(.lpar)
	mut args := []ast.Arg
	mut is_variadic := false
	// `int, int, string` (no names, just types)
	types_only := p.tok.kind in [.amp] || (p.peek_tok.kind == .comma && p.table.known_type(p.tok.lit)) || p.peek_tok.kind == .rpar
	if types_only {
		// p.warn('types only')
		mut arg_no := 1
		for p.tok.kind != .rpar {
			arg_name := 'arg_$arg_no'
			if p.tok.kind == .ellipsis {
				p.check(.ellipsis)
				is_variadic = true
			}
			mut arg_type := p.parse_type()
			if is_variadic {
				arg_type = table.type_to_variadic(arg_type)
			}
			if p.tok.kind == .comma {
				if is_variadic {
					p.error('cannot use ...(variadic) with non-final parameter no $arg_no')
				}
				p.next()
			}
			args << ast.Arg{
				name: arg_name
				typ: arg_type
			}
		}
		arg_no++
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
			if p.tok.kind == .ellipsis {
				p.check(.ellipsis)
				is_variadic = true
			}
			mut typ := p.parse_type()
			if is_variadic {
				typ = table.type_to_variadic(typ)
			}
			for arg_name in arg_names {
				args << ast.Arg{
					name: arg_name
					typ: typ
				}
				// if typ.typ.kind == .variadic && p.tok.kind == .comma {
				if is_variadic && p.tok.kind == .comma {
					p.error('cannot use ...(variadic) with non-final parameter $arg_name')
				}
			}
			if p.tok.kind != .rpar {
				p.check(.comma)
			}
		}
	}
	p.check(.rpar)
	return args,is_variadic
}
