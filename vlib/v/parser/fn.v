// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table
import v.scanner
import v.token

pub fn (var p Parser) call_expr(is_c bool, mod string) ast.CallExpr {
	first_pos := p.tok.position()
	tok := p.tok
	name := p.check_name()
	fn_name := if is_c {
		'C.$name'
	} else if mod.len > 0 {
		'${mod}.$name'
	} else {
		name
	}
	p.check(.lpar)
	args := p.call_args()
	last_pos := p.tok.position()
	p.check(.rpar)
	pos := token.Position{
		line_nr: first_pos.line_nr
		pos: first_pos.pos
		len: last_pos.pos - first_pos.pos + last_pos.len
	}
	var or_stmts := []ast.Stmt
	var is_or_block_used := false
	if p.tok.kind == .key_orelse {
		p.next()
		p.open_scope()
		p.scope.register('err', ast.Var{
			name: 'err'
			typ: table.string_type
		})
		p.scope.register('errcode', ast.Var{
			name: 'errcode'
			typ: table.int_type
		})
		is_or_block_used = true
		or_stmts = p.parse_block_no_scope()
		p.close_scope()
	}
	node := ast.CallExpr{
		name: fn_name
		args: args
		mod: p.mod
		pos: pos
		is_c: is_c
		or_block: ast.OrExpr{
			stmts: or_stmts
			is_used: is_or_block_used
		}
	}
	return node
}

pub fn (var p Parser) call_args() []ast.CallArg {
	var args := []ast.CallArg
	for p.tok.kind != .rpar {
		var is_mut := false
		if p.tok.kind == .key_mut {
			p.check(.key_mut)
			is_mut = true
		}
		e := p.expr(0)
		args << ast.CallArg{
			is_mut: is_mut
			expr: e
		}
		if p.tok.kind != .rpar {
			p.check(.comma)
		}
	}
	return args
}

fn (var p Parser) fn_decl() ast.FnDecl {
	// p.table.clear_vars()
	pos := p.tok.position()
	p.open_scope()
	is_deprecated := p.attr == 'deprecated'
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
	var rec_name := ''
	var is_method := false
	var rec_type := table.void_type
	var rec_mut := false
	var args := []table.Arg
	if p.tok.kind == .lpar {
		p.next()		// (
		is_method = true
		rec_mut = p.tok.kind == .key_var
		if rec_mut {
			p.next()			// `var`
		}
		rec_name = p.check_name()
		if !rec_mut {
			rec_mut = p.tok.kind == .key_mut
		}
		is_amp := p.peek_tok.kind == .amp
		// if rec_mut {
		// p.check(.key_mut)
		// }
		// TODO: talk to alex, should mut be parsed with the type like this?
		// or should it be a property of the arg, like this ptr/mut becomes indistinguishable
		rec_type = p.parse_type_with_mut(rec_mut)
		if is_amp && rec_mut {
			p.error('use `(f mut Foo)` or `(f &Foo)` instead of `(f mut &Foo)`')
		}
		args << table.Arg{
			name: rec_name
			is_mut: rec_mut
			typ: rec_type
		}
		p.check(.rpar)
	}
	var name := ''
	if p.tok.kind == .name {
		// TODO high order fn
		name = p.check_name()
		if !is_c && !p.pref.translated && scanner.contains_capital(name) {
			p.error('function names cannot contain uppercase letters, use snake_case instead')
		}
		if is_method && p.table.get_type_symbol(rec_type).has_method(name) {
			p.error('duplicate method `$name`')
		}
	}
	if p.tok.kind in [.plus, .minus, .mul, .div, .mod] {
		name = p.tok.kind.str()		// op_to_fn_name()
		p.next()
	}
	// <T>
	is_generic := p.tok.kind == .lt
	if is_generic {
		p.next()
		p.next()
		p.check(.gt)
	}
	// Args
	args2, is_variadic := p.fn_args()
	args << args2
	for arg in args {
		p.scope.register(arg.name, ast.Var{
			name: arg.name
			typ: arg.typ
		})
	}
	// Return type
	var return_type := table.void_type
	if p.tok.kind.is_start_of_type() {
		return_type = p.parse_type()
	}
	// Register
	if is_method {
		var type_sym := p.table.get_type_symbol(rec_type)
		// p.warn('reg method $type_sym.name . $name ()')
		type_sym.register_method(table.Fn{
			name: name
			args: args
			return_type: return_type
			is_variadic: is_variadic
			is_generic: is_generic
		})
	} else {
		if is_c {
			name = 'C.$name'
		} else {
			name = p.prepend_mod(name)
		}
		if _ := p.table.find_fn(name) {
			p.error('redefinition of `$name`')
		}
		p.table.register_fn(table.Fn{
			name: name
			args: args
			return_type: return_type
			is_variadic: is_variadic
			is_c: is_c
			is_generic: is_generic
		})
	}
	// Body
	var stmts := []ast.Stmt
	no_body := p.tok.kind != .lcbr
	if p.tok.kind == .lcbr {
		stmts = p.parse_block()
	}
	p.close_scope()
	p.attr = ''
	return ast.FnDecl{
		name: name
		stmts: stmts
		return_type: return_type
		args: args
		is_deprecated: is_deprecated
		is_pub: is_pub
		is_variadic: is_variadic
		receiver: ast.Field{
			name: rec_name
			typ: rec_type
		}
		is_method: is_method
		rec_mut: rec_mut
		is_c: is_c
		no_body: no_body
		pos: pos
		is_builtin: p.builtin_mod || p.mod in ['math', 'strconv', 'strconv.ftoa', 'hash.wyhash',
			'math.bits', 'strings']
	}
}

fn (var p Parser) fn_args() ([]table.Arg, bool) {
	p.check(.lpar)
	var args := []table.Arg
	var is_variadic := false
	// `int, int, string` (no names, just types)
	types_only := p.tok.kind in [.amp, .and] || (p.peek_tok.kind == .comma && p.table.known_type(p.tok.lit)) ||
		p.peek_tok.kind == .rpar
	if types_only {
		// p.warn('types only')
		var arg_no := 1
		for p.tok.kind != .rpar {
			arg_name := 'arg_$arg_no'
			is_mut := p.tok.kind == .key_mut
			if is_mut {
				p.check(.key_mut)
			}
			if p.tok.kind == .ellipsis {
				p.check(.ellipsis)
				is_variadic = true
			}
			var arg_type := p.parse_type()
			if is_variadic {
				arg_type = table.type_set(arg_type, .variadic)
			}
			if p.tok.kind == .comma {
				if is_variadic {
					p.error('cannot use ...(variadic) with non-final parameter no $arg_no')
				}
				p.next()
			}
			args << table.Arg{
				name: arg_name
				is_mut: is_mut
				typ: arg_type
			}
			arg_no++
		}
	} else {
		for p.tok.kind != .rpar {
			var arg_names := [p.check_name()]
			// `a, b, c int`
			for p.tok.kind == .comma {
				p.check(.comma)
				arg_names << p.check_name()
			}
			is_mut := p.tok.kind == .key_mut
			// if is_mut {
			// p.check(.key_mut)
			// }
			if p.tok.kind == .ellipsis {
				p.check(.ellipsis)
				is_variadic = true
			}
			var typ := p.parse_type()
			if is_variadic {
				typ = table.type_set(typ, .variadic)
			}
			for arg_name in arg_names {
				args << table.Arg{
					name: arg_name
					is_mut: is_mut
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
	return args, is_variadic
}

fn (p Parser) fileis(s string) bool {
	return p.file_name.contains(s)
}
