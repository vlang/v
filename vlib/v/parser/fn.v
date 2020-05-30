// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table
import v.token
import v.util

pub fn (mut p Parser) call_expr(language table.Language, mod string) ast.CallExpr {
	first_pos := p.tok.position()
	fn_name := if language == .c {
		'C.${p.check_name()}'
	} else if language == .js {
		'JS.${p.check_js_name()}'
	} else if mod.len > 0 {
		'${mod}.${p.check_name()}'
	} else {
		p.check_name()
	}
	mut or_kind := ast.OrKind.absent
	if fn_name == 'json.decode' {
		p.expecting_type = true // Makes name_expr() parse the type `User` in `json.decode(User, txt)`
		p.expr_mod = ''
		or_kind = .block
	}
	mut generic_type := table.void_type
	if p.tok.kind == .lt {
		// `foo<int>(10)`
		p.next() // `<`
		p.expr_mod = ''
		mut generic_type = p.parse_type()
		p.check(.gt) // `>`
		// In case of `foo<T>()`
		// T is unwrapped and registered in the checker.
		if generic_type != table.t_type {
			p.table.register_fn_gen_type(fn_name, generic_type)
		}
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
	mut or_stmts := []ast.Stmt{}
	if p.tok.kind == .key_orelse {
		// `foo() or {}``
		was_inside_or_expr := p.inside_or_expr
		p.inside_or_expr = true
		p.next()
		p.open_scope()
		p.scope.register('err', ast.Var{
			name: 'err'
			typ: table.string_type
			pos: p.tok.position()
			is_used: true
		})
		p.scope.register('errcode', ast.Var{
			name: 'errcode'
			typ: table.int_type
			pos: p.tok.position()
			is_used: true
		})
		or_kind = .block
		or_stmts = p.parse_block_no_scope()
		p.close_scope()
		p.inside_or_expr = was_inside_or_expr
	}
	if p.tok.kind == .question {
		// `foo()?`
		p.next()
		or_kind = .propagate
	}
	node := ast.CallExpr{
		name: fn_name
		args: args
		mod: p.mod
		pos: pos
		language: language
		or_block: ast.OrExpr{
			stmts: or_stmts
			kind: or_kind
			pos: pos
		}
		generic_type: generic_type
	}
	return node
}

pub fn (mut p Parser) call_args() []ast.CallArg {
	mut args := []ast.CallArg{}
	for p.tok.kind != .rpar {
		mut is_mut := false
		if p.tok.kind == .key_mut {
			p.next()
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

fn (mut p Parser) fn_decl() ast.FnDecl {
	start_pos := p.tok.position()
	is_deprecated := p.attr == 'deprecated'
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.check(.key_fn)
	p.open_scope()
	// C. || JS.
	language := if p.tok.kind == .name && p.tok.lit == 'C' {
		table.Language.c
	} else if p.tok.kind == .name && p.tok.lit == 'JS' {
		table.Language.js
	} else {
		table.Language.v
	}
	if language != .v {
		p.next()
		p.check(.dot)
	}
	// Receiver?
	mut rec_name := ''
	mut is_method := false
	mut receiver_pos := token.Position{}
	mut rec_type := table.void_type
	mut rec_mut := false
	mut args := []table.Arg{}
	if p.tok.kind == .lpar {
		p.next() // (
		is_method = true
		rec_mut = p.tok.kind == .key_mut
		if rec_mut {
			p.next() // `mut`
		}
		rec_start_pos := p.tok.position()
		rec_name = p.check_name()
		if !rec_mut {
			rec_mut = p.tok.kind == .key_mut
			if rec_mut {
				p.warn_with_pos('use `(mut f Foo)` instead of `(f mut Foo)`', p.tok.position())
			}
		}
		receiver_pos = rec_start_pos.extend(p.tok.position())
		is_amp := p.tok.kind == .amp
		// if rec_mut {
		// p.check(.key_mut)
		// }
		// TODO: talk to alex, should mut be parsed with the type like this?
		// or should it be a property of the arg, like this ptr/mut becomes indistinguishable
		rec_type = p.parse_type_with_mut(rec_mut)
		if is_amp && rec_mut {
			p.error('use `(mut f Foo)` or `(f &Foo)` instead of `(mut f &Foo)`')
		}
		args << table.Arg{
			name: rec_name
			is_mut: rec_mut
			typ: rec_type
		}
		p.check(.rpar)
	}
	mut name := ''
	if p.tok.kind == .name {
		// TODO high order fn
		name = if language == .js {
			p.check_js_name()
		} else {
			p.check_name()
		}
		if language == .v && !p.pref.translated && util.contains_capital(name) {
			p.error('function names cannot contain uppercase letters, use snake_case instead')
		}
		if is_method && p.table.get_type_symbol(rec_type).has_method(name) {
			p.error('duplicate method `$name`')
		}
	}
	if p.tok.kind in [.plus, .minus, .mul, .div, .mod] {
		name = p.tok.kind.str() // op_to_fn_name()
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
	for i, arg in args {
		if p.scope.known_var(arg.name) {
			p.error('redefinition of parameter `$arg.name`')
		}
		p.scope.register(arg.name, ast.Var{
			name: arg.name
			typ: arg.typ
			is_mut: arg.is_mut
			pos: p.tok.position()
			is_used: true
			is_arg: true
		})
		// Do not allow `mut` with simple types
		// TODO move to checker?
		if arg.is_mut {
			if i == 0 && is_method {
				continue
			}
			sym := p.table.get_type_symbol(arg.typ)
			if sym.kind !in [.array, .struct_, .map, .placeholder] && !arg.typ.is_ptr() {
				p.error('mutable arguments are only allowed for arrays, maps, and structs\n' +
					'return values instead: `fn foo(n mut int) {` => `fn foo(n int) int {`')
			}
		}
	}
	mut end_pos := p.prev_tok.position()
	// Return type
	mut return_type := table.void_type
	if p.tok.kind.is_start_of_type() {
		end_pos = p.tok.position()
		return_type = p.parse_type()
	}
	ctdefine := p.attr_ctdefine
	// Register
	if is_method {
		mut type_sym := p.table.get_type_symbol(rec_type)
		// p.warn('reg method $type_sym.name . $name ()')
		type_sym.register_method(table.Fn{
			name: name
			args: args
			return_type: return_type
			is_variadic: is_variadic
			is_generic: is_generic
			is_pub: is_pub
			ctdefine: ctdefine
		})
	} else {
		if language == .c {
			name = 'C.$name'
		} else if language == .js {
			name = 'JS.$name'
		} else {
			name = p.prepend_mod(name)
		}
		if _ := p.table.find_fn(name) {
			p.fn_redefinition_error(name)
		}
		p.table.register_fn(table.Fn{
			name: name
			args: args
			return_type: return_type
			is_variadic: is_variadic
			language: language
			is_generic: is_generic
			is_pub: is_pub
			ctdefine: ctdefine
			mod: p.mod
		})
	}
	// Body
	mut stmts := []ast.Stmt{}
	no_body := p.tok.kind != .lcbr
	body_start_pos := p.peek_tok.position()
	if p.tok.kind == .lcbr {
		stmts = p.parse_block_no_scope()
	}
	p.close_scope()
	p.attr = ''
	p.attr_ctdefine = ''
	return ast.FnDecl{
		name: name
		stmts: stmts
		return_type: return_type
		args: args
		is_deprecated: is_deprecated
		is_pub: is_pub
		is_generic: is_generic
		is_variadic: is_variadic
		receiver: ast.Field{
			name: rec_name
			typ: rec_type
		}
		receiver_pos: receiver_pos
		is_method: is_method
		rec_mut: rec_mut
		language: language
		no_body: no_body
		pos: start_pos.extend(end_pos)
		body_pos: body_start_pos
		file: p.file_name
		is_builtin: p.builtin_mod || p.mod in util.builtin_module_parts
		ctdefine: ctdefine
	}
}

fn (mut p Parser) anon_fn() ast.AnonFn {
	pos := p.tok.position()
	p.check(.key_fn)
	p.open_scope()
	// TODO generics
	args, is_variadic := p.fn_args()
	for arg in args {
		p.scope.register(arg.name, ast.Var{
			name: arg.name
			typ: arg.typ
			pos: p.tok.position()
			is_used: true
			is_arg: true
		})
	}
	mut return_type := table.void_type
	if p.tok.kind.is_start_of_type() {
		return_type = p.parse_type()
	}
	mut stmts := []ast.Stmt{}
	no_body := p.tok.kind != .lcbr
	if p.tok.kind == .lcbr {
		stmts = p.parse_block_no_scope()
	}
	p.close_scope()
	mut func := table.Fn{
		args: args
		is_variadic: is_variadic
		return_type: return_type
	}
	name := 'anon_${p.tok.pos}_$func.signature()'
	func.name = name
	idx := p.table.find_or_register_fn_type(p.mod, func, true, false)
	typ := table.new_type(idx)
	// name := p.table.get_type_name(typ)
	return ast.AnonFn{
		decl: ast.FnDecl{
			name: name
			stmts: stmts
			return_type: return_type
			args: args
			is_variadic: is_variadic
			is_method: false
			is_anon: true
			no_body: no_body
			pos: pos
			file: p.file_name
		}
		typ: typ
	}
}

fn (mut p Parser) fn_args() ([]table.Arg, bool) {
	p.check(.lpar)
	mut args := []table.Arg{}
	mut is_variadic := false
	// `int, int, string` (no names, just types)
	types_only := p.tok.kind in [.amp, .and] || (p.peek_tok.kind == .comma && p.table.known_type(p.tok.lit)) ||
		p.peek_tok.kind == .rpar
	// TODO copy pasta, merge 2 branches
	if types_only {
		// p.warn('types only')
		mut arg_no := 1
		for p.tok.kind != .rpar {
			arg_name := 'arg_$arg_no'
			is_mut := p.tok.kind == .key_mut
			if is_mut {
				p.next()
			}
			if p.tok.kind == .ellipsis {
				p.next()
				is_variadic = true
			}
			mut arg_type := p.parse_type()
			if is_mut {
				// if arg_type.is_ptr() {
				// p.error('cannot mut')
				// }
				// arg_type = arg_type.to_ptr()
				arg_type = arg_type.set_nr_muls(1)
			}
			if is_variadic {
				arg_type = arg_type.set_flag(.variadic)
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
			mut is_mut := p.tok.kind == .key_mut
			if is_mut {
				p.next()
			}
			mut arg_names := [p.check_name()]
			// `a, b, c int`
			for p.tok.kind == .comma {
				p.next()
				arg_names << p.check_name()
			}
			if p.tok.kind == .key_mut {
				// TODO remove old syntax
				is_mut = true
			}
			if p.tok.kind == .ellipsis {
				p.next()
				is_variadic = true
			}
			mut typ := p.parse_type()
			if is_mut {
				if typ.is_ptr() {
					// name := p.table.get_type_name(typ)
					// p.warn('`$name` is already a reference, it cannot be marked as `mut`')
				}
				typ = typ.set_nr_muls(1)
			}
			if is_variadic {
				typ = typ.set_flag(.variadic)
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

fn (p &Parser) fileis(s string) bool {
	return p.file_name.contains(s)
}

fn (mut p Parser) fn_redefinition_error(name string) {
	// Find where this function was already declared
	// TODO
	/*
	for file in p.ast_files {

	}
	*/
	p.table.redefined_fns << name
	// p.error('redefinition of function `$name`')
}

fn have_fn_main(stmts []ast.Stmt) bool {
	mut has_main_fn := false
	for stmt in stmts {
		match stmt {
			ast.FnDecl {
				if it.name == 'main' {
					has_main_fn = true
				}
			}
			else {}
		}
	}
	return has_main_fn
}
