// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table
import v.token
import v.util

pub fn (mut p Parser) call_expr(language table.Language, mod string) ast.CallExpr {
	// pub fn (mut p Parser) call_expr(language table.Language, mod string) ast.Expr {
	first_pos := p.tok.position()
	mut fn_name := if language == .c {
		'C.$p.check_name()'
	} else if language == .js {
		'JS.$p.check_js_name()'
	} else if mod.len > 0 {
		'${mod}.$p.check_name()'
	} else {
		p.check_name()
	}
	if language != .v {
		p.check_for_impure_v(language, first_pos)
	}
	mut or_kind := ast.OrKind.absent
	if fn_name == 'json.decode' {
		p.expecting_type = true // Makes name_expr() parse the type `User` in `json.decode(User, txt)`
		p.expr_mod = ''
		or_kind = .block
	}
	mut generic_type := table.void_type
	mut generic_list_pos := p.tok.position()
	if p.tok.kind == .lt {
		// `foo<int>(10)`
		p.next() // `<`
		p.expr_mod = ''
		generic_type = p.parse_type()
		p.check(.gt) // `>`
		generic_list_pos = generic_list_pos.extend(p.prev_tok.position())
		// In case of `foo<T>()`
		// T is unwrapped and registered in the checker.
		if !generic_type.has_flag(.generic) {
			full_generic_fn_name := if fn_name.contains('.') {
				fn_name
			} else {
				p.prepend_mod(fn_name)
			}
			p.table.register_fn_gen_type(full_generic_fn_name, generic_type)
		}
	}
	p.check(.lpar)
	args := p.call_args()
	last_pos := p.tok.position()
	p.check(.rpar)
	// ! in mutable methods
	if p.tok.kind == .not {
		p.next()
	}
	pos := first_pos.extend(last_pos)
	mut or_stmts := []ast.Stmt{} // TODO remove unnecessary allocations by just using .absent
	mut or_pos := p.tok.position()
	if p.tok.kind == .key_orelse {
		// `foo() or {}``
		was_inside_or_expr := p.inside_or_expr
		p.inside_or_expr = true
		p.next()
		p.open_scope()
		p.scope.register(ast.Var{
			name: 'err'
			typ: table.string_type
			pos: p.tok.position()
			is_used: true
		})
		p.scope.register(ast.Var{
			name: 'errcode'
			typ: table.int_type
			pos: p.tok.position()
			is_used: true
		})
		or_kind = .block
		or_stmts = p.parse_block_no_scope(false)
		or_pos = or_pos.extend(p.prev_tok.position())
		p.close_scope()
		p.inside_or_expr = was_inside_or_expr
	}
	if p.tok.kind == .question {
		// `foo()?`
		p.next()
		or_kind = .propagate
	}
	if fn_name in p.imported_symbols {
		fn_name = p.imported_symbols[fn_name]
	}
	comments := p.eat_line_end_comments()
	return ast.CallExpr{
		name: fn_name
		args: args
		mod: p.mod
		pos: pos
		language: language
		generic_type: generic_type
		generic_list_pos: generic_list_pos
		or_block: ast.OrExpr{
			stmts: or_stmts
			kind: or_kind
			pos: or_pos
		}
		scope: p.scope
		comments: comments
	}
}

pub fn (mut p Parser) call_args() []ast.CallArg {
	mut args := []ast.CallArg{}
	start_pos := p.tok.position()
	for p.tok.kind != .rpar {
		if p.tok.kind == .eof {
			p.error_with_pos('unexpected eof reached, while parsing call argument', start_pos)
			return []
		}
		is_shared := p.tok.kind == .key_shared
		is_atomic := p.tok.kind == .key_atomic
		is_mut := p.tok.kind == .key_mut || is_shared || is_atomic
		if is_mut {
			p.next()
		}
		mut comments := p.eat_comments()
		arg_start_pos := p.tok.position()
		mut array_decompose := false
		if p.tok.kind == .ellipsis {
			p.next()
			array_decompose = true
		}
		mut e := p.expr(0)
		if array_decompose {
			e = ast.ArrayDecompose{
				expr: e
				pos: p.tok.position()
			}
		}
		if mut e is ast.StructInit {
			e.pre_comments << comments
			comments = []ast.Comment{}
		}
		pos := arg_start_pos.extend(p.prev_tok.position())
		comments << p.eat_comments()
		args << ast.CallArg{
			is_mut: is_mut
			share: table.sharetype_from_flags(is_shared, is_atomic)
			expr: e
			comments: comments
			pos: pos
		}
		if p.tok.kind != .rpar {
			p.check(.comma)
		}
	}
	return args
}

fn (mut p Parser) fn_decl() ast.FnDecl {
	p.top_level_statement_start()
	start_pos := p.tok.position()
	is_manualfree := p.is_manualfree || p.attrs.contains('manualfree')
	is_deprecated := p.attrs.contains('deprecated')
	is_direct_arr := p.attrs.contains('direct_array_access')
	mut is_unsafe := p.attrs.contains('unsafe')
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.check(.key_fn)
	p.open_scope()
	// C. || JS.
	mut language := table.Language.v
	if p.tok.kind == .name && p.tok.lit == 'C' {
		is_unsafe = !p.attrs.contains('trusted')
		language = table.Language.c
	} else if p.tok.kind == .name && p.tok.lit == 'JS' {
		language = table.Language.js
	}
	if language != .v {
		p.next()
		p.check(.dot)
		p.check_for_impure_v(language, p.tok.position())
	}
	// Receiver?
	mut rec_name := ''
	mut is_method := false
	mut receiver_pos := token.Position{}
	mut rec_type_pos := token.Position{}
	mut rec_type := table.void_type
	mut rec_mut := false
	mut params := []table.Param{}
	if p.tok.kind == .lpar {
		p.next() // (
		is_method = true
		is_shared := p.tok.kind == .key_shared
		is_atomic := p.tok.kind == .key_atomic
		rec_mut = p.tok.kind == .key_mut || is_shared || is_atomic
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
		if p.tok.kind == .name && p.tok.lit == 'JS' {
			language = table.Language.js
		}
		// if rec_mut {
		// p.check(.key_mut)
		// }
		// TODO: talk to alex, should mut be parsed with the type like this?
		// or should it be a property of the arg, like this ptr/mut becomes indistinguishable
		rec_type_pos = p.tok.position()
		rec_type = p.parse_type_with_mut(rec_mut)
		if rec_type.idx() == 0 {
			// error is set in parse_type
			return ast.FnDecl{
				scope: 0
			}
		}
		rec_type_pos = rec_type_pos.extend(p.prev_tok.position())
		if is_amp && rec_mut {
			p.error('use `(mut f Foo)` or `(f &Foo)` instead of `(mut f &Foo)`')
			return ast.FnDecl{
				scope: 0
			}
		}
		if is_shared {
			rec_type = rec_type.set_flag(.shared_f)
		}
		if is_atomic {
			rec_type = rec_type.set_flag(.atomic_f)
		}
		params << table.Param{
			pos: rec_start_pos
			name: rec_name
			is_mut: rec_mut
			typ: rec_type
		}
		p.check(.rpar)
	}
	mut name := ''
	if p.tok.kind == .name {
		pos := p.tok.position()
		// TODO high order fn
		name = if language == .js { p.check_js_name() } else { p.check_name() }
		if language == .v && !p.pref.translated && util.contains_capital(name) && p.mod != 'builtin' {
			p.error_with_pos('function names cannot contain uppercase letters, use snake_case instead',
				pos)
			return ast.FnDecl{
				scope: 0
			}
		}
		type_sym := p.table.get_type_symbol(rec_type)
		// interfaces are handled in the checker, methods can not be defined on them this way
		if is_method && (type_sym.has_method(name) && type_sym.kind != .interface_) {
			p.error_with_pos('duplicate method `$name`', pos)
			return ast.FnDecl{
				scope: 0
			}
		}
		// cannot redefine buildin function
		if !is_method && p.mod != 'builtin' && name in builtin_functions {
			p.error_with_pos('cannot redefine builtin function `$name`', pos)
			return ast.FnDecl{
				scope: 0
			}
		}
	}
	if p.tok.kind in [.plus, .minus, .mul, .div, .mod, .gt, .lt, .eq, .ne, .le, .ge] &&
		p.peek_tok.kind == .lpar {
		name = p.tok.kind.str() // op_to_fn_name()
		if rec_type == table.void_type {
			p.error_with_pos('cannot use operator overloading with normal functions',
				p.tok.position())
		}
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
	args2, are_args_type_only, is_variadic := p.fn_args()
	params << args2
	if !are_args_type_only {
		for param in params {
			if p.scope.known_var(param.name) {
				p.error_with_pos('redefinition of parameter `$param.name`', param.pos)
				return ast.FnDecl{
					scope: 0
				}
			}
			p.scope.register(ast.Var{
				name: param.name
				typ: param.typ
				is_mut: param.is_mut
				pos: param.pos
				is_used: true
				is_arg: true
			})
		}
	}
	mut end_pos := p.prev_tok.position()
	// Return type
	mut return_type := table.void_type
	if p.tok.kind.is_start_of_type() ||
		(p.tok.kind == .key_fn && p.tok.line_nr == p.prev_tok.line_nr) {
		return_type = p.parse_type()
	}
	mut type_sym_method_idx := 0
	// Register
	if is_method {
		mut type_sym := p.table.get_type_symbol(rec_type)
		// Do not allow to modify / add methods to types from other modules
		// arrays/maps dont belong to a module only their element types do
		// we could also check if kind is .array,  .array_fixed, .map instead of mod.len
		mut is_non_local := type_sym.mod.len > 0 && type_sym.mod != p.mod && type_sym.language == .v
		// check maps & arrays, must be defined in same module as the elem type
		if !is_non_local && type_sym.kind in [.array, .map] {
			elem_type_sym := p.table.get_type_symbol(p.table.value_type(rec_type))
			is_non_local = elem_type_sym.mod.len > 0 &&
				elem_type_sym.mod != p.mod && elem_type_sym.language == .v
		}
		if is_non_local {
			p.error_with_pos('cannot define new methods on non-local type $type_sym.name',
				rec_type_pos)
			return ast.FnDecl{
				scope: 0
			}
		}
		// p.warn('reg method $type_sym.name . $name ()')
		type_sym_method_idx = type_sym.register_method(table.Fn{
			name: name
			params: params
			return_type: return_type
			is_variadic: is_variadic
			is_generic: is_generic
			is_pub: is_pub
			is_deprecated: is_deprecated
			is_unsafe: is_unsafe
			mod: p.mod
			attrs: p.attrs
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
		// p.warn('reg functn $name ()')
		p.table.register_fn(table.Fn{
			name: name
			params: params
			return_type: return_type
			is_variadic: is_variadic
			is_generic: is_generic
			is_pub: is_pub
			is_deprecated: is_deprecated
			is_unsafe: is_unsafe
			mod: p.mod
			attrs: p.attrs
			language: language
		})
	}
	end_pos = p.prev_tok.position()
	// Body
	p.cur_fn_name = name
	mut stmts := []ast.Stmt{}
	no_body := p.tok.kind != .lcbr
	body_start_pos := p.peek_tok.position()
	if p.tok.kind == .lcbr {
		stmts = p.parse_block_no_scope(true)
	}
	if !no_body && are_args_type_only {
		p.error_with_pos('functions with type only args can not have bodies', body_start_pos)
		return ast.FnDecl{
			scope: 0
		}
	}
	fn_decl := ast.FnDecl{
		name: name
		mod: p.mod
		stmts: stmts
		return_type: return_type
		params: params
		is_manualfree: is_manualfree
		is_deprecated: is_deprecated
		is_direct_arr: is_direct_arr
		is_pub: is_pub
		is_generic: is_generic
		is_variadic: is_variadic
		receiver: ast.Field{
			name: rec_name
			typ: rec_type
		}
		receiver_pos: receiver_pos
		is_method: is_method
		method_type_pos: rec_type_pos
		method_idx: type_sym_method_idx
		rec_mut: rec_mut
		language: language
		no_body: no_body
		pos: start_pos.extend_with_last_line(end_pos, p.prev_tok.line_nr)
		body_pos: body_start_pos
		file: p.file_name
		is_builtin: p.builtin_mod || p.mod in util.builtin_module_parts
		attrs: p.attrs
		scope: p.scope
	}
	p.close_scope()
	return fn_decl
}

fn (mut p Parser) anon_fn() ast.AnonFn {
	pos := p.tok.position()
	p.check(.key_fn)
	if p.pref.is_script && p.tok.kind == .name {
		p.error_with_pos('function declarations in script mode should be before all script statements',
			p.tok.position())
		return ast.AnonFn{}
	}
	p.open_scope()
	// TODO generics
	args, _, is_variadic := p.fn_args()
	for arg in args {
		p.scope.register(ast.Var{
			name: arg.name
			typ: arg.typ
			is_mut: arg.is_mut
			pos: arg.pos
			is_used: true
			is_arg: true
		})
	}
	mut same_line := p.tok.line_nr == p.prev_tok.line_nr
	mut return_type := table.void_type
	// lpar: multiple return types
	if same_line {
		if p.tok.kind.is_start_of_type() {
			return_type = p.parse_type()
		} else if p.tok.kind != .lcbr {
			p.error_with_pos('expected return type, not $p.tok for anonymous function',
				p.tok.position())
		}
	}
	mut stmts := []ast.Stmt{}
	no_body := p.tok.kind != .lcbr
	same_line = p.tok.line_nr == p.prev_tok.line_nr
	if no_body && same_line {
		p.error_with_pos('unexpected `$p.tok.kind` after anonymous function signature, expecting `{`',
			p.tok.position())
	}
	if p.tok.kind == .lcbr {
		stmts = p.parse_block_no_scope(false)
	}
	p.close_scope()
	mut func := table.Fn{
		params: args
		is_variadic: is_variadic
		return_type: return_type
	}
	name := 'anon_${p.tok.pos}_${p.table.fn_type_signature(func)}'
	func.name = name
	idx := p.table.find_or_register_fn_type(p.mod, func, true, false)
	typ := table.new_type(idx)
	// name := p.table.get_type_name(typ)
	return ast.AnonFn{
		decl: ast.FnDecl{
			name: name
			mod: p.mod
			stmts: stmts
			return_type: return_type
			params: args
			is_variadic: is_variadic
			is_method: false
			is_anon: true
			no_body: no_body
			pos: pos
			file: p.file_name
			scope: p.scope
		}
		typ: typ
	}
}

// part of fn declaration
fn (mut p Parser) fn_args() ([]table.Param, bool, bool) {
	p.check(.lpar)
	mut args := []table.Param{}
	mut is_variadic := false
	// `int, int, string` (no names, just types)
	argname := if p.tok.kind == .name && p.tok.lit.len > 0 && p.tok.lit[0].is_capital() {
		p.prepend_mod(p.tok.lit)
	} else {
		p.tok.lit
	}
	types_only := p.tok.kind in [.amp, .ellipsis, .key_fn] ||
		(p.peek_tok.kind == .comma && p.table.known_type(argname)) || p.peek_tok.kind == .dot ||
		p.peek_tok.kind == .rpar
	// TODO copy pasta, merge 2 branches
	if types_only {
		// p.warn('types only')
		mut arg_no := 1
		for p.tok.kind != .rpar {
			if p.tok.kind == .eof {
				p.error_with_pos('expecting `)`', p.tok.position())
				return []table.Param{}, false, false
			}
			is_shared := p.tok.kind == .key_shared
			is_atomic := p.tok.kind == .key_atomic
			is_mut := p.tok.kind == .key_mut || is_shared || is_atomic
			if is_mut {
				p.next()
			}
			if p.tok.kind == .ellipsis {
				p.next()
				is_variadic = true
			}
			pos := p.tok.position()
			mut arg_type := p.parse_type()
			if arg_type == 0 {
				// error is added in parse_type
				return []table.Param{}, false, false
			}
			if is_mut {
				if !arg_type.has_flag(.generic) {
					if is_shared {
						p.check_fn_shared_arguments(arg_type, pos)
					} else if is_atomic {
						p.check_fn_atomic_arguments(arg_type, pos)
					} else {
						p.check_fn_mutable_arguments(arg_type, pos)
					}
				} else if is_shared || is_atomic {
					p.error_with_pos('generic object cannot be `atomic`or `shared`', pos)
					return []table.Param{}, false, false
				}
				// if arg_type.is_ptr() {
				// p.error('cannot mut')
				// }
				// arg_type = arg_type.to_ptr()
				arg_type = arg_type.set_nr_muls(1)
				if is_shared {
					arg_type = arg_type.set_flag(.shared_f)
				}
				if is_atomic {
					arg_type = arg_type.set_flag(.atomic_f)
				}
			}
			if is_variadic {
				arg_type = table.new_type(p.table.find_or_register_array(arg_type)).set_flag(.variadic)
			}
			if p.tok.kind == .eof {
				p.error_with_pos('expecting `)`', p.prev_tok.position())
				return []table.Param{}, false, false
			}
			if p.tok.kind == .comma {
				if is_variadic {
					p.error_with_pos('cannot use ...(variadic) with non-final parameter no $arg_no',
						pos)
					return []table.Param{}, false, false
				}
				p.next()
			}
			args << table.Param{
				pos: pos
				name: ''
				is_mut: is_mut
				typ: arg_type
			}
			arg_no++
			if arg_no > 1024 {
				p.error_with_pos('too many args', pos)
				return []table.Param{}, false, false
			}
		}
	} else {
		for p.tok.kind != .rpar {
			if p.tok.kind == .eof {
				p.error_with_pos('expecting `)`', p.tok.position())
				return []table.Param{}, false, false
			}
			is_shared := p.tok.kind == .key_shared
			is_atomic := p.tok.kind == .key_atomic
			mut is_mut := p.tok.kind == .key_mut || is_shared || is_atomic
			if is_mut {
				p.next()
			}
			mut arg_pos := [p.tok.position()]
			mut arg_names := [p.check_name()]
			// `a, b, c int`
			for p.tok.kind == .comma {
				if !p.pref.is_fmt {
					p.warn('`fn f(x, y Type)` syntax has been deprecated and will soon be removed. ' +
						'Use `fn f(x Type, y Type)` instead. You can run `v fmt -w "$p.scanner.file_path"` to automatically fix your code.')
				}
				p.next()
				arg_pos << p.tok.position()
				arg_names << p.check_name()
			}
			if p.tok.kind == .key_mut {
				// TODO remove old syntax
				p.warn_with_pos('use `mut f Foo` instead of `f mut Foo`', p.tok.position())
				is_mut = true
			}
			if p.tok.kind == .ellipsis {
				p.next()
				is_variadic = true
			}
			pos := p.tok.position()
			mut typ := p.parse_type()
			if typ == 0 {
				// error is added in parse_type
				return []table.Param{}, false, false
			}
			if is_mut {
				if !typ.has_flag(.generic) {
					if is_shared {
						p.check_fn_shared_arguments(typ, pos)
					} else if is_atomic {
						p.check_fn_atomic_arguments(typ, pos)
					} else {
						p.check_fn_mutable_arguments(typ, pos)
					}
				} else if is_shared || is_atomic {
					p.error_with_pos('generic object cannot be `atomic` or `shared`',
						pos)
					return []table.Param{}, false, false
				}
				typ = typ.set_nr_muls(1)
				if is_shared {
					typ = typ.set_flag(.shared_f)
				}
				if is_atomic {
					typ = typ.set_flag(.atomic_f)
				}
			}
			if is_variadic {
				typ = table.new_type(p.table.find_or_register_array(typ)).set_flag(.variadic)
			}
			for i, arg_name in arg_names {
				args << table.Param{
					pos: arg_pos[i]
					name: arg_name
					is_mut: is_mut
					typ: typ
				}
				// if typ.typ.kind == .variadic && p.tok.kind == .comma {
				if is_variadic && p.tok.kind == .comma {
					p.error_with_pos('cannot use ...(variadic) with non-final parameter $arg_name',
						arg_pos[i])
					return []table.Param{}, false, false
				}
			}
			if p.tok.kind == .eof {
				p.error_with_pos('expecting `)`', p.prev_tok.position())
				return []table.Param{}, false, false
			}
			if p.tok.kind != .rpar {
				p.check(.comma)
			}
		}
	}
	p.check(.rpar)
	return args, types_only, is_variadic
}

fn (mut p Parser) check_fn_mutable_arguments(typ table.Type, pos token.Position) {
	sym := p.table.get_type_symbol(typ)
	if sym.kind !in [.array, .array_fixed, .struct_, .map, .placeholder, .sum_type] &&
		!typ.is_ptr() && !typ.is_pointer() {
		p.error_with_pos('mutable arguments are only allowed for arrays, maps, structs and pointers\n' +
			'return values instead: `fn foo(mut n $sym.name) {` => `fn foo(n $sym.name) $sym.name {`',
			pos)
	}
}

fn (mut p Parser) check_fn_shared_arguments(typ table.Type, pos token.Position) {
	sym := p.table.get_type_symbol(typ)
	if sym.kind !in [.array, .struct_, .map, .placeholder] && !typ.is_ptr() {
		p.error_with_pos('shared arguments are only allowed for arrays, maps, and structs\n',
			pos)
	}
}

fn (mut p Parser) check_fn_atomic_arguments(typ table.Type, pos token.Position) {
	sym := p.table.get_type_symbol(typ)
	if sym.kind !in [.u32, .int, .u64] {
		p.error_with_pos('atomic arguments are only allowed for 32/64 bit integers\n' +
			'use shared arguments instead: `fn foo(atomic n $sym.name) {` => `fn foo(shared n $sym.name) {`',
			pos)
	}
}

fn (mut p Parser) fn_redefinition_error(name string) {
	if p.pref.translated {
		return
	}
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
	for stmt in stmts {
		if stmt is ast.FnDecl {
			if stmt.name == 'main.main' && stmt.mod == 'main' {
				return true
			}
		}
	}
	return false
}
