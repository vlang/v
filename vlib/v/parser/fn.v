// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.token
import v.util

pub fn (mut p Parser) call_expr(language ast.Language, mod string) ast.CallExpr {
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
		or_kind = .block
	}

	old_expr_mod := p.expr_mod
	defer {
		p.expr_mod = old_expr_mod
	}
	p.expr_mod = ''

	mut concrete_types := []ast.Type{}
	mut concrete_list_pos := p.tok.position()
	if p.tok.kind == .lt {
		// `foo<int>(10)`
		p.expr_mod = ''
		concrete_types = p.parse_generic_type_list()
		concrete_list_pos = concrete_list_pos.extend(p.prev_tok.position())
		// In case of `foo<T>()`
		// T is unwrapped and registered in the checker.
		full_generic_fn_name := if fn_name.contains('.') { fn_name } else { p.prepend_mod(fn_name) }
		has_generic_generic := concrete_types.filter(it.has_flag(.generic)).len > 0
		if !has_generic_generic {
			// will be added in checker
			p.table.register_fn_generic_types(full_generic_fn_name, concrete_types)
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
	mut pos := first_pos.extend(last_pos)
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
			typ: ast.error_type
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
	comments := p.eat_comments(same_line: true)
	pos.update_last_line(p.prev_tok.line_nr)
	return ast.CallExpr{
		name: fn_name
		name_pos: first_pos
		args: args
		mod: p.mod
		pos: pos
		language: language
		concrete_types: concrete_types
		concrete_list_pos: concrete_list_pos
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
		mut comments := p.eat_comments({})
		arg_start_pos := p.tok.position()
		mut array_decompose := false
		if p.tok.kind == .ellipsis {
			p.next()
			array_decompose = true
		}
		mut expr := ast.empty_expr()
		if p.tok.kind == .name && p.peek_tok.kind == .colon {
			// `foo(key:val, key2:val2)`
			expr = p.struct_init(true) // short_syntax:true
		} else {
			expr = p.expr(0)
		}
		if array_decompose {
			expr = ast.ArrayDecompose{
				expr: expr
				pos: p.tok.position()
			}
		}
		if mut expr is ast.StructInit {
			expr.pre_comments << comments
			comments = []ast.Comment{}
		}
		pos := arg_start_pos.extend(p.prev_tok.position())
		comments << p.eat_comments({})
		args << ast.CallArg{
			is_mut: is_mut
			share: ast.sharetype_from_flags(is_shared, is_atomic)
			expr: expr
			comments: comments
			pos: pos
		}
		if p.tok.kind != .rpar {
			p.check(.comma)
		}
	}
	return args
}

struct ReceiverParsingInfo {
mut:
	name     string
	pos      token.Position
	typ      ast.Type
	type_pos token.Position
	is_mut   bool
	language ast.Language
}

fn (mut p Parser) fn_decl() ast.FnDecl {
	p.top_level_statement_start()
	start_pos := p.tok.position()
	is_manualfree := p.is_manualfree || p.attrs.contains('manualfree')
	is_deprecated := p.attrs.contains('deprecated')
	is_direct_arr := p.attrs.contains('direct_array_access')
	conditional_ctdefine := p.attrs.find_comptime_define() or { '' }
	mut is_unsafe := p.attrs.contains('unsafe')
	is_keep_alive := p.attrs.contains('keep_args_alive')
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.check(.key_fn)
	p.open_scope()
	// C. || JS.
	mut language := ast.Language.v
	if p.tok.kind == .name && p.tok.lit == 'C' {
		is_unsafe = !p.attrs.contains('trusted')
		language = ast.Language.c
	} else if p.tok.kind == .name && p.tok.lit == 'JS' {
		language = ast.Language.js
	}
	if is_keep_alive && language != .c {
		p.error_with_pos('attribute [keep_args_alive] is only supported for C functions',
			p.tok.position())
	}
	if language != .v {
		p.next()
		p.check(.dot)
		p.check_for_impure_v(language, p.tok.position())
	}
	// Receiver?
	mut rec := ReceiverParsingInfo{
		typ: ast.void_type
		language: language
	}
	mut is_method := false
	mut params := []ast.Param{}
	if p.tok.kind == .lpar {
		is_method = true
		p.fn_receiver(mut params, mut rec) or { return ast.FnDecl{
			scope: 0
		} }

		// rec.language was initialized with language variable.
		// So language is changed only if rec.language has been changed.
		language = rec.language
	}
	mut name := ''
	name_pos := p.tok.position()
	if p.tok.kind == .name {
		// TODO high order fn
		name = if language == .js { p.check_js_name() } else { p.check_name() }
		if language == .v && !p.pref.translated && util.contains_capital(name) && !p.builtin_mod {
			p.error_with_pos('function names cannot contain uppercase letters, use snake_case instead',
				name_pos)
			return ast.FnDecl{
				scope: 0
			}
		}
		type_sym := p.table.get_type_symbol(rec.typ)
		if is_method {
			mut is_duplicate := type_sym.has_method(name)
			// make sure this is a normal method and not an interface method
			if type_sym.kind == .interface_ && is_duplicate {
				if type_sym.info is ast.Interface {
					// if the method is in info then its an interface method
					is_duplicate = !type_sym.info.has_method(name)
				}
			}
			if is_duplicate {
				p.error_with_pos('duplicate method `$name`', name_pos)
				return ast.FnDecl{
					scope: 0
				}
			}
		}
		// cannot redefine buildin function
		if !is_method && !p.builtin_mod && name in builtin_functions {
			p.error_with_pos('cannot redefine builtin function `$name`', name_pos)
			return ast.FnDecl{
				scope: 0
			}
		}
	} else if p.tok.kind in [.plus, .minus, .mul, .div, .mod, .lt, .eq] && p.peek_tok.kind == .lpar {
		name = p.tok.kind.str() // op_to_fn_name()
		if rec.typ == ast.void_type {
			p.error_with_pos('cannot use operator overloading with normal functions',
				p.tok.position())
		}
		p.next()
	} else if p.tok.kind in [.ne, .gt, .ge, .le] && p.peek_tok.kind == .lpar {
		p.error_with_pos('cannot overload `!=`, `>`, `<=` and `>=` as they are auto generated from `==` and`<`',
			p.tok.position())
	} else {
		p.error_with_pos('expecting method name', p.tok.position())
		return ast.FnDecl{
			scope: 0
		}
	}
	// <T>
	generic_names := p.parse_generic_names()
	// check generic receiver method has no generic names
	if is_method && rec.typ.has_flag(.generic) && generic_names.len == 0
		&& p.table.get_type_symbol(rec.typ).kind != .any {
		p.error_with_pos('generic receiver method `$name` should add generic names, e.g. $name<T>',
			name_pos)
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
				is_auto_deref: param.is_mut || param.is_auto_rec
				pos: param.pos
				is_used: true
				is_arg: true
			})
		}
	}
	// Return type
	mut return_type_pos := p.tok.position()
	mut return_type := ast.void_type
	// don't confuse token on the next line: fn decl, [attribute]
	same_line := p.tok.line_nr == p.prev_tok.line_nr
	if (p.tok.kind.is_start_of_type() && (same_line || p.tok.kind != .lsbr))
		|| (same_line && p.tok.kind == .key_fn) {
		return_type = p.parse_type()
		return_type_pos = return_type_pos.extend(p.prev_tok.position())
	}
	mut type_sym_method_idx := 0
	no_body := p.tok.kind != .lcbr
	end_pos := p.prev_tok.position()
	short_fn_name := name
	is_main := short_fn_name == 'main' && p.mod == 'main'
	is_test := short_fn_name.starts_with('test_') || short_fn_name.starts_with('testsuite_')
	// Register
	if is_method {
		mut type_sym := p.table.get_type_symbol(rec.typ)
		// Do not allow to modify / add methods to types from other modules
		// arrays/maps dont belong to a module only their element types do
		// we could also check if kind is .array,  .array_fixed, .map instead of mod.len
		mut is_non_local := type_sym.mod.len > 0 && type_sym.mod != p.mod && type_sym.language == .v
		// check maps & arrays, must be defined in same module as the elem type
		if !is_non_local && type_sym.kind in [.array, .map] {
			elem_type_sym := p.table.get_type_symbol(p.table.value_type(rec.typ))
			is_non_local = elem_type_sym.mod.len > 0 && elem_type_sym.mod != p.mod
				&& elem_type_sym.language == .v
		}
		if is_non_local {
			p.error_with_pos('cannot define new methods on non-local type $type_sym.name',
				rec.type_pos)
			return ast.FnDecl{
				scope: 0
			}
		}
		type_sym_method_idx = type_sym.register_method(ast.Fn{
			name: name
			params: params
			return_type: return_type
			is_variadic: is_variadic
			generic_names: generic_names
			is_pub: is_pub
			is_deprecated: is_deprecated
			is_unsafe: is_unsafe
			is_main: is_main
			is_test: is_test
			is_conditional: conditional_ctdefine != ''
			is_keep_alive: is_keep_alive
			ctdefine: conditional_ctdefine
			no_body: no_body
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
		if !p.pref.translated && language == .v && name in p.table.fns {
			p.table.redefined_fns << name
		}
		p.table.register_fn(ast.Fn{
			name: name
			params: params
			return_type: return_type
			is_variadic: is_variadic
			generic_names: generic_names
			is_pub: is_pub
			is_deprecated: is_deprecated
			is_unsafe: is_unsafe
			is_main: is_main
			is_test: is_test
			is_conditional: conditional_ctdefine != ''
			is_keep_alive: is_keep_alive
			ctdefine: conditional_ctdefine
			no_body: no_body
			mod: p.mod
			attrs: p.attrs
			language: language
		})
	}
	// Body
	p.cur_fn_name = name
	mut stmts := []ast.Stmt{}
	body_start_pos := p.peek_tok.position()
	if p.tok.kind == .lcbr {
		p.inside_fn = true
		p.inside_unsafe_fn = is_unsafe
		stmts = p.parse_block_no_scope(true)
		p.inside_unsafe_fn = false
		p.inside_fn = false
	}
	if !no_body && are_args_type_only {
		p.error_with_pos('functions with type only args can not have bodies', body_start_pos)
		return ast.FnDecl{
			scope: 0
		}
	}
	// if no_body && !name.starts_with('C.') {
	// 	p.error_with_pos('did you mean C.$name instead of $name', start_pos)
	// }
	fn_decl := ast.FnDecl{
		name: name
		mod: p.mod
		stmts: stmts
		return_type: return_type
		return_type_pos: return_type_pos
		params: params
		is_manualfree: is_manualfree
		is_deprecated: is_deprecated
		is_direct_arr: is_direct_arr
		is_pub: is_pub
		is_variadic: is_variadic
		is_main: is_main
		is_test: is_test
		is_conditional: conditional_ctdefine != ''
		is_keep_alive: is_keep_alive
		receiver: ast.StructField{
			name: rec.name
			typ: rec.typ
		}
		generic_names: generic_names
		receiver_pos: rec.pos
		is_method: is_method
		method_type_pos: rec.type_pos
		method_idx: type_sym_method_idx
		rec_mut: rec.is_mut
		language: language
		no_body: no_body
		pos: start_pos.extend_with_last_line(end_pos, p.prev_tok.line_nr)
		body_pos: body_start_pos
		file: p.file_name
		is_builtin: p.builtin_mod || p.mod in util.builtin_module_parts
		attrs: p.attrs
		scope: p.scope
		label_names: p.label_names
	}
	p.label_names = []
	p.close_scope()
	return fn_decl
}

fn (mut p Parser) fn_receiver(mut params []ast.Param, mut rec ReceiverParsingInfo) ? {
	lpar_pos := p.tok.position()
	p.next() // (
	is_shared := p.tok.kind == .key_shared
	is_atomic := p.tok.kind == .key_atomic
	rec.is_mut = p.tok.kind == .key_mut || is_shared || is_atomic
	if rec.is_mut {
		p.next() // `mut`
	}
	rec_start_pos := p.tok.position()
	rec.name = p.check_name()
	if !rec.is_mut {
		rec.is_mut = p.tok.kind == .key_mut
		if rec.is_mut {
			ptoken2 := p.peek_token(2) // needed to prevent codegen bug, where .position() expects &Token
			p.warn_with_pos('use `(mut f Foo)` instead of `(f mut Foo)`', lpar_pos.extend(ptoken2.position()))
		}
	}
	if p.tok.kind == .key_shared {
		ptoken2 := p.peek_token(2) // needed to prevent codegen bug, where .position() expects &Token
		p.error_with_pos('use `(shared f Foo)` instead of `(f shared Foo)`', lpar_pos.extend(ptoken2.position()))
	}
	rec.pos = rec_start_pos.extend(p.tok.position())
	is_amp := p.tok.kind == .amp
	if p.tok.kind == .name && p.tok.lit == 'JS' {
		rec.language = ast.Language.js
	}
	// if rec.is_mut {
	// p.check(.key_mut)
	// }
	// TODO: talk to alex, should mut be parsed with the type like this?
	// or should it be a property of the arg, like this ptr/mut becomes indistinguishable
	rec.type_pos = p.tok.position()
	rec.typ = p.parse_type_with_mut(rec.is_mut)
	if rec.typ.idx() == 0 {
		// error is set in parse_type
		return none
	}
	rec.type_pos = rec.type_pos.extend(p.prev_tok.position())
	if is_amp && rec.is_mut {
		p.error_with_pos('use `(mut f Foo)` or `(f &Foo)` instead of `(mut f &Foo)`',
			lpar_pos.extend(p.tok.position()))
		return none
	}
	if is_shared {
		rec.typ = rec.typ.set_flag(.shared_f)
	}
	if is_atomic {
		rec.typ = rec.typ.set_flag(.atomic_f)
	}
	// optimize method `automatic use fn (a &big_foo) instead of fn (a big_foo)`
	type_sym := p.table.get_type_symbol(rec.typ)
	mut is_auto_rec := false
	if type_sym.kind == .struct_ {
		info := type_sym.info as ast.Struct
		if !rec.is_mut && !rec.typ.is_ptr() && info.fields.len > 8 {
			rec.typ = rec.typ.to_ptr()
			is_auto_rec = true
		}
	}

	params << ast.Param{
		pos: rec_start_pos
		name: rec.name
		is_mut: rec.is_mut
		is_auto_rec: is_auto_rec
		typ: rec.typ
		type_pos: rec.type_pos
	}
	p.check(.rpar)

	return
}

fn (mut p Parser) parse_generic_names() []string {
	mut param_names := []string{}
	if p.tok.kind != .lt {
		return param_names
	}
	p.check(.lt)
	mut first_done := false
	mut count := 0
	for p.tok.kind !in [.gt, .eof] {
		if first_done {
			p.check(.comma)
		}
		name := p.tok.lit
		if name.len > 0 && !name[0].is_capital() {
			p.error('generic parameter needs to be uppercase')
		}
		if name.len > 1 {
			p.error('generic parameter name needs to be exactly one char')
		}
		if !util.is_generic_type_name(p.tok.lit) {
			p.error('`$p.tok.lit` is a reserved name and cannot be used for generics')
		}
		if name in param_names {
			p.error('duplicated generic parameter `$name`')
		}
		if count > 8 {
			p.error('cannot have more than 9 generic parameters')
		}
		p.check(.name)
		param_names << name
		first_done = true
		count++
	}
	p.check(.gt)
	return param_names
}

// is_generic_name returns true if the current token is a generic name.
fn (p Parser) is_generic_name() bool {
	return p.tok.kind == .name && util.is_generic_type_name(p.tok.lit)
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
	p.scope.detached_from_parent = true
	// TODO generics
	args, _, is_variadic := p.fn_args()
	for arg in args {
		if arg.name.len == 0 {
			p.error_with_pos('use `_` to name an unused parameter', arg.pos)
		}
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
	mut return_type := ast.void_type
	mut return_type_pos := p.tok.position()
	// lpar: multiple return types
	if same_line {
		if p.tok.kind.is_start_of_type() {
			return_type = p.parse_type()
			return_type_pos = return_type_pos.extend(p.tok.position())
		} else if p.tok.kind != .lcbr {
			p.error_with_pos('expected return type, not $p.tok for anonymous function',
				p.tok.position())
		}
	}
	mut stmts := []ast.Stmt{}
	no_body := p.tok.kind != .lcbr
	same_line = p.tok.line_nr == p.prev_tok.line_nr
	if no_body && same_line {
		p.error_with_pos('unexpected $p.tok after anonymous function signature, expecting `{`',
			p.tok.position())
	}
	mut label_names := []string{}
	mut func := ast.Fn{
		params: args
		is_variadic: is_variadic
		return_type: return_type
	}
	name := 'anon_fn_${p.table.fn_type_signature(func)}_$p.tok.pos'
	keep_fn_name := p.cur_fn_name
	p.cur_fn_name = name
	if p.tok.kind == .lcbr {
		tmp := p.label_names
		p.label_names = []
		stmts = p.parse_block_no_scope(false)
		label_names = p.label_names
		p.label_names = tmp
	}
	p.cur_fn_name = keep_fn_name
	p.close_scope()
	func.name = name
	idx := p.table.find_or_register_fn_type(p.mod, func, true, false)
	typ := ast.new_type(idx)
	// name := p.table.get_type_name(typ)
	return ast.AnonFn{
		decl: ast.FnDecl{
			name: name
			mod: p.mod
			stmts: stmts
			return_type: return_type
			return_type_pos: return_type_pos
			params: args
			is_variadic: is_variadic
			is_method: false
			is_anon: true
			no_body: no_body
			pos: pos.extend(p.prev_tok.position())
			file: p.file_name
			scope: p.scope
			label_names: label_names
		}
		typ: typ
	}
}

// part of fn declaration
fn (mut p Parser) fn_args() ([]ast.Param, bool, bool) {
	p.check(.lpar)
	mut args := []ast.Param{}
	mut is_variadic := false
	// `int, int, string` (no names, just types)
	argname := if p.tok.kind == .name && p.tok.lit.len > 0 && p.tok.lit[0].is_capital() {
		p.prepend_mod(p.tok.lit)
	} else {
		p.tok.lit
	}
	types_only := p.tok.kind in [.amp, .ellipsis, .key_fn, .lsbr]
		|| (p.peek_tok.kind == .comma && p.table.known_type(argname))
		|| p.peek_tok.kind == .dot || p.peek_tok.kind == .rpar
	// TODO copy pasta, merge 2 branches
	if types_only {
		mut arg_no := 1
		for p.tok.kind != .rpar {
			if p.tok.kind == .eof {
				p.error_with_pos('expecting `)`', p.tok.position())
				return []ast.Param{}, false, false
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
				return []ast.Param{}, false, false
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
					return []ast.Param{}, false, false
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
				arg_type = ast.new_type(p.table.find_or_register_array(arg_type)).set_flag(.variadic)
			}
			if p.tok.kind == .eof {
				p.error_with_pos('expecting `)`', p.prev_tok.position())
				return []ast.Param{}, false, false
			}
			if p.tok.kind == .comma {
				if is_variadic {
					p.error_with_pos('cannot use ...(variadic) with non-final parameter no $arg_no',
						pos)
					return []ast.Param{}, false, false
				}
				p.next()
			}
			args << ast.Param{
				pos: pos
				name: ''
				is_mut: is_mut
				typ: arg_type
			}
			arg_no++
			if arg_no > 1024 {
				p.error_with_pos('too many args', pos)
				return []ast.Param{}, false, false
			}
		}
	} else {
		for p.tok.kind != .rpar {
			if p.tok.kind == .eof {
				p.error_with_pos('expecting `)`', p.tok.position())
				return []ast.Param{}, false, false
			}
			is_shared := p.tok.kind == .key_shared
			is_atomic := p.tok.kind == .key_atomic
			mut is_mut := p.tok.kind == .key_mut || is_shared || is_atomic
			if is_mut {
				p.next()
			}
			mut arg_pos := [p.tok.position()]
			mut arg_names := [p.check_name()]
			mut type_pos := [p.tok.position()]
			// `a, b, c int`
			for p.tok.kind == .comma {
				if !p.pref.is_fmt {
					p.warn(
						'`fn f(x, y Type)` syntax has been deprecated and will soon be removed. ' +
						'Use `fn f(x Type, y Type)` instead. You can run `v fmt -w "$p.scanner.file_path"` to automatically fix your code.')
				}
				p.next()
				arg_pos << p.tok.position()
				arg_names << p.check_name()
				type_pos << p.tok.position()
			}
			if p.tok.kind == .key_mut {
				// TODO remove old syntax
				if !p.pref.is_fmt {
					p.warn_with_pos('use `mut f Foo` instead of `f mut Foo`', p.tok.position())
				}
				is_mut = true
			}
			if p.tok.kind == .key_shared {
				p.error_with_pos('use `shared f Foo` instead of `f shared Foo`', p.tok.position())
			}
			if p.tok.kind == .ellipsis {
				p.next()
				is_variadic = true
			}
			pos := p.tok.position()
			mut typ := p.parse_type()
			if typ == 0 {
				// error is added in parse_type
				return []ast.Param{}, false, false
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
					return []ast.Param{}, false, false
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
				typ = ast.new_type(p.table.find_or_register_array(typ)).derive(typ).set_flag(.variadic)
			}
			for i, arg_name in arg_names {
				args << ast.Param{
					pos: arg_pos[i]
					name: arg_name
					is_mut: is_mut
					typ: typ
					type_pos: type_pos[i]
				}
				// if typ.typ.kind == .variadic && p.tok.kind == .comma {
				if is_variadic && p.tok.kind == .comma {
					p.error_with_pos('cannot use ...(variadic) with non-final parameter $arg_name',
						arg_pos[i])
					return []ast.Param{}, false, false
				}
			}
			if p.tok.kind == .eof {
				p.error_with_pos('expecting `)`', p.prev_tok.position())
				return []ast.Param{}, false, false
			}
			if p.tok.kind != .rpar {
				p.check(.comma)
			}
		}
	}
	p.check(.rpar)
	return args, types_only, is_variadic
}

fn (mut p Parser) check_fn_mutable_arguments(typ ast.Type, pos token.Position) {
	sym := p.table.get_type_symbol(typ)
	if sym.kind in [.array, .array_fixed, .interface_, .map, .placeholder, .struct_,
		.generic_struct_inst, .sum_type] {
		return
	}
	if typ.is_ptr() || typ.is_pointer() {
		return
	}
	if sym.kind == .alias {
		atyp := (sym.info as ast.Alias).parent_type
		p.check_fn_mutable_arguments(atyp, pos)
		return
	}
	p.error_with_pos(
		'mutable arguments are only allowed for arrays, interfaces, maps, pointers, structs or their aliases\n' +
		'return values instead: `fn foo(mut n $sym.name) {` => `fn foo(n $sym.name) $sym.name {`',
		pos)
}

fn (mut p Parser) check_fn_shared_arguments(typ ast.Type, pos token.Position) {
	sym := p.table.get_type_symbol(typ)
	if sym.kind !in [.array, .struct_, .map, .placeholder] && !typ.is_ptr() {
		p.error_with_pos('shared arguments are only allowed for arrays, maps, and structs\n',
			pos)
	}
}

fn (mut p Parser) check_fn_atomic_arguments(typ ast.Type, pos token.Position) {
	sym := p.table.get_type_symbol(typ)
	if sym.kind !in [.u32, .int, .u64] {
		p.error_with_pos('atomic arguments are only allowed for 32/64 bit integers\n' +
			'use shared arguments instead: `fn foo(atomic n $sym.name) {` => `fn foo(shared n $sym.name) {`',
			pos)
	}
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
