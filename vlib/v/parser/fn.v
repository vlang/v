// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.token
import v.util
import os

fn (mut p Parser) call_expr(language ast.Language, mod string) ast.CallExpr {
	first_pos := p.tok.pos()
	mut name := if language == .js { p.check_js_name() } else { p.check_name() }
	mut is_static_type_method := language == .v && name != '' && name[0].is_capital()
		&& p.tok.kind == .dot
	if is_static_type_method {
		p.check(.dot)
		name = name + '__static__' + p.check_name()
	}
	mut fn_name := if language == .c {
		'C.${name}'
	} else if language == .js {
		'JS.${name}'
	} else if language == .wasm {
		'WASM.${name}'
	} else if mod != '' {
		'${mod}.${name}'
	} else {
		name
	}
	if language != .v {
		p.check_for_impure_v(language, first_pos)
	}
	mut or_kind := ast.OrKind.absent
	if fn_name == 'json.decode' || fn_name == 'C.va_arg' {
		p.expecting_type = true // Makes name_expr() parse the type `User` in `json.decode(User, txt)`
	}

	old_expr_mod := p.expr_mod
	defer {
		p.expr_mod = old_expr_mod
	}
	p.expr_mod = ''

	mut concrete_types := []ast.Type{}
	mut concrete_list_pos := p.tok.pos()
	if p.tok.kind in [.lt, .lsbr] {
		// `foo<int>(10)`
		p.expr_mod = ''
		concrete_types = p.parse_concrete_types()
		concrete_list_pos = concrete_list_pos.extend(p.prev_tok.pos())
	}
	p.check(.lpar)
	args := p.call_args()
	if p.tok.kind != .rpar {
		params := p.table.fns[fn_name] or { unsafe { p.table.fns['${p.mod}.${fn_name}'] } }.params
		if args.len < params.len && p.prev_tok.kind != .comma {
			pos := if p.tok.kind == .eof { p.prev_tok.pos() } else { p.tok.pos() }
			p.unexpected_with_pos(pos, expecting: '`,`')
		} else if args.len > params.len {
			ok_arg_pos := (args[params.len - 1] or { args[0] }).pos
			pos := token.Pos{
				...ok_arg_pos
				col: ok_arg_pos.col + ok_arg_pos.len
			}
			p.unexpected_with_pos(pos.extend(p.tok.pos()), expecting: '`)`')
		} else {
			pos := if p.tok.kind == .eof { p.prev_tok.pos() } else { p.tok.pos() }
			p.unexpected_with_pos(pos, expecting: '`)`')
		}
	}
	last_pos := p.tok.pos()
	p.next()
	mut pos := first_pos.extend(last_pos)
	mut or_stmts := []ast.Stmt{} // TODO: remove unnecessary allocations by just using .absent
	mut or_pos := p.tok.pos()
	if p.tok.kind == .key_orelse {
		// `foo() or {}``
		or_kind = .block
		or_stmts, or_pos = p.or_block(.with_err_var)
	}
	if p.tok.kind in [.question, .not] {
		is_not := p.tok.kind == .not
		// `foo()?`
		p.next()
		if p.inside_defer {
			p.error_with_pos('error propagation not allowed inside `defer` blocks', p.prev_tok.pos())
		}
		or_kind = if is_not { .propagate_result } else { .propagate_option }
	}
	if fn_name in p.imported_symbols {
		fn_name = p.imported_symbols[fn_name]
		p.register_used_import_for_symbol_name(fn_name)
	}
	comments := p.eat_comments(same_line: true)
	pos.update_last_line(p.prev_tok.line_nr)
	return ast.CallExpr{
		name:               fn_name
		name_pos:           first_pos
		args:               args
		mod:                p.mod
		pos:                pos
		language:           language
		concrete_types:     concrete_types
		concrete_list_pos:  concrete_list_pos
		raw_concrete_types: concrete_types
		or_block:           ast.OrExpr{
			stmts: or_stmts
			kind:  or_kind
			pos:   or_pos
		}
		scope:              p.scope
		comments:           comments
		is_return_used:     p.expecting_value
		is_static_method:   is_static_type_method
	}
}

fn (mut p Parser) call_args() []ast.CallArg {
	prev_inside_call_args := p.inside_call_args
	p.inside_call_args = true
	defer {
		p.inside_call_args = prev_inside_call_args
	}
	mut args := []ast.CallArg{}
	for p.tok.kind != .rpar {
		if p.tok.kind == .eof {
			return args
		}
		is_shared := p.tok.kind == .key_shared
		is_atomic := p.tok.kind == .key_atomic
		is_mut := p.tok.kind == .key_mut || is_shared || is_atomic
		if is_mut {
			p.next()
		}
		mut comments := p.eat_comments()
		arg_start_pos := p.tok.pos()
		mut array_decompose := false
		if p.tok.kind == .ellipsis {
			p.next()
			array_decompose = true
		}
		mut expr := ast.empty_expr
		if p.peek_tok.kind == .colon {
			// `foo(key:val, key2:val2)`
			expr = p.struct_init('void_type', .short_syntax, false)
		} else {
			expr = p.expr(0)
		}
		if array_decompose {
			expr = ast.ArrayDecompose{
				expr: expr
				pos:  p.tok.pos()
			}
		}
		if mut expr is ast.StructInit {
			expr.pre_comments << comments
			comments = []ast.Comment{}
		}
		pos := arg_start_pos.extend(p.prev_tok.pos())
		comments << p.eat_comments()
		args << ast.CallArg{
			is_mut:   is_mut
			share:    ast.sharetype_from_flags(is_shared, is_atomic)
			expr:     expr
			comments: comments
			pos:      pos
		}
		if p.tok.kind != .comma {
			break
		}
		p.next()
	}
	return args
}

struct ReceiverParsingInfo {
mut:
	name     string
	pos      token.Pos
	typ      ast.Type
	type_pos token.Pos
	is_mut   bool
	language ast.Language
}

fn (mut p Parser) fn_decl() ast.FnDecl {
	p.top_level_statement_start()
	start_pos := p.tok.pos()

	mut is_manualfree := p.is_manualfree
	mut is_deprecated := false
	mut is_direct_arr := false
	mut is_keep_alive := false
	mut is_exported := false
	mut is_unsafe := false
	mut is_must_use := false
	mut is_trusted := false
	mut is_noreturn := false
	mut is_ctor_new := false
	mut is_c2v_variadic := false
	mut is_c_extern := false
	mut is_markused := false
	mut is_expand_simple_interpolation := false
	mut comments := []ast.Comment{}
	fn_attrs := p.attrs
	p.attrs = []
	for fna in fn_attrs {
		match fna.name {
			'noreturn' {
				is_noreturn = true
			}
			'manualfree' {
				is_manualfree = true
			}
			'deprecated' {
				is_deprecated = true
			}
			'direct_array_access' {
				if !p.pref.force_bounds_checking {
					is_direct_arr = true
				}
			}
			'keep_args_alive' {
				is_keep_alive = true
			}
			'export' {
				is_exported = true
			}
			'wasm_export' {
				is_exported = true
			}
			'unsafe' {
				is_unsafe = true
			}
			'must_use' {
				is_must_use = true
			}
			'trusted' {
				is_trusted = true
			}
			'c2v_variadic' {
				is_c2v_variadic = true
			}
			'use_new' {
				is_ctor_new = true
			}
			'markused' {
				is_markused = true
			}
			'c_extern' {
				is_c_extern = true
			}
			'windows_stdcall' {
				p.note_with_pos('the tag [windows_stdcall] has been deprecated, it will be an error after 2022-06-01, use `[callconv: stdcall]` instead',
					p.tok.pos())
			}
			'_fastcall' {
				p.note_with_pos('the tag [_fastcall] has been deprecated, it will be an error after 2022-06-01, use `[callconv: fastcall]` instead',
					p.tok.pos())
			}
			'callconv' {
				if !fna.has_arg {
					p.error_with_pos('callconv attribute is present but its value is missing',
						p.prev_tok.pos())
				}
				if fna.arg !in ['stdcall', 'fastcall', 'cdecl'] {
					p.error_with_pos('unsupported calling convention, supported are stdcall, fastcall and cdecl',
						p.prev_tok.pos())
				}
			}
			'expand_simple_interpolation' {
				is_expand_simple_interpolation = true
			}
			else {}
		}
	}
	conditional_ctdefine_idx := fn_attrs.find_comptime_define() or { -1 }
	is_pub := p.tok.kind == .key_pub
	if is_pub {
		p.next()
	}
	p.check(.key_fn)
	comments << p.eat_comments()
	p.open_scope()
	defer {
		p.close_scope()
	}
	language_tok_pos := p.tok.pos()
	mut language := p.parse_language()
	p.fn_language = language
	if language != .v {
		for fna in fn_attrs {
			if fna.name == 'export' {
				p.error_with_pos('interop function cannot be exported', fna.pos)
				break
			}
		}
	}
	if is_keep_alive && language != .c {
		p.error_with_pos('attribute [keep_args_alive] is only supported for C functions',
			language_tok_pos)
	}
	if language != .v {
		p.check_for_impure_v(language, language_tok_pos)
		if language == .c {
			is_unsafe = !is_trusted
		}
	}
	// Receiver?
	mut rec := ReceiverParsingInfo{
		typ:      ast.void_type
		language: language
	}
	mut is_method := false
	mut is_static_type_method := false
	mut params := []ast.Param{}
	if p.tok.kind == .lpar {
		is_method = true
		p.fn_receiver(mut params, mut rec) or { return ast.FnDecl{
			scope: unsafe { nil }
		} }

		// rec.language was initialized with language variable.
		// So language is changed only if rec.language has been changed.
		language = rec.language
		p.fn_language = language
	}
	mut name := ''
	mut type_sym := p.table.sym(rec.typ)
	mut name_pos := p.tok.pos()
	mut static_type_pos := p.tok.pos()
	if p.tok.kind == .name || is_ident_name(p.tok.lit) {
		mut check_name := ''
		// TODO: high order fn
		is_static_type_method = p.tok.lit.len > 0 && p.tok.lit[0].is_capital()
			&& p.peek_tok.kind == .dot && language == .v // `fn Foo.bar() {}`
		if is_static_type_method {
			type_name := p.tok.lit // "Foo"
			static_type_pos = p.tok.pos()
			rec.typ = p.parse_type()
			p.check(.dot)
			check_name = p.check_name()
			name = type_name + '__static__' + check_name // "foo__bar"
			name_pos = name_pos.extend(p.prev_tok.pos())
		} else {
			check_name = if language == .js { p.check_js_name() } else { p.check_name() }
			name = check_name
		}
		if language == .v && !p.pref.translated && !p.is_translated
			&& util.contains_capital(check_name) && !p.builtin_mod {
			p.error_with_pos('function names cannot contain uppercase letters, use snake_case instead',
				name_pos)
			return ast.FnDecl{
				scope: unsafe { nil }
			}
		}
		if is_method {
			mut is_duplicate := type_sym.has_method(name)
			// make sure this is a normal method and not an interface method
			if type_sym.kind == .interface && is_duplicate {
				if mut type_sym.info is ast.Interface {
					// if the method is in info then its an interface method
					is_duplicate = !type_sym.info.has_method(name)
				}
			}
			if is_duplicate {
				if type_sym.kind == .enum
					&& name in ['is_empty', 'has', 'all', 'set', 'set_all', 'clear', 'clear_all', 'toggle', 'zero', 'from'] {
					if enum_fn := type_sym.find_method(name) {
						name_pos = enum_fn.name_pos
					}
					p.error_with_pos('duplicate method `${name}`, `${name}` is an enum type built-in method',
						name_pos)
				} else {
					p.error_with_pos('duplicate method `${name}`', name_pos)
				}
				return ast.FnDecl{
					scope: unsafe { nil }
				}
			}
		}
		if !p.pref.is_fmt {
			if name in p.imported_symbols {
				p.error_with_pos('cannot redefine imported function `${name}`', name_pos)
				return ast.FnDecl{
					scope: unsafe { nil }
				}
			}
		}
	} else if p.tok.kind in [.plus, .minus, .mul, .div, .mod, .lt, .eq] && p.peek_tok.kind == .lpar {
		name = p.tok.kind.str() // op_to_fn_name()
		if rec.typ == ast.void_type {
			p.error_with_pos('cannot use operator overloading with normal functions',
				p.tok.pos())
		}
		if type_sym.has_method(name) {
			p.error_with_pos('cannot duplicate operator overload `${name}`', p.tok.pos())
		}
		p.next()
	} else if p.tok.kind in [.ne, .gt, .ge, .le] && p.peek_tok.kind == .lpar {
		p.error_with_pos('cannot overload `!=`, `>`, `<=` and `>=` as they are auto generated from `==` and`<`',
			p.tok.pos())
	} else if p.tok.kind in [.plus_assign, .minus_assign, .div_assign, .mult_assign, .mod_assign] {
		extracted_op := match p.tok.kind {
			.plus_assign { '+' }
			.minus_assign { '-' }
			.div_assign { '/' }
			.mod_assign { '%' }
			.mult_assign { '*' }
			else { 'unknown op' }
		}
		if type_sym.has_method(extracted_op) {
			p.error('cannot overload `${p.tok.kind}`, operator is implicitly overloaded because the `${extracted_op}` operator is overloaded')
		}
		p.error('cannot overload `${p.tok.kind}`, overload `${extracted_op}` and `${p.tok.kind}` will be automatically generated')
	} else {
		p.error_with_pos('expecting method name', p.tok.pos())
		return ast.FnDecl{
			scope: unsafe { nil }
		}
	}
	// [T]
	_, mut generic_names := p.parse_generic_types()
	// generic names can be infer with receiver's generic names
	if is_method && rec.typ.has_flag(.generic) {
		sym := p.table.sym(rec.typ)
		if sym.info is ast.Struct {
			decl_generic_names := p.types_to_names(sym.info.generic_types, p.tok.pos(),
				'sym.info.generic_types') or { return ast.FnDecl{
				scope: unsafe { nil }
			} }
			fn_generic_names := generic_names.clone()
			generic_names = p.table.generic_type_names(rec.typ)
			if decl_generic_names.len != generic_names.len {
				plural := if decl_generic_names.len == 1 { '' } else { 's' }
				p.error_with_pos('expected ${decl_generic_names.len} generic parameter${plural}, got ${generic_names.len}',
					rec.type_pos)
			}
			for gname in fn_generic_names {
				if gname !in generic_names {
					generic_names << gname
				}
			}
		}
	}
	// Params
	params_t, are_params_type_only, mut is_variadic, mut is_c_variadic := p.fn_params()
	if is_c2v_variadic {
		is_variadic = true
	}
	params << params_t
	// Return type
	mut return_type_pos := p.tok.pos()
	mut return_type := ast.void_type
	// don't confuse token on the next line: fn decl, [attribute]
	same_line := p.tok.line_nr == p.prev_tok.line_nr
	if (p.tok.kind.is_start_of_type() && (same_line || p.tok.kind != .lsbr))
		|| (same_line && p.tok.kind == .key_fn) {
		p.inside_fn_return = true
		return_type = p.parse_type()
		p.inside_fn_return = false
		return_type_pos = return_type_pos.extend(p.prev_tok.pos())

		if p.tok.kind in [.question, .not] {
			ret_type_sym := p.table.sym(return_type)
			p.error_with_pos('wrong syntax, it must be ${p.tok.kind}${ret_type_sym.name}, not ${ret_type_sym.name}${p.tok.kind}',
				return_type_pos)
		}
	}

	if p.tok.kind == .comma {
		mr_pos := return_type_pos.extend(p.peek_tok.pos())
		p.error_with_pos('multiple return types in function declaration must use parentheses, e.g. (int, string)',
			mr_pos)
	}
	mut type_sym_method_idx := 0
	no_body := p.tok.kind != .lcbr
	end_pos := p.prev_tok.pos()
	short_fn_name := name
	is_main := short_fn_name == 'main' && p.mod == 'main'
	if is_main {
		p.main_already_defined = true
	}
	is_test := (!is_method && params.len == 0) && p.inside_test_file
		&& (short_fn_name.starts_with('test_') || short_fn_name.starts_with('testsuite_'))
	file_mode := p.file_backend_mode
	if is_main {
		if _ := p.table.find_fn('main.main') {
			if '.' in os.args {
				p.error_with_pos('multiple `main` functions detected, and you ran `v .`
perhaps there are multiple V programs in this directory, and you need to
run them via `v file.v` instead',
					name_pos)
			}
		}
	}
	if is_method && is_static_type_method {
		p.error_with_pos('cannot declare a static function as a receiver method', name_pos)
	}
	// Register
	if !are_params_type_only {
		for k, param in params {
			if p.scope.known_var(param.name) {
				p.error_with_pos('redefinition of parameter `${param.name}`', param.pos)
				return ast.FnDecl{
					scope: unsafe { nil }
				}
			}
			is_stack_obj := !param.typ.has_flag(.shared_f) && (param.is_mut || param.typ.is_ptr())
			p.scope.register(ast.Var{
				name:          param.name
				typ:           param.typ
				is_mut:        param.is_mut
				is_auto_deref: param.is_mut
				is_stack_obj:  is_stack_obj
				pos:           param.pos
				is_used:       is_pub || no_body || (is_method && k == 0) || p.builtin_mod
				is_arg:        true
				ct_type_var:   if (!is_method || k >= 0) && param.typ.has_flag(.generic)
					&& !param.typ.has_flag(.variadic) {
					.generic_param
				} else {
					.no_comptime
				}
			})
		}
	}
	if is_method {
		// Do not allow to modify / add methods to types from other modules
		// arrays/maps dont belong to a module only their element types do
		// we could also check if kind is .array,  .array_fixed, .map instead of mod.len
		mut is_non_local := type_sym.mod.len > 0 && type_sym.mod != p.mod && type_sym.language == .v
		// check maps & arrays, must be defined in same module as the elem type
		if !is_non_local && !(p.builtin_mod && p.pref.is_fmt) && type_sym.kind in [.array, .map] {
			elem_type_sym := p.table.sym(p.table.value_type(rec.typ))
			is_non_local = elem_type_sym.mod.len > 0 && elem_type_sym.mod != p.mod
				&& elem_type_sym.language == .v
		}
		if is_non_local {
			p.error_with_pos('cannot define new methods on non-local type ${type_sym.name}. Define an alias and use that instead like `type AliasName = ${type_sym.name}` ',
				rec.type_pos)
			return ast.FnDecl{
				scope: unsafe { nil }
			}
		}
		type_sym_method_idx = type_sym.register_method(ast.Fn{
			name:          name
			file_mode:     file_mode
			params:        params
			return_type:   return_type
			is_variadic:   is_variadic
			generic_names: generic_names
			is_pub:        is_pub
			is_deprecated: is_deprecated
			is_noreturn:   is_noreturn
			is_unsafe:     is_unsafe
			is_must_use:   is_must_use
			is_main:       is_main
			is_test:       is_test
			is_keep_alive: is_keep_alive
			is_method:     true
			receiver_type: rec.typ
			//
			attrs:          fn_attrs
			is_conditional: conditional_ctdefine_idx != ast.invalid_type_idx
			ctdefine_idx:   conditional_ctdefine_idx
			//
			no_body:  no_body
			mod:      p.mod
			file:     p.file_path
			pos:      start_pos
			name_pos: name_pos
			language: language
			//
			is_expand_simple_interpolation: is_expand_simple_interpolation
		})
	} else {
		name = match language {
			.c { 'C.${name}' }
			.js { 'JS.${name}' }
			.wasm { 'WASM.${name}' }
			else { p.prepend_mod(name) }
		}
		if !p.pref.translated && language == .v {
			if existing := p.table.fns[name] {
				if existing.name != '' {
					if file_mode == .v && existing.file_mode != .v {
						// a definition made in a .c.v file, should have a priority over a .v file definition of the same function
						if !p.pref.is_fmt {
							name = p.prepend_mod('pure_v_but_overridden_by_${existing.file_mode}_${short_fn_name}')
						}
					} else {
						p.table.redefined_fns << name
					}
				}
			}
		}
		p.table.register_fn(ast.Fn{
			name:                  name
			file_mode:             file_mode
			params:                params
			return_type:           return_type
			is_variadic:           is_variadic
			is_c_variadic:         is_c_variadic
			generic_names:         generic_names
			is_pub:                is_pub
			is_deprecated:         is_deprecated
			is_noreturn:           is_noreturn
			is_ctor_new:           is_ctor_new
			is_unsafe:             is_unsafe
			is_must_use:           is_must_use
			is_main:               is_main
			is_test:               is_test
			is_keep_alive:         is_keep_alive
			is_method:             false
			is_static_type_method: is_static_type_method
			receiver_type:         if is_static_type_method { rec.typ } else { 0 } // used only if is static type method
			is_file_translated:    p.is_translated
			//
			attrs:          fn_attrs
			is_conditional: conditional_ctdefine_idx != ast.invalid_type_idx
			ctdefine_idx:   conditional_ctdefine_idx
			//
			no_body:  no_body
			mod:      p.mod
			file:     p.file_path
			pos:      start_pos
			name_pos: name_pos
			language: language
			//
			is_expand_simple_interpolation: is_expand_simple_interpolation
		})
	}
	/*
	// Register implicit context var
	p.scope.register(ast.Var{
		name: 'ctx'
		typ: ast.error_type
		pos: p.tok.pos()
		is_used: true
		is_stack_obj: true
	})
	*/
	// Body
	p.cur_fn_name = name
	mut stmts := []ast.Stmt{}
	body_start_pos := p.tok.pos()
	if p.tok.kind == .lcbr {
		if language != .v && !(language == .js && type_sym.info is ast.Interface) {
			p.error_with_pos('interop functions cannot have a body', body_start_pos)
		}
		last_fn_scope := p.scope
		p.inside_fn = true
		p.inside_unsafe_fn = is_unsafe
		p.cur_fn_scope = p.scope
		stmts = p.parse_block_no_scope(true)
		p.cur_fn_scope = last_fn_scope
		p.inside_unsafe_fn = false
		p.inside_fn = false
	}
	if !no_body && are_params_type_only {
		p.error_with_pos('functions with type only params can not have bodies', body_start_pos)
		return ast.FnDecl{
			scope: unsafe { nil }
		}
	}
	// if no_body && !name.starts_with('C.') {
	// 	p.error_with_pos('did you mean C.$name instead of $name', start_pos)
	// }
	fn_decl := ast.FnDecl{
		name:               name
		short_name:         short_fn_name
		mod:                p.mod
		stmts:              stmts
		return_type:        return_type
		return_type_pos:    return_type_pos
		params:             params
		is_noreturn:        is_noreturn
		is_manualfree:      is_manualfree
		is_deprecated:      is_deprecated
		is_exported:        is_exported
		is_direct_arr:      is_direct_arr
		is_pub:             is_pub
		is_variadic:        is_variadic
		is_c_variadic:      is_c_variadic
		is_c_extern:        is_c_extern
		is_main:            is_main
		is_test:            is_test
		is_keep_alive:      is_keep_alive
		is_unsafe:          is_unsafe
		is_must_use:        is_must_use
		is_markused:        is_markused
		is_file_translated: p.is_translated
		//
		attrs:          fn_attrs
		is_conditional: conditional_ctdefine_idx != ast.invalid_type_idx
		ctdefine_idx:   conditional_ctdefine_idx
		//
		receiver:              ast.StructField{
			name: rec.name
			typ:  rec.typ
		}
		generic_names:         generic_names
		receiver_pos:          rec.pos
		is_method:             is_method
		is_static_type_method: is_static_type_method
		static_type_pos:       static_type_pos
		method_type_pos:       rec.type_pos
		method_idx:            type_sym_method_idx
		rec_mut:               rec.is_mut
		language:              language
		no_body:               no_body
		pos:                   start_pos.extend_with_last_line(end_pos, p.prev_tok.line_nr)
		end_pos:               p.tok.pos()
		name_pos:              name_pos
		body_pos:              body_start_pos
		file:                  p.file_path
		is_builtin:            p.builtin_mod || p.mod in util.builtin_module_parts
		scope:                 p.scope
		label_names:           p.label_names
		end_comments:          p.eat_comments(same_line: true)
		comments:              comments
		//
		is_expand_simple_interpolation: is_expand_simple_interpolation
	}
	if generic_names.len > 0 {
		p.table.register_fn_generic_types(fn_decl.fkey())
	}
	p.label_names = []
	return fn_decl
}

fn (mut p Parser) fn_receiver(mut params []ast.Param, mut rec ReceiverParsingInfo) ! {
	p.inside_receiver_param = true
	defer {
		p.inside_receiver_param = false
	}
	lpar_pos := p.tok.pos()
	p.next() // (
	is_shared := p.tok.kind == .key_shared
	is_atomic := p.tok.kind == .key_atomic
	rec.is_mut = p.tok.kind == .key_mut || is_shared || is_atomic
	if rec.is_mut {
		p.next() // `mut`
	}
	if is_shared {
		p.register_auto_import('sync')
	}
	rec_start_pos := p.tok.pos()
	rec.name = p.check_name()
	if !rec.is_mut {
		rec.is_mut = p.tok.kind == .key_mut
		if rec.is_mut {
			ptoken2 := p.peek_token(2) // needed to prevent codegen bug, where .pos() expects &Token
			p.warn_with_pos('use `(mut f Foo)` instead of `(f mut Foo)`', lpar_pos.extend(ptoken2.pos()))
		}
	}
	if p.tok.kind == .key_shared {
		ptoken2 := p.peek_token(2) // needed to prevent codegen bug, where .pos() expects &Token
		p.error_with_pos('use `(shared f Foo)` instead of `(f shared Foo)`', lpar_pos.extend(ptoken2.pos()))
	}
	rec.pos = rec_start_pos.extend(p.tok.pos())
	is_amp := p.tok.kind == .amp
	if p.tok.kind == .name && p.tok.lit == 'JS' {
		rec.language = ast.Language.js
	}
	// if rec.is_mut {
	// p.check(.key_mut)
	// }
	// TODO: talk to alex, should mut be parsed with the type like this?
	// or should it be a property of the arg, like this ptr/mut becomes indistinguishable
	rec.type_pos = p.tok.pos()
	rec.typ = p.parse_type_with_mut(rec.is_mut)
	if rec.typ.idx() == 0 {
		// error is set in parse_type
		return error('void receiver type')
	}
	rec.type_pos = rec.type_pos.extend(p.prev_tok.pos())
	if is_amp && rec.is_mut {
		p.error_with_pos('use `(mut f Foo)` or `(f &Foo)` instead of `(mut f &Foo)`',
			lpar_pos.extend(p.tok.pos()))
		return error('invalid `mut f &Foo`')
	}
	if is_shared {
		rec.typ = rec.typ.set_flag(.shared_f)
	}
	if is_atomic {
		rec.typ = rec.typ.set_flag(.atomic_f)
	}

	if rec.language != .v {
		p.check_for_impure_v(rec.language, rec.type_pos)
	}

	p.check(.rpar)

	params << ast.Param{
		pos:       rec_start_pos
		name:      rec.name
		is_mut:    rec.is_mut
		is_atomic: is_atomic
		is_shared: is_shared
		typ:       rec.typ
		type_pos:  rec.type_pos
	}
}

fn (mut p Parser) anon_fn() ast.AnonFn {
	pos := p.tok.pos()
	p.check(.key_fn)
	if p.tok.kind == .name {
		if p.disallow_declarations_in_script_mode() {
			return ast.AnonFn{}
		}
	}
	old_inside_defer := p.inside_defer
	p.inside_defer = false
	p.open_scope()
	defer {
		p.close_scope()
	}
	p.scope.detached_from_parent = true
	inherited_vars := if p.tok.kind == .lsbr && !(p.peek_tok.kind == .name
		&& p.peek_tok.lit.len == 1 && p.peek_tok.lit[0].is_capital()) {
		p.closure_vars()
	} else {
		[]ast.Param{}
	}
	inherited_vars_name := inherited_vars.map(it.name)
	_, generic_names := p.parse_generic_types()
	params, _, is_variadic, _ := p.fn_params()
	for param in params {
		if param.name == '' && p.table.sym(param.typ).kind != .placeholder {
			p.error_with_pos('use `_` to name an unused parameter', param.pos)
		}
		if param.name in inherited_vars_name {
			p.error_with_pos('the parameter name `${param.name}` conflicts with the captured value name',
				param.pos)
		} else if p.scope.known_var(param.name) {
			p.error_with_pos('redefinition of parameter `${param.name}`', param.pos)
		}
		is_stack_obj := !param.typ.has_flag(.shared_f) && (param.is_mut || param.typ.is_ptr())
		p.scope.register(ast.Var{
			name:          param.name
			typ:           param.typ
			is_mut:        param.is_mut
			is_auto_deref: param.is_mut
			pos:           param.pos
			is_used:       true
			is_arg:        true
			is_stack_obj:  is_stack_obj
		})
	}
	mut same_line := p.tok.line_nr == p.prev_tok.line_nr
	mut return_type := ast.void_type
	mut return_type_pos := p.tok.pos()
	// lpar: multiple return types
	if same_line {
		if (p.tok.kind.is_start_of_type() && (same_line || p.tok.kind != .lsbr))
			|| (same_line && p.tok.kind == .key_fn) {
			p.inside_fn_return = true
			return_type = p.parse_type()
			p.inside_fn_return = false
			return_type_pos = return_type_pos.extend(p.tok.pos())
		} else if p.tok.kind != .lcbr {
			p.error_with_pos('expected return type, not ${p.tok} for anonymous function',
				p.tok.pos())
		}
	}
	mut stmts := []ast.Stmt{}
	no_body := p.tok.kind != .lcbr
	same_line = p.tok.line_nr == p.prev_tok.line_nr
	if no_body && same_line {
		p.unexpected(got: '${p.tok} after anonymous function signature', expecting: '`{`')
	}
	mut label_names := []string{}
	mut func := ast.Fn{
		params:      params
		is_variadic: is_variadic
		return_type: return_type
		is_method:   false
	}
	name := p.table.get_anon_fn_name(p.unique_prefix, func, p.tok.pos)
	keep_fn_name := p.cur_fn_name
	p.cur_fn_name = name
	if p.tok.kind == .lcbr {
		tmp := p.label_names
		p.label_names = []
		old_assign_rhs := p.inside_assign_rhs
		p.inside_assign_rhs = false
		stmts = p.parse_block_no_scope(false)
		p.inside_assign_rhs = old_assign_rhs
		label_names = p.label_names.clone()
		p.label_names = tmp
	}
	p.cur_fn_name = keep_fn_name
	func.name = name
	idx := p.table.find_or_register_fn_type(func, true, false)
	typ := if generic_names.len > 0 {
		ast.new_type(idx).set_flag(.generic)
	} else {
		ast.new_type(idx)
	}
	p.inside_defer = old_inside_defer
	// name := p.table.get_type_name(typ)
	return ast.AnonFn{
		decl:           ast.FnDecl{
			name:            name
			short_name:      ''
			mod:             p.mod
			stmts:           stmts
			return_type:     return_type
			return_type_pos: return_type_pos
			params:          params
			is_variadic:     is_variadic
			is_method:       false
			generic_names:   generic_names
			is_anon:         true
			no_body:         no_body
			pos:             pos.extend(p.prev_tok.pos())
			file:            p.file_path
			scope:           p.scope
			label_names:     label_names
		}
		inherited_vars: inherited_vars
		typ:            typ
	}
}

// part of fn declaration
// returns: params, are_params_type_only, mut is_variadic, mut is_c_variadic
fn (mut p Parser) fn_params() ([]ast.Param, bool, bool, bool) {
	p.check(.lpar)
	mut params := []ast.Param{}
	mut is_variadic := false
	mut is_c_variadic := false
	// `int, int, string` (no names, just types)
	param_name := if p.tok.kind == .name && p.tok.lit.len > 0 && p.tok.lit[0].is_capital() {
		p.prepend_mod(p.tok.lit)
	} else {
		p.tok.lit
	}
	is_generic_type := p.tok.kind == .name && p.tok.lit.len == 1 && p.tok.lit[0].is_capital()

	types_only := p.tok.kind in [.question, .not, .amp, .ellipsis, .key_fn, .lsbr]
		|| (p.peek_tok.kind == .comma && (p.table.known_type(param_name) || is_generic_type))
		|| p.peek_tok.kind == .dot || p.peek_tok.kind == .rpar || p.fn_language == .c
		|| (p.tok.kind == .key_mut && (p.peek_tok.kind in [.amp, .ellipsis, .key_fn, .lsbr]
		|| (p.peek_token(2).kind == .comma && (p.tok.kind != .key_mut
		|| p.peek_tok.lit[0].is_capital())) || p.peek_token(2).kind == .rpar
		|| (p.peek_tok.kind == .name && p.peek_token(2).kind == .dot)))
	mut prev_param_newline := p.tok.pos().line_nr
	// TODO: copy paste, merge 2 branches
	if types_only {
		mut param_no := 1
		for p.tok.kind != .rpar {
			if p.tok.kind == .eof {
				p.error_with_pos('expecting `)`', p.tok.pos())
				return []ast.Param{}, false, false, false
			}
			is_shared := p.tok.kind == .key_shared
			is_atomic := p.tok.kind == .key_atomic
			is_mut := p.tok.kind == .key_mut || is_shared || is_atomic
			mut name := ''
			if is_mut {
				p.next()
			}
			if p.fn_language == .c && p.tok.kind == .name
				&& p.peek_tok.kind !in [.comma, .rpar, .dot] {
				name = p.tok.lit
				p.next()
			}
			if p.tok.kind == .ellipsis {
				p.next()
				is_variadic = true
				is_c_variadic = p.tok.kind == .rpar
				if is_c_variadic {
					p.check(.rpar)
					return params, types_only, is_variadic, is_c_variadic
				}
			}
			pos := p.tok.pos()
			mut param_type := p.parse_type()
			type_pos := pos.extend(p.prev_tok.pos())
			if param_type == 0 {
				// error is added in parse_type
				return []ast.Param{}, false, false, false
			}
			if is_mut {
				if !param_type.has_flag(.generic) {
					if is_variadic {
						p.error_with_pos('variadic arguments cannot be `mut`, `shared` or `atomic`',
							pos)
					}
					if is_shared {
						p.check_fn_shared_arguments(param_type, pos)
					} else if is_atomic {
						p.check_fn_atomic_arguments(param_type, pos)
					} else {
						p.check_fn_mutable_arguments(param_type, pos)
					}
				} else if is_shared || is_atomic {
					p.error_with_pos('generic object cannot be `atomic`or `shared`', pos)
					return []ast.Param{}, false, false, false
				}
				if param_type.is_ptr() && p.table.sym(param_type).kind == .struct {
					param_type = param_type.ref()
				} else {
					param_type = param_type.set_nr_muls(1)
				}
				if param_type.has_flag(.option) {
					param_type = param_type.set_flag(.option_mut_param_t)
				}
				if is_shared {
					param_type = param_type.set_flag(.shared_f)
				}
				if is_atomic {
					param_type = param_type.set_flag(.atomic_f)
				}
			}
			if is_variadic {
				param_type = ast.new_type(p.table.find_or_register_array(param_type)).set_flag(.variadic)
			}
			if p.tok.kind == .eof {
				p.error_with_pos('expecting `)`', p.prev_tok.pos())
				return []ast.Param{}, false, false, false
			}

			if p.tok.kind == .comma {
				if is_variadic {
					p.error_with_pos('cannot use ...(variadic) with non-final parameter no ${param_no}',
						pos)
					return []ast.Param{}, false, false, false
				}
				p.next()
			}
			alanguage := p.table.sym(param_type).language
			if alanguage != .v {
				p.check_for_impure_v(alanguage, pos)
			}
			params << ast.Param{
				pos:        pos
				name:       name
				is_mut:     is_mut
				typ:        param_type
				type_pos:   type_pos
				on_newline: prev_param_newline != pos.line_nr
			}
			prev_param_newline = pos.line_nr
			param_no++
			if param_no > 1024 {
				p.error_with_pos('too many parameters', pos)
				return []ast.Param{}, false, false, false
			}
		}
	} else {
		for p.tok.kind != .rpar {
			if p.tok.kind == .eof {
				p.error_with_pos('expecting `)`', p.tok.pos())
				return []ast.Param{}, false, false, false
			}
			is_shared := p.tok.kind == .key_shared
			is_atomic := p.tok.kind == .key_atomic
			mut is_mut := p.tok.kind == .key_mut || is_shared || is_atomic
			if is_mut {
				p.next()
			}
			if p.tok.kind == .ellipsis && p.peek_tok.kind == .rpar {
				p.check(.ellipsis)
				p.check(.rpar)
				return params, types_only, true, true
			}

			mut param_pos := [p.tok.pos()]
			name := p.check_name()
			mut param_names := [name]
			if name != '' && p.fn_language == .v && name[0].is_capital() {
				p.error_with_pos('parameter name must not begin with upper case letter (`${param_names[0]}`)',
					p.prev_tok.pos())
			}
			mut type_pos := [p.tok.pos()]
			// `a, b, c int`
			for p.tok.kind == .comma {
				if !p.pref.is_fmt {
					p.error('`fn f(x, y Type)` syntax has been deprecated. ' +
						'Use `fn f(x Type, y Type)` instead. You can run `v fmt -w "${p.scanner.file_path}"` to automatically fix your code.')
				}
				p.next()
				param_pos << p.tok.pos()
				param_names << p.check_name()
				type_pos << p.tok.pos()
			}
			if p.tok.kind == .key_mut {
				// TODO: remove old syntax
				if !p.pref.is_fmt {
					p.warn_with_pos('use `mut f Foo` instead of `f mut Foo`', p.tok.pos())
				}
				is_mut = true
			}
			if p.tok.kind == .key_shared {
				p.error_with_pos('use `shared f Foo` instead of `f shared Foo`', p.tok.pos())
			}
			if p.tok.kind == .ellipsis {
				p.next()
				is_variadic = true
				is_c_variadic = p.tok.kind == .rpar
				if is_c_variadic {
					p.check(.rpar)
					return params, types_only, is_variadic, is_c_variadic
				}
			}
			pos := p.tok.pos()
			mut typ := p.parse_type()
			type_pos[0] = pos.extend(p.prev_tok.pos())
			if typ == 0 {
				// error is added in parse_type
				return []ast.Param{}, false, false, false
			}
			if is_mut {
				if !typ.has_flag(.generic) {
					if is_variadic {
						p.error_with_pos('variadic arguments cannot be `mut`, `shared` or `atomic`',
							pos)
					}
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
					return []ast.Param{}, false, false, false
				}
				if typ.is_ptr() && p.table.sym(typ).kind == .struct {
					typ = typ.ref()
				} else {
					typ = typ.set_nr_muls(1)
				}
				if typ.has_flag(.option) {
					typ = typ.set_flag(.option_mut_param_t)
				}
				if is_shared {
					typ = typ.set_flag(.shared_f)
				}
				if is_atomic {
					typ = typ.set_flag(.atomic_f)
				}
			}
			if is_variadic {
				// derive flags, however nr_muls only needs to be set on the array elem type, so clear it on the arg type
				typ = ast.new_type(p.table.find_or_register_array(typ)).derive(typ).set_nr_muls(0).set_flag(.variadic)
			}
			for i, para_name in param_names {
				alanguage := p.table.sym(typ).language
				if alanguage != .v {
					p.check_for_impure_v(alanguage, type_pos[i])
				}
				params << ast.Param{
					pos:        param_pos[i]
					name:       para_name
					is_mut:     is_mut
					is_atomic:  is_atomic
					is_shared:  is_shared
					typ:        typ
					type_pos:   type_pos[i]
					on_newline: prev_param_newline != param_pos[i].line_nr
				}
				prev_param_newline = param_pos[i].line_nr
				// if typ.typ.kind == .variadic && p.tok.kind == .comma {
				if is_variadic && p.tok.kind == .comma && p.peek_tok.kind != .rpar {
					p.error_with_pos('cannot use ...(variadic) with non-final parameter ${para_name}',
						param_pos[i])
					return []ast.Param{}, false, false, false
				}
			}
			if p.tok.kind == .eof {
				p.error_with_pos('expecting `)`', p.prev_tok.pos())
				return []ast.Param{}, false, false, false
			}
			if p.tok.kind != .rpar {
				p.check(.comma)
			}
		}
	}
	p.check(.rpar)
	return params, types_only, is_variadic, is_c_variadic
}

fn (mut p Parser) spawn_expr() ast.SpawnExpr {
	p.next()
	spos := p.tok.pos()
	old_inside_assign_rhs := p.inside_assign_rhs
	p.inside_assign_rhs = false
	expr := p.expr(0)
	p.inside_assign_rhs = old_inside_assign_rhs
	mut call_expr := if expr is ast.CallExpr {
		expr
	} else {
		p.error_with_pos('expression in `spawn` must be a function call', expr.pos())
		ast.CallExpr{
			scope: p.scope
		}
	}
	call_expr.is_return_used = true
	pos := spos.extend(p.prev_tok.pos())
	p.register_auto_import('sync.threads')
	p.table.gostmts++
	return ast.SpawnExpr{
		call_expr: call_expr
		pos:       pos
	}
}

fn (mut p Parser) go_expr() ast.GoExpr {
	p.next()
	spos := p.tok.pos()
	old_inside_assign_rhs := p.inside_assign_rhs
	p.inside_assign_rhs = false
	expr := p.expr(0)
	p.inside_assign_rhs = old_inside_assign_rhs
	mut call_expr := if expr is ast.CallExpr {
		expr
	} else {
		p.error_with_pos('expression in `go` must be a function call', expr.pos())
		ast.CallExpr{
			scope: p.scope
		}
	}
	call_expr.is_return_used = true
	pos := spos.extend(p.prev_tok.pos())
	// p.register_auto_import('coroutines')
	p.table.gostmts++
	return ast.GoExpr{
		call_expr: call_expr
		pos:       pos
	}
}

fn (mut p Parser) closure_vars() []ast.Param {
	p.check(.lsbr)
	mut vars := []ast.Param{cap: 5}
	for {
		is_shared := p.tok.kind == .key_shared
		is_atomic := p.tok.kind == .key_atomic
		is_mut := p.tok.kind == .key_mut || is_shared || is_atomic
		// FIXME: is_shared & is_atomic aren't used further
		if is_mut {
			p.next()
		}
		var_pos := p.tok.pos()
		p.check(.name)
		var_name := p.prev_tok.lit
		mut var := p.scope.parent.find_var(var_name) or {
			if p.table.global_scope.known_global(var_name) {
				p.error_with_pos('no need to capture global variable `${var_name}` in closure',
					p.prev_tok.pos())
				return []
			}
			p.error_with_pos('undefined ident: `${var_name}`', p.prev_tok.pos())
			return []
		}
		var.is_used = true
		if is_mut {
			var.is_changed = true
		}
		p.scope.register(ast.Var{
			...(*var)
			pos:           var_pos
			is_inherited:  true
			has_inherited: var.is_inherited
			is_used:       false
			is_changed:    false
			is_mut:        is_mut
		})
		vars << ast.Param{
			pos:       var_pos
			name:      var_name
			is_mut:    is_mut
			is_atomic: is_atomic
			is_shared: is_shared
		}
		if p.tok.kind != .comma {
			break
		}
		p.next()
	}
	p.check(.rsbr)
	return vars
}

fn (mut p Parser) check_fn_mutable_arguments(typ ast.Type, pos token.Pos) {
	sym := p.table.sym(typ)
	if sym.kind in [.array, .array_fixed, .interface, .map, .placeholder, .struct, .generic_inst,
		.sum_type] {
		return
	}
	if typ.is_any_kind_of_pointer() {
		return
	}
	if sym.kind == .alias {
		atyp := (sym.info as ast.Alias).parent_type
		p.check_fn_mutable_arguments(atyp, pos)
		return
	}
	if p.fn_language == .c {
		return
	}
	p.error_with_pos(
		'mutable arguments are only allowed for arrays, interfaces, maps, pointers, structs or their aliases\n' +
		'return values instead: `fn foo(mut n ${sym.name}) {` => `fn foo(n ${sym.name}) ${sym.name} {`',
		pos)
}

fn (mut p Parser) check_fn_shared_arguments(typ ast.Type, pos token.Pos) {
	mut sym := p.table.sym(typ)
	if sym.kind == .generic_inst {
		sym = p.table.type_symbols[(sym.info as ast.GenericInst).parent_idx]
	}
	if sym.kind !in [.array, .struct, .map, .placeholder] && !typ.is_ptr() {
		p.error_with_pos('shared arguments are only allowed for arrays, maps, and structs\n',
			pos)
	}
}

fn (mut p Parser) check_fn_atomic_arguments(typ ast.Type, pos token.Pos) {
	sym := p.table.sym(typ)
	if sym.kind !in [.u32, .int, .u64] {
		p.error_with_pos('atomic arguments are only allowed for 32/64 bit integers\n' +
			'use shared arguments instead: `fn foo(atomic n ${sym.name}) {` => `fn foo(shared n ${sym.name}) {`',
			pos)
	}
}

fn have_fn_main(stmts []ast.Stmt) bool {
	for stmt in stmts {
		if stmt is ast.FnDecl {
			if stmt.name == 'main.main' {
				return true
			}
		}
	}
	return false
}
