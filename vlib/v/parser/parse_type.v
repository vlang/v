// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.transformer
import v.ast
import v.util
import v.token

const maximum_inline_sum_type_variants = 3

fn (mut p Parser) parse_array_type(expecting token.Kind, is_option bool) ast.Type {
	p.check(expecting)
	// fixed array
	if p.tok.kind in [.number, .name] {
		mut fixed_size := 0
		mut size_expr := p.expr(0)
		mut size_unresolved := true
		if p.pref.is_fmt {
			fixed_size = 987654321
		} else {
			match mut size_expr {
				ast.IntegerLiteral {
					fixed_size = size_expr.val.int()
					size_unresolved = false
				}
				ast.Ident {
					if mut const_field := p.table.global_scope.find_const('${p.mod}.${size_expr.name}') {
						if mut const_field.expr is ast.IntegerLiteral {
							fixed_size = const_field.expr.val.int()
							size_unresolved = false
						} else if mut const_field.expr is ast.InfixExpr {
							mut t := transformer.new_transformer_with_table(p.table, p.pref)
							folded_expr := t.infix_expr(mut const_field.expr)

							if folded_expr is ast.IntegerLiteral {
								fixed_size = folded_expr.val.int()
								size_unresolved = false
							}
						}
					} else {
						if p.pref.is_fmt {
							// for vfmt purposes, pretend the constant does exist
							// it may have been defined in another .v file:
							fixed_size = 1
							size_unresolved = false
						}
					}
				}
				ast.InfixExpr {
					mut t := transformer.new_transformer_with_table(p.table, p.pref)
					folded_expr := t.infix_expr(mut size_expr)

					if folded_expr is ast.IntegerLiteral {
						fixed_size = folded_expr.val.int()
						size_unresolved = false
					}
				}
				else {
					p.error_with_pos('fixed array size cannot use non-constant value',
						size_expr.pos())
				}
			}
		}
		p.check(.rsbr)
		p.fixed_array_dim++
		defer {
			p.fixed_array_dim--
		}
		elem_type := p.parse_type()
		if elem_type.idx() == 0 {
			// error is handled by parse_type
			return 0
		}
		// has been explicitly resolved, but size is 0
		if fixed_size <= 0 && !size_unresolved {
			p.error_with_pos('fixed size cannot be zero or negative', size_expr.pos())
		}
		idx := p.table.find_or_register_array_fixed(elem_type, fixed_size, size_expr,
			p.fixed_array_dim == 1 && !is_option && p.inside_fn_return)
		if elem_type.has_flag(.generic) {
			return ast.new_type(idx).set_flag(.generic)
		}
		return ast.new_type(idx)
	}
	// array
	p.check(.rsbr)
	elem_type := p.parse_type()
	if elem_type.idx() == 0 {
		// error is set in parse_type
		return 0
	}
	if elem_type.idx() == ast.thread_type_idx {
		p.register_auto_import('sync.threads')
	}
	mut nr_dims := 1
	// detect attr
	not_attr := p.peek_tok.kind != .name && p.peek_token(2).kind !in [.semicolon, .rsbr]
	for p.tok.kind == expecting && not_attr {
		p.next()
		p.check(.rsbr)
		nr_dims++
	}
	idx := p.table.find_or_register_array_with_dims(elem_type, nr_dims)
	if elem_type.has_flag(.generic) {
		return ast.new_type(idx).set_flag(.generic)
	}
	return ast.new_type(idx)
}

fn (mut p Parser) parse_map_type() ast.Type {
	is_option := p.tok.kind == .question && p.peek_tok.kind == .name // option map
	if is_option {
		p.next()
	}
	p.next()
	if p.tok.kind != .lsbr {
		return ast.map_type
	}
	p.check(.lsbr)
	key_type := p.parse_type()
	if key_type.idx() == 0 {
		// error is reported in parse_type
		return 0
	}
	key_sym := p.table.sym(key_type)
	is_alias := key_sym.kind == .alias
	key_type_supported := key_type in [ast.string_type_idx, ast.voidptr_type_idx]
		|| key_sym.kind in [.enum_, .placeholder, .any]
		|| ((key_type.is_int() || key_type.is_float() || is_alias) && !key_type.is_ptr())
	if !key_type_supported {
		if is_alias {
			p.error('cannot use the alias type as the parent type is unsupported')
			return 0
		}
		s := p.table.type_to_str(key_type)
		p.error_with_pos('maps only support string, integer, float, rune, enum or voidptr keys for now (not `${s}`)',
			p.tok.pos())
		return 0
	}
	p.check(.rsbr)
	value_type := p.parse_type()
	if value_type.idx() == 0 {
		// error is reported in parse_type
		return 0
	}
	if value_type.idx() == ast.void_type_idx {
		p.error_with_pos('map value type cannot be void', p.tok.pos())
		return 0
	}
	idx := p.table.find_or_register_map(key_type, value_type)
	if key_type.has_flag(.generic) || value_type.has_flag(.generic) {
		return ast.new_type(idx).set_flag(.generic)
	}
	if is_option {
		return ast.new_type(idx).set_flag(.option)
	} else {
		return ast.new_type(idx)
	}
}

fn (mut p Parser) parse_chan_type() ast.Type {
	if p.peek_tok.kind !in [.name, .key_mut, .amp, .lsbr] {
		p.next()
		return ast.chan_type
	}
	p.register_auto_import('sync')
	p.next()
	p.inside_chan_decl = true
	is_mut := p.tok.kind == .key_mut
	elem_type := p.parse_type()
	p.inside_chan_decl = false
	idx := p.table.find_or_register_chan(elem_type, is_mut)
	if elem_type.has_flag(.generic) {
		return ast.new_type(idx).set_flag(.generic)
	}
	return ast.new_type(idx)
}

fn (mut p Parser) parse_thread_type() ast.Type {
	if p.peek_tok.kind == .lpar {
		p.next()
		ret_type := p.parse_multi_return_type()
		idx := p.table.find_or_register_thread(ret_type)
		return ast.new_type(idx)
	}
	is_opt := p.peek_tok.kind == .question
	is_result := p.peek_tok.kind == .not
	if is_opt || is_result {
		p.next()
	}
	if p.peek_tok.kind !in [.name, .key_pub, .key_mut, .amp, .lsbr] {
		p.next()
		if is_opt {
			mut ret_type := ast.void_type
			ret_type = ret_type.set_flag(.option)
			idx := p.table.find_or_register_thread(ret_type)
			return ast.new_type(idx)
		} else if is_result {
			mut ret_type := ast.void_type
			ret_type = ret_type.set_flag(.result)
			idx := p.table.find_or_register_thread(ret_type)
			return ast.new_type(idx)
		} else {
			return ast.thread_type
		}
	}
	if !is_opt && !is_result {
		p.next()
	}
	if is_opt || is_result || p.tok.kind in [.amp, .lsbr]
		|| (p.tok.lit.len > 0 && p.tok.lit[0].is_capital())
		|| ast.builtin_type_names_matcher.matches(p.tok.lit)
		|| p.peek_tok.kind == .dot {
		mut ret_type := p.parse_type()
		if is_opt {
			ret_type = ret_type.set_flag(.option)
		} else if is_result {
			ret_type = ret_type.set_flag(.result)
		}
		idx := p.table.find_or_register_thread(ret_type)
		if ret_type.has_flag(.generic) {
			return ast.new_type(idx).set_flag(.generic)
		}
		return ast.new_type(idx)
	}
	return ast.thread_type
}

fn (mut p Parser) parse_multi_return_type() ast.Type {
	p.check(.lpar)
	mut mr_types := []ast.Type{}
	mut has_generic := false
	for p.tok.kind !in [.eof, .rpar] {
		mr_type := p.parse_type()
		if mr_type.idx() == 0 {
			break
		}
		if mr_type.has_flag(.generic) {
			has_generic = true
		}
		mr_types << mr_type
		if p.tok.kind == .comma {
			p.next()
		} else {
			break
		}
	}
	p.check(.rpar)
	if mr_types.len == 1 {
		// no multi return type needed
		return mr_types[0]
	}
	idx := p.table.find_or_register_multi_return(mr_types)
	if has_generic {
		return ast.new_type(idx).set_flag(.generic)
	}
	return ast.new_type(idx)
}

// given anon name based off signature when `name` is blank
fn (mut p Parser) parse_fn_type(name string, generic_types []ast.Type) ast.Type {
	fn_type_pos := p.peek_token(-2).pos()
	p.check(.key_fn)

	for attr in p.attrs {
		match attr.name {
			'callconv' {
				if !attr.has_arg {
					p.error_with_pos('callconv attribute is present but its value is missing',
						p.prev_tok.pos())
				}
				if attr.arg !in ['stdcall', 'fastcall', 'cdecl'] {
					p.error_with_pos('unsupported calling convention, supported are stdcall, fastcall and cdecl',
						p.prev_tok.pos())
				}
			}
			else {}
		}
	}

	mut has_generic := false
	line_nr := p.tok.line_nr
	params, _, is_variadic := p.fn_params()
	for param in params {
		if param.typ.has_flag(.generic) {
			has_generic = true
			break
		}
	}
	mut return_type := ast.void_type
	mut return_type_pos := token.Pos{}
	if p.tok.line_nr == line_nr && p.tok.kind.is_start_of_type() && !p.is_attributes() {
		return_type_pos = p.tok.pos()
		return_type = p.parse_type()
		if return_type.has_flag(.generic) {
			has_generic = true
		}
		return_type_pos = return_type_pos.extend(p.prev_tok.pos())
	}

	generic_names := p.types_to_names(generic_types, fn_type_pos, 'generic_types') or {
		return ast.Type(0)
	}

	func := ast.Fn{
		name: name
		params: params
		is_variadic: is_variadic
		return_type: return_type
		return_type_pos: return_type_pos
		generic_names: generic_names
		is_method: false
		attrs: p.attrs
	}
	if has_generic && generic_types.len == 0 && name.len > 0 {
		p.error_with_pos('`${name}` type is generic fntype, must specify the generic type names, e.g. ${name}[T]',
			fn_type_pos)
	}
	// MapFooFn typedefs are manually added in cheaders.v
	// because typedefs get generated after the map struct is generated
	has_decl := p.builtin_mod && name.starts_with('Map') && name.ends_with('Fn')
	idx := p.table.find_or_register_fn_type(func, false, has_decl)
	if has_generic {
		return ast.new_type(idx).set_flag(.generic)
	}
	return ast.new_type(idx)
}

fn (mut p Parser) parse_type_with_mut(is_mut bool) ast.Type {
	typ := p.parse_type()
	if is_mut {
		return typ.set_nr_muls(1)
	}
	return typ
}

// Parses any language indicators on a type.
@[direct_array_access]
fn (mut p Parser) parse_language() ast.Language {
	language := match true {
		p.tok.lit.len == 1 && p.tok.lit[0] == `C` {
			ast.Language.c
		}
		p.tok.lit.len == 2 && p.tok.lit[0] == `J` && p.tok.lit[1] == `S` {
			ast.Language.js
		}
		p.tok.lit.len == 4 && p.tok.lit[0] == `W` && p.tok.lit[1] == `A` && p.tok.lit[2] == `S`
			&& p.tok.lit[3] == `M` {
			ast.Language.wasm
		}
		else {
			ast.Language.v
		}
	}
	if language != .v {
		p.next()
		p.check(.dot)
	}
	return language
}

// parse_inline_sum_type parses the type and registers it in case the type is an anonymous sum type.
// It also takes care of inline sum types where parse_type only parses a standalone type.
fn (mut p Parser) parse_inline_sum_type() ast.Type {
	if !p.pref.is_fmt {
		p.warn(
			'inline sum types have been deprecated and will be removed on January 1, 2023 due ' +
			'to complicating the language and the compiler too much; define named sum types with `type Foo = Bar | Baz` instead')
	}
	variants := p.parse_sum_type_variants()
	if variants.len > 1 {
		if variants.len > parser.maximum_inline_sum_type_variants {
			pos := variants[0].pos.extend(variants.last().pos)
			p.warn_with_pos('an inline sum type expects a maximum of ${parser.maximum_inline_sum_type_variants} types (${variants.len} were given)',
				pos)
		}
		mut variant_names := []string{}
		for variant in variants {
			if variant.typ == 0 {
				p.error_with_pos('unknown type for variant: ${variant}', variant.pos)
				return ast.Type(0)
			}
			variant_names << p.table.sym(variant.typ).name
		}
		variant_names.sort()
		// deterministic name
		name := '_v_anon_sum_type_${variant_names.join('_')}'
		variant_types := variants.map(it.typ)
		prepend_mod_name := p.prepend_mod(name)
		mut idx := p.table.find_type_idx(prepend_mod_name)
		if idx > 0 {
			return ast.new_type(idx)
		}
		idx = p.table.register_sym(ast.TypeSymbol{
			kind: .sum_type
			name: prepend_mod_name
			cname: util.no_dots(prepend_mod_name)
			mod: p.mod
			info: ast.SumType{
				is_anon: true
				variants: variant_types
			}
		})
		return ast.new_type(idx)
	} else if variants.len == 1 {
		return variants[0].typ
	}
	return ast.Type(0)
}

// parse_sum_type_variants parses several types separated with a pipe and returns them as a list with at least one node.
// If there is less than one node, it will add an error to the error list.
fn (mut p Parser) parse_sum_type_variants() []ast.TypeNode {
	p.inside_sum_type = true
	defer {
		p.inside_sum_type = false
	}
	mut types := []ast.TypeNode{}
	for {
		type_start_pos := p.tok.pos()
		typ := p.parse_type()
		end_comments := p.eat_comments(same_line: true)
		// TODO: needs to be its own var, otherwise TCC fails because of a known stack error
		prev_tok := p.prev_tok
		type_end_pos := prev_tok.pos()
		type_pos := type_start_pos.extend(type_end_pos)
		types << ast.TypeNode{
			typ: typ
			pos: type_pos
			end_comments: end_comments
		}

		if p.tok.kind != .pipe {
			break
		}
		p.check(.pipe)
	}
	return types
}

fn (mut p Parser) parse_type() ast.Type {
	// option or result
	mut is_option := false
	mut is_result := false
	line_nr := p.tok.line_nr
	option_pos := p.tok.pos()
	if p.tok.kind == .question {
		p.next()
		is_option = true
	} else if p.tok.kind == .not {
		p.next()
		is_result = true
	}

	if is_option || is_result {
		// maybe the '[' is the start of the field attribute
		// TODO: remove once old syntax dropped
		is_required_field := p.inside_struct_field_decl && p.tok.kind == .lsbr
			&& p.peek_tok.kind == .name && p.peek_tok.lit == 'required'
		is_attr := p.tok.kind == .at

		if p.tok.line_nr > line_nr || p.tok.kind in [.comma, .rpar, .assign]
			|| (is_attr || is_required_field) {
			mut typ := ast.void_type
			if is_option {
				typ = typ.set_flag(.option)
			} else if is_result {
				typ = typ.set_flag(.result)
			}
			return typ
		}
	}

	is_shared := p.tok.kind == .key_shared
	is_atomic := p.tok.kind == .key_atomic
	if is_shared {
		p.register_auto_import('sync')
	}
	mut nr_muls := 0
	if p.tok.kind == .key_mut {
		if p.inside_fn_return {
			p.error_with_pos('cannot use `mut` on fn return type', p.tok.pos())
		} else if p.inside_struct_field_decl {
			p.error_with_pos('cannot use `mut` on struct field type', p.tok.pos())
		}
	}
	if p.tok.kind == .key_mut || is_shared {
		nr_muls++
		p.next()
	}
	if is_atomic {
		p.next()
	}
	if p.tok.kind == .mul {
		p.error('use `&Type` instead of `*Type` when declaring references')
		return 0
	}
	mut nr_amps := 0
	// &Type
	for p.tok.kind == .amp {
		nr_amps++
		nr_muls++
		p.next()
	}
	// Anon structs
	if p.tok.kind == .key_struct {
		p.anon_struct_decl = p.struct_decl(true)
		// Find the registered anon struct type, it was registered above in `p.struct_decl()`
		return p.table.find_type_idx(p.anon_struct_decl.name)
	}

	language := p.parse_language()
	mut typ := ast.void_type
	is_array := p.tok.kind == .lsbr
	pos := p.tok.pos()
	if p.tok.kind != .lcbr {
		typ = p.parse_any_type(language, nr_muls > 0, true, is_option)
		if typ.idx() == 0 {
			// error is set in parse_type
			return 0
		}
		if typ == ast.void_type {
			p.error_with_pos('use `?` instead of `?void`', pos)
			return 0
		}
		sym := p.table.sym(typ)
		if is_option && sym.info is ast.SumType && sym.info.is_anon {
			p.error_with_pos('an inline sum type cannot be an Option', option_pos.extend(p.prev_tok.pos()))
		}
	}
	if is_option {
		typ = typ.set_flag(.option)
	}
	if is_result {
		typ = typ.set_flag(.result)
	}
	if is_shared {
		typ = typ.set_flag(.shared_f)
	}
	if is_atomic {
		typ = typ.set_flag(.atomic_f)
	}
	if typ.idx() == ast.array_type && !p.builtin_mod && p.mod !in ['os', 'strconv', 'sync']
		&& !p.inside_unsafe {
		p.error_with_pos('`array` is an internal type, it cannot be used directly. Use `[]int`, `[]Foo` etc',
			pos)
	}
	if nr_muls > 0 {
		typ = typ.set_nr_muls(nr_muls)
		if is_array && nr_amps > 0 {
			p.error_with_pos('V arrays are already references behind the scenes,
there is no need to use a reference to an array (e.g. use `[]string` instead of `&[]string`).
If you need to modify an array in a function, use a mutable argument instead: `fn foo(mut s []string) {}`.',
				pos)
			return 0
		}
	}
	return typ
}

fn (mut p Parser) parse_any_type(language ast.Language, is_ptr bool, check_dot bool, is_option bool) ast.Type {
	mut name := p.tok.lit
	if language == .c {
		name = 'C.${name}'
	} else if language == .js {
		name = 'JS.${name}'
	} else if p.peek_tok.kind == .dot && check_dot && p.tok.lit.len > 0
		&& !p.tok.lit[0].is_capital() {
		// `module.Type`
		mut mod := name
		mut mod_pos := p.tok.pos()
		p.next()
		p.check(.dot)
		mut mod_last_part := mod
		for p.peek_tok.kind == .dot {
			mod_pos = mod_pos.extend(p.tok.pos())
			mod_last_part = p.tok.lit
			mod += '.${mod_last_part}'
			p.next()
			p.check(.dot)
		}
		if !p.known_import(mod) && !p.pref.is_fmt {
			mut msg := 'unknown module `${mod}`'
			if mod.len > mod_last_part.len && p.known_import(mod_last_part) {
				msg += '; did you mean `${mod_last_part}`?'
			}
			p.error_with_pos(msg, mod_pos)
			return 0
		}
		if mod in p.imports {
			p.register_used_import(mod)
			mod = p.imports[mod]
		}
		// prefix with full module
		name = '${mod}.${p.tok.lit}'
		if p.tok.lit.len > 0 && !p.tok.lit[0].is_capital() {
			p.error('imported types must start with a capital letter')
			return 0
		}
	} else if p.expr_mod != '' && !p.inside_generic_params {
		// p.expr_mod is from the struct and not from the generic parameter
		name = p.expr_mod + '.' + name
	} else if name in p.imported_symbols {
		name = p.imported_symbols[name]
		p.register_used_import_for_symbol_name(name)
	} else if !p.builtin_mod && name.len > 1 && name !in p.table.type_idxs {
		// `Foo` in module `mod` means `mod.Foo`
		name = p.mod + '.' + name
	}
	match p.tok.kind {
		.key_fn {
			// func
			return p.parse_fn_type('', []ast.Type{})
		}
		.lsbr, .nilsbr {
			// array
			return p.parse_array_type(p.tok.kind, is_option)
		}
		else {
			if p.tok.kind == .lpar {
				// multiple return
				if is_ptr {
					p.unexpected(prepend_msg: 'parse_type:', got: '`&` before multiple returns')
					return 0
				}
				return p.parse_multi_return_type()
			}
			if ((p.peek_tok.kind == .dot && p.peek_token(3).kind == .pipe)
				|| p.peek_tok.kind == .pipe) && !p.inside_sum_type && !p.inside_receiver_param {
				return p.parse_inline_sum_type()
			}
			if name == 'map' {
				return p.parse_map_type()
			}
			if name == 'chan' {
				return p.parse_chan_type()
			}
			if name == 'thread' {
				return p.parse_thread_type()
			}
			mut ret := ast.Type(0)
			if name == '' {
				// This means the developer is using some wrong syntax like `x: int` instead of `x int`
				p.error('expecting type declaration')
			} else {
				match name {
					'voidptr' {
						ret = ast.voidptr_type
					}
					'byteptr' {
						ret = ast.byteptr_type
					}
					'charptr' {
						ret = ast.charptr_type
					}
					'i8' {
						ret = ast.i8_type
					}
					'i16' {
						ret = ast.i16_type
					}
					'int' {
						ret = ast.int_type
					}
					'i64' {
						ret = ast.i64_type
					}
					'u8' {
						ret = ast.u8_type
					}
					'u16' {
						ret = ast.u16_type
					}
					'u32' {
						ret = ast.u32_type
					}
					'u64' {
						ret = ast.u64_type
					}
					'f32' {
						ret = ast.f32_type
					}
					'f64' {
						ret = ast.f64_type
					}
					'string' {
						ret = ast.string_type
					}
					'char' {
						ret = ast.char_type
					}
					'bool' {
						ret = ast.bool_type
					}
					'float_literal' {
						ret = ast.float_literal_type
					}
					'int_literal' {
						ret = ast.int_literal_type
					}
					'any' {
						if p.file_backend_mode != .js && p.mod != 'builtin' {
							p.error('cannot use `any` type here')
						}
						ret = ast.any_type
					}
					else {
						p.next()
						if name.len == 1 && name[0].is_capital() {
							return p.parse_generic_type(name)
						}
						if p.tok.kind in [.lt, .lsbr]
							&& p.tok.pos - p.prev_tok.pos == p.prev_tok.len {
							return p.parse_generic_inst_type(name)
						}
						return p.find_type_or_add_placeholder(name, language)
					}
				}
			}
			p.next()
			return ret
		}
	}
}

fn (mut p Parser) find_type_or_add_placeholder(name string, language ast.Language) ast.Type {
	// struct / enum / placeholder
	mut idx := p.table.find_type_idx(name)
	if idx > 0 {
		mut typ := ast.new_type(idx)
		sym := p.table.sym(typ)
		match sym.info {
			ast.Struct, ast.Interface, ast.SumType {
				if p.struct_init_generic_types.len > 0 && sym.info.generic_types.len > 0
					&& p.struct_init_generic_types != sym.info.generic_types {
					generic_names := p.types_to_names(p.struct_init_generic_types, p.tok.pos(),
						'struct_init_generic_types') or { return ast.Type(0) }
					// NOTE:
					// Used here for the wraparound `< >` characters, is not a reserved character in generic syntax,
					// is used as the `generic names` part of the qualified type name,
					// while the `[ ]` characters is the `concrete names` part of the qualified type name.
					// The two pairs characters distinguish the generic name from the concrete name, such as:
					//   main.Foo<T, U>[int, string]
					//   main.Foo<T, <T, U>>[int, [int, string]]
					//   ...
					// There are methods in ast/types that rely on this representation:
					//   fn symbol_name_except_generic()
					//   fn embed_name()
					//   fn strip_extra_struct_types()
					mut sym_name := sym.name + '<'
					for i, gt in generic_names {
						sym_name += gt
						if i != generic_names.len - 1 {
							sym_name += ','
						}
					}
					sym_name += '>'
					existing_idx := p.table.type_idxs[sym_name]
					if existing_idx > 0 {
						idx = existing_idx
					} else {
						idx = p.table.register_sym(ast.TypeSymbol{
							...sym
							name: sym_name
							rname: sym.name
							generic_types: p.struct_init_generic_types.clone()
						})
					}
					typ = ast.new_type(idx)
				}
			}
			else {}
		}
		return typ
	}
	// not found - add placeholder
	idx = p.table.add_placeholder_type(name, language)
	return ast.new_type(idx)
}

fn (mut p Parser) parse_generic_type(name string) ast.Type {
	mut idx := p.table.find_type_idx(name)
	if idx > 0 {
		return ast.new_type(idx).set_flag(.generic)
	}
	idx = p.table.register_sym(ast.TypeSymbol{
		name: name
		cname: util.no_dots(name)
		mod: p.mod
		kind: .any
		is_pub: true
	})
	return ast.new_type(idx).set_flag(.generic)
}

fn (mut p Parser) parse_generic_inst_type(name string) ast.Type {
	mut bs_name := name
	mut bs_cname := name
	start_pos := p.tok.pos()
	p.next()
	p.inside_generic_params = true
	bs_name += '['
	bs_cname += '_T_'
	mut concrete_types := []ast.Type{}
	mut is_instance := true
	for p.tok.kind != .eof {
		mut type_pos := p.tok.pos()
		gt := p.parse_type()
		type_pos = type_pos.extend(p.prev_tok.pos())
		if gt.has_flag(.generic) {
			is_instance = false
		}
		gts := p.table.sym(gt)
		if gts.kind == .multi_return {
			p.error_with_pos('cannot use multi return as generic concrete type', type_pos)
		}
		if gt.is_ptr() {
			bs_name += '&'
		}
		bs_name += gts.name
		bs_cname += gts.cname
		concrete_types << gt
		if p.tok.kind != .comma {
			break
		}
		p.next()
		bs_name += ', '
		bs_cname += '_'
	}
	if !is_instance {
		p.struct_init_generic_types = concrete_types
	}
	concrete_types_pos := start_pos.extend(p.tok.pos())
	p.next()
	p.inside_generic_params = false
	bs_name += ']'
	// fmt operates on a per-file basis, so is_instance might be not set correctly. Thus it's ignored.
	if (is_instance || p.pref.is_fmt) && concrete_types.len > 0 {
		mut gt_idx := p.table.find_type_idx(bs_name)
		if gt_idx > 0 {
			return ast.new_type(gt_idx)
		}
		gt_idx = p.table.add_placeholder_type(bs_name, .v)
		mut parent_idx := p.table.type_idxs[name]
		if parent_idx == 0 {
			parent_idx = p.table.add_placeholder_type(name, .v)
		}
		parent_sym := p.table.sym(ast.new_type(parent_idx))
		match parent_sym.info {
			ast.Struct {
				if parent_sym.info.generic_types.len == 0 {
					p.error_with_pos('struct `${parent_sym.name}` is not a generic struct, cannot instantiate to the concrete types',
						concrete_types_pos)
				} else if parent_sym.info.generic_types.len != concrete_types.len {
					p.error_with_pos('the number of generic types of struct `${parent_sym.name}` is inconsistent with the concrete types',
						concrete_types_pos)
				}
			}
			ast.Interface {
				if parent_sym.info.generic_types.len == 0 {
					p.error_with_pos('interface `${parent_sym.name}` is not a generic interface, cannot instantiate to the concrete types',
						concrete_types_pos)
				} else if parent_sym.info.generic_types.len != concrete_types.len {
					p.error_with_pos('the number of generic types of interface `${parent_sym.name}` is inconsistent with the concrete types',
						concrete_types_pos)
				}
			}
			ast.SumType {
				if parent_sym.info.generic_types.len == 0 {
					p.error_with_pos('sumtype `${parent_sym.name}` is not a generic sumtype, cannot instantiate to the concrete types',
						concrete_types_pos)
				} else if parent_sym.info.generic_types.len != concrete_types.len {
					p.error_with_pos('the number of generic types of sumtype `${parent_sym.name}` is inconsistent with the concrete types',
						concrete_types_pos)
				}
			}
			else {}
		}

		idx := p.table.register_sym(ast.TypeSymbol{
			kind: .generic_inst
			name: bs_name
			cname: util.no_dots(bs_cname)
			mod: p.mod
			info: ast.GenericInst{
				parent_idx: parent_idx
				concrete_types: concrete_types
			}
		})
		return ast.new_type(idx)
	}
	return p.find_type_or_add_placeholder(name, .v).set_flag(.generic)
}

fn (mut p Parser) types_to_names(types []ast.Type, pos token.Pos, error_label string) ![]string {
	mut res := []string{}
	for t in types {
		if t == 0 {
			p.error_with_pos('unknown type found, ${error_label}: ${types}', pos)
			return error('unknown 0 type')
		}
		res << p.table.sym(t).name
	}
	return res
}
