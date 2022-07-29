// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.transformer
import v.ast
import v.util
import v.token

const maximum_inline_sum_type_variants = 3

pub fn (mut p Parser) parse_array_type(expecting token.Kind) ast.Type {
	p.check(expecting)
	// fixed array
	if p.tok.kind in [.number, .name] {
		mut fixed_size := 0
		size_expr := p.expr(0)
		if p.pref.is_fmt {
			fixed_size = 987654321
		} else {
			match size_expr {
				ast.IntegerLiteral {
					fixed_size = size_expr.val.int()
				}
				ast.Ident {
					mut show_non_const_error := false
					if mut const_field := p.table.global_scope.find_const('${p.mod}.$size_expr.name') {
						if mut const_field.expr is ast.IntegerLiteral {
							fixed_size = const_field.expr.val.int()
						} else {
							if mut const_field.expr is ast.InfixExpr {
								// QUESTION: this should most likely no be done in the parser, right?
								mut t := transformer.new_transformer(p.pref)
								folded_expr := t.infix_expr(mut const_field.expr)

								if folded_expr is ast.IntegerLiteral {
									fixed_size = folded_expr.val.int()
								} else {
									show_non_const_error = true
								}
							} else {
								show_non_const_error = true
							}
						}
					} else {
						if p.pref.is_fmt {
							// for vfmt purposes, pretend the constant does exist
							// it may have been defined in another .v file:
							fixed_size = 1
						} else {
							show_non_const_error = true
						}
					}
					if show_non_const_error {
						p.error_with_pos('non-constant array bound `$size_expr.name`',
							size_expr.pos)
					}
				}
				else {
					p.error_with_pos('fixed array size cannot use non-constant value',
						size_expr.pos())
				}
			}
		}
		p.check(.rsbr)
		elem_type := p.parse_type()
		if elem_type.idx() == 0 {
			// error is handled by parse_type
			return 0
		}
		if fixed_size <= 0 {
			p.error_with_pos('fixed size cannot be zero or negative', size_expr.pos())
		}
		idx := p.table.find_or_register_array_fixed(elem_type, fixed_size, size_expr)
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

pub fn (mut p Parser) parse_map_type() ast.Type {
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
		p.error_with_pos('maps only support string, integer, float, rune, enum or voidptr keys for now (not `$s`)',
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
	return ast.new_type(idx)
}

pub fn (mut p Parser) parse_chan_type() ast.Type {
	if p.peek_tok.kind !in [.name, .key_mut, .amp, .lsbr] {
		p.next()
		return ast.chan_type
	}
	p.register_auto_import('sync')
	p.next()
	is_mut := p.tok.kind == .key_mut
	elem_type := p.parse_type()
	idx := p.table.find_or_register_chan(elem_type, is_mut)
	if elem_type.has_flag(.generic) {
		return ast.new_type(idx).set_flag(.generic)
	}
	return ast.new_type(idx)
}

pub fn (mut p Parser) parse_thread_type() ast.Type {
	is_opt := p.peek_tok.kind == .question
	if is_opt {
		p.next()
	}
	if p.peek_tok.kind !in [.name, .key_mut, .amp, .lsbr] {
		p.next()
		if is_opt {
			mut ret_type := ast.void_type
			ret_type = ret_type.set_flag(.optional)
			idx := p.table.find_or_register_thread(ret_type)
			return ast.new_type(idx)
		} else {
			return ast.thread_type
		}
	}
	if !is_opt {
		p.next()
	}
	ret_type := p.parse_type()
	idx := p.table.find_or_register_thread(ret_type)
	if ret_type.has_flag(.generic) {
		return ast.new_type(idx).set_flag(.generic)
	}
	return ast.new_type(idx)
}

pub fn (mut p Parser) parse_multi_return_type() ast.Type {
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
pub fn (mut p Parser) parse_fn_type(name string) ast.Type {
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
	args, _, is_variadic := p.fn_args()
	for arg in args {
		if arg.typ.has_flag(.generic) {
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
	func := ast.Fn{
		name: name
		params: args
		is_variadic: is_variadic
		return_type: return_type
		return_type_pos: return_type_pos
		is_method: false
		attrs: p.attrs
	}
	// MapFooFn typedefs are manually added in cheaders.v
	// because typedefs get generated after the map struct is generated
	has_decl := p.builtin_mod && name.starts_with('Map') && name.ends_with('Fn')
	idx := p.table.find_or_register_fn_type(p.mod, func, false, has_decl)
	if has_generic {
		return ast.new_type(idx).set_flag(.generic)
	}
	return ast.new_type(idx)
}

pub fn (mut p Parser) parse_type_with_mut(is_mut bool) ast.Type {
	typ := p.parse_type()
	if is_mut {
		return typ.set_nr_muls(1)
	}
	return typ
}

// Parses any language indicators on a type.
pub fn (mut p Parser) parse_language() ast.Language {
	language := if p.tok.lit == 'C' {
		ast.Language.c
	} else if p.tok.lit == 'JS' {
		ast.Language.js
	} else {
		ast.Language.v
	}
	if language != .v {
		p.next()
		p.check(.dot)
	}
	return language
}

// parse_inline_sum_type parses the type and registers it in case the type is an anonymous sum type.
// It also takes care of inline sum types where parse_type only parses a standalone type.
pub fn (mut p Parser) parse_inline_sum_type() ast.Type {
	variants := p.parse_sum_type_variants()
	if variants.len > 1 {
		if variants.len > parser.maximum_inline_sum_type_variants {
			pos := variants[0].pos.extend(variants[variants.len - 1].pos)
			p.warn_with_pos('an inline sum type expects a maximum of $parser.maximum_inline_sum_type_variants types ($variants.len were given)',
				pos)
		}
		mut variant_names := variants.map(p.table.sym(it.typ).name)
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
pub fn (mut p Parser) parse_sum_type_variants() []ast.TypeNode {
	p.inside_sum_type = true
	defer {
		p.inside_sum_type = false
	}
	mut types := []ast.TypeNode{}
	for {
		type_start_pos := p.tok.pos()
		typ := p.parse_type()
		// TODO: needs to be its own var, otherwise TCC fails because of a known stack error
		prev_tok := p.prev_tok
		type_end_pos := prev_tok.pos()
		type_pos := type_start_pos.extend(type_end_pos)
		types << ast.TypeNode{
			typ: typ
			pos: type_pos
		}
		if p.tok.kind != .pipe {
			break
		}
		p.check(.pipe)
	}
	return types
}

pub fn (mut p Parser) parse_type() ast.Type {
	// optional or result
	mut is_optional := false
	mut is_result := false
	line_nr := p.tok.line_nr
	optional_pos := p.tok.pos()
	if p.tok.kind == .question {
		p.next()
		is_optional = true
	} else if p.tok.kind == .not {
		p.next()
		is_result = true
	}
	if (is_optional || is_result) && (p.tok.line_nr > line_nr || p.tok.kind in [.comma, .rpar]) {
		mut typ := ast.void_type
		if is_optional {
			typ = typ.set_flag(.optional)
		} else if is_result {
			typ = typ.set_flag(.result)
		}
		return typ
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
		struct_decl := p.struct_decl(true)
		// Find the registered anon struct type, it was registered above in `p.struct_decl()`
		return p.table.find_type_idx(struct_decl.name)
	}

	language := p.parse_language()
	mut typ := ast.void_type
	is_array := p.tok.kind == .lsbr
	if p.tok.kind != .lcbr {
		pos := p.tok.pos()
		typ = p.parse_any_type(language, nr_muls > 0, true)
		if typ.idx() == 0 {
			// error is set in parse_type
			return 0
		}
		if typ == ast.void_type {
			p.error_with_pos('use `?` instead of `?void`', pos)
			return 0
		}
		sym := p.table.sym(typ)
		if is_optional && sym.info is ast.SumType && (sym.info as ast.SumType).is_anon {
			p.error_with_pos('an inline sum type cannot be optional', optional_pos.extend(p.prev_tok.pos()))
		}
	}
	if is_optional {
		typ = typ.set_flag(.optional)
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
	if nr_muls > 0 {
		typ = typ.set_nr_muls(nr_muls)
		if is_array && nr_amps > 0 {
			p.error('V arrays are already references behind the scenes,
there is no need to use a reference to an array (e.g. use `[]string` instead of `&[]string`).
If you need to modify an array in a function, use a mutable argument instead: `fn foo(mut s []string) {}`.')
			return 0
		}
	}
	return typ
}

pub fn (mut p Parser) parse_any_type(language ast.Language, is_ptr bool, check_dot bool) ast.Type {
	mut name := p.tok.lit
	if language == .c {
		name = 'C.$name'
	} else if language == .js {
		name = 'JS.$name'
	} else if p.peek_tok.kind == .dot && check_dot {
		// `module.Type`
		mut mod := name
		mut mod_pos := p.tok.pos()
		p.next()
		p.check(.dot)
		mut mod_last_part := mod
		for p.peek_tok.kind == .dot {
			mod_pos = mod_pos.extend(p.tok.pos())
			mod_last_part = p.tok.lit
			mod += '.$mod_last_part'
			p.next()
			p.check(.dot)
		}
		if !p.known_import(mod) && !p.pref.is_fmt {
			mut msg := 'unknown module `$mod`'
			if mod.len > mod_last_part.len && p.known_import(mod_last_part) {
				msg += '; did you mean `$mod_last_part`?'
			}
			p.error_with_pos(msg, mod_pos)
			return 0
		}
		if mod in p.imports {
			p.register_used_import(mod)
			mod = p.imports[mod]
		}
		// prefix with full module
		name = '${mod}.$p.tok.lit'
		if p.tok.lit.len > 0 && !p.tok.lit[0].is_capital() {
			p.error('imported types must start with a capital letter')
			return 0
		}
	} else if p.expr_mod != '' && !p.inside_generic_params {
		// p.expr_mod is from the struct and not from the generic parameter
		name = p.expr_mod + '.' + name
	} else if name in p.imported_symbols {
		name = p.imported_symbols[name]
	} else if !p.builtin_mod && name.len > 1 && name !in p.table.type_idxs {
		// `Foo` in module `mod` means `mod.Foo`
		name = p.mod + '.' + name
	}
	match p.tok.kind {
		.key_fn {
			// func
			return p.parse_fn_type('')
		}
		.lsbr, .nilsbr {
			// array
			return p.parse_array_type(p.tok.kind)
		}
		else {
			if p.tok.kind == .lpar && !p.inside_sum_type {
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
							p.error('cannot use `any` type here, `any` will be implemented in V 0.4')
						}
						ret = ast.any_type
					}
					else {
						p.next()
						if name.len == 1 && name[0].is_capital() {
							return p.parse_generic_type(name)
						}
						if p.tok.kind == .lt {
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

pub fn (mut p Parser) find_type_or_add_placeholder(name string, language ast.Language) ast.Type {
	// struct / enum / placeholder
	mut idx := p.table.find_type_idx(name)
	if idx > 0 {
		return ast.new_type(idx)
	}
	// not found - add placeholder
	idx = p.table.add_placeholder_type(name, language)
	return ast.new_type(idx)
}

pub fn (mut p Parser) parse_generic_type(name string) ast.Type {
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

pub fn (mut p Parser) parse_generic_inst_type(name string) ast.Type {
	mut bs_name := name
	mut bs_cname := name
	start_pos := p.tok.pos()
	p.next()
	p.inside_generic_params = true
	bs_name += '<'
	bs_cname += '_T_'
	mut concrete_types := []ast.Type{}
	mut is_instance := false
	for p.tok.kind != .eof {
		mut type_pos := p.tok.pos()
		gt := p.parse_type()
		type_pos = type_pos.extend(p.prev_tok.pos())
		if !gt.has_flag(.generic) {
			is_instance = true
		}
		gts := p.table.sym(gt)
		if !is_instance && gts.name.len > 1 {
			p.error_with_pos('generic struct parameter name needs to be exactly one char',
				type_pos)
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
	p.check(.gt)
	p.inside_generic_params = false
	bs_name += '>'
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
					p.error_with_pos('struct `$parent_sym.name` is not a generic struct, cannot instantiate to the concrete types',
						concrete_types_pos)
				} else if parent_sym.info.generic_types.len != concrete_types.len {
					p.error_with_pos('the number of generic types of struct `$parent_sym.name` is inconsistent with the concrete types',
						concrete_types_pos)
				}
			}
			ast.Interface {
				if parent_sym.info.generic_types.len == 0 {
					p.error_with_pos('interface `$parent_sym.name` is not a generic interface, cannot instantiate to the concrete types',
						concrete_types_pos)
				} else if parent_sym.info.generic_types.len != concrete_types.len {
					p.error_with_pos('the number of generic types of interfce `$parent_sym.name` is inconsistent with the concrete types',
						concrete_types_pos)
				}
			}
			ast.SumType {
				if parent_sym.info.generic_types.len == 0 {
					p.error_with_pos('sumtype `$parent_sym.name` is not a generic sumtype, cannot instantiate to the concrete types',
						concrete_types_pos)
				} else if parent_sym.info.generic_types.len != concrete_types.len {
					p.error_with_pos('the number of generic types of sumtype `$parent_sym.name` is inconsistent with the concrete types',
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
