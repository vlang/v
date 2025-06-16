// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.token

fn (mut c Checker) array_init(mut node ast.ArrayInit) ast.Type {
	mut elem_type := ast.void_type
	unwrap_elem_type := c.unwrap_generic(node.elem_type)
	if c.pref.warn_about_allocs {
		c.warn_alloc('array initialization', node.pos)
	}
	// `x := []string{}` (the type was set in the parser)
	if node.typ != ast.void_type {
		if !c.is_builtin_mod && c.mod !in ['builtin', 'strings', 'strconv', 'math.bits'] {
			c.table.used_features.arr_init = true
		}
		if node.elem_type != 0 {
			elem_sym := c.table.sym(node.elem_type)
			c.check_any_type(node.elem_type, elem_sym, node.pos)
			if node.typ.has_flag(.option) && (node.has_cap || node.has_len) {
				c.error('Option array `${elem_sym.name}` cannot have initializers', node.pos)
			}
			match elem_sym.info {
				ast.Struct {
					if elem_sym.info.generic_types.len > 0 && elem_sym.info.concrete_types.len == 0
						&& !node.elem_type.has_flag(.generic) {
						if c.table.cur_concrete_types.len == 0 {
							c.error('generic struct `${elem_sym.name}` must specify type parameter, e.g. ${elem_sym.name}[int]',
								node.elem_type_pos)
						} else {
							c.error('generic struct `${elem_sym.name}` must specify type parameter, e.g. ${elem_sym.name}[T]',
								node.elem_type_pos)
						}
					}
				}
				ast.Interface {
					if elem_sym.info.generic_types.len > 0 && elem_sym.info.concrete_types.len == 0
						&& !node.elem_type.has_flag(.generic) {
						if c.table.cur_concrete_types.len == 0 {
							c.error('generic interface `${elem_sym.name}` must specify type parameter, e.g. ${elem_sym.name}[int]',
								node.elem_type_pos)
						} else {
							c.error('generic interface `${elem_sym.name}` must specify type parameter, e.g. ${elem_sym.name}[T]',
								node.elem_type_pos)
						}
					}
				}
				ast.SumType {
					if elem_sym.info.generic_types.len > 0 && elem_sym.info.concrete_types.len == 0
						&& !node.elem_type.has_flag(.generic) {
						if c.table.cur_concrete_types.len == 0 {
							c.error('generic sumtype `${elem_sym.name}` must specify type parameter, e.g. ${elem_sym.name}[int]',
								node.elem_type_pos)
						} else {
							c.error('generic sumtype `${elem_sym.name}` must specify type parameter, e.g. ${elem_sym.name}[T]',
								node.elem_type_pos)
						}
					}
				}
				ast.Alias {
					if elem_sym.name == 'byte' {
						c.error('byte is deprecated, use u8 instead', node.elem_type_pos)
					}
				}
				ast.Map {
					c.markused_array_method(!c.is_builtin_mod, 'map')
				}
				else {}
			}
		}
		if node.exprs.len == 0 {
			if node.has_cap {
				c.check_array_init_para_type('cap', mut node.cap_expr, node.pos)
			}
			if node.has_len {
				c.check_array_init_para_type('len', mut node.len_expr, node.pos)
			}
		}
		if node.has_init {
			c.check_array_init_default_expr(mut node)
		}
		if node.has_len {
			len_typ := c.check_expr_option_or_result_call(node.len_expr, c.expr(mut node.len_expr))
			if len_typ.has_flag(.option) {
				c.error('cannot use unwrapped Option as length', node.len_expr.pos())
			}
			// check &int{}, interface, sum_type initialized
			if !node.has_init {
				c.check_elements_initialized(unwrap_elem_type) or {
					c.warn('${err.msg()}, therefore `len:` cannot be used (unless inside `unsafe`, or if you also use `init:`)',
						node.pos)
				}
			}
		}
		if node.has_cap {
			cap_typ := c.check_expr_option_or_result_call(node.cap_expr, c.expr(mut node.cap_expr))
			if cap_typ.has_flag(.option) {
				c.error('cannot use unwrapped Option as capacity', node.cap_expr.pos())
			}
		}
		c.ensure_type_exists(node.elem_type, node.elem_type_pos)
		if node.typ.has_flag(.generic) && c.table.cur_fn != unsafe { nil }
			&& c.table.cur_fn.generic_names.len == 0 {
			c.error('generic struct cannot be used in non-generic function', node.pos)
		}

		// `&Struct{} check
		if node.has_len {
			c.check_elements_ref_fields_initialized(unwrap_elem_type, node.pos)
		}
		// T{0} initialization when T is an array
		if !node.is_fixed && node.expr_types.len == 0 {
			for mut expr in node.exprs {
				typ := c.expr(mut expr)
				c.check_expected(typ, node.elem_type) or {
					c.error('invalid array element: ${err.msg()}', expr.pos())
				}
				node.expr_types << typ
			}
		}
		return node.typ
	}

	if node.is_fixed {
		c.ensure_type_exists(node.elem_type, node.elem_type_pos)
		if !c.is_builtin_mod {
			c.check_elements_initialized(unwrap_elem_type) or {
				c.warn('fixed ${err.msg()} (unless inside `unsafe`)', node.pos)
			}
		}
		c.check_elements_ref_fields_initialized(unwrap_elem_type, node.pos)
	}
	// `a = []`
	if node.exprs.len == 0 {
		// `a := fn_returning_opt_array() or { [] }`
		if c.expected_type == ast.void_type {
			if c.expected_or_type != ast.void_type {
				c.expected_type = c.expected_or_type
			} else if c.expected_expr_type != ast.void_type {
				c.expected_type = c.expected_expr_type
			}
		}
		mut type_sym := c.table.sym(c.expected_type)
		if type_sym.kind != .array || type_sym.array_info().elem_type == ast.void_type {
			c.error('array_init: no type specified (maybe: `[]Type{}` instead of `[]`)',
				node.pos)
			return ast.void_type
		}
		array_info := type_sym.array_info()
		node.elem_type = array_info.elem_type
		// clear option flag in case of: `fn opt_arr() ?[]int { return [] }`
		return if c.expected_type.has_flag(.shared_f) {
			c.expected_type.clear_flag(.shared_f).deref()
		} else {
			c.expected_type
		}.clear_option_and_result()
	}
	// `[1,2,3]`
	if node.exprs.len > 0 && node.elem_type == ast.void_type {
		if !c.is_builtin_mod && c.mod !in ['builtin', 'strings', 'strconv', 'math.bits'] {
			c.table.used_features.arr_init = true
		}
		mut expected_value_type := ast.void_type
		mut expecting_interface_array := false
		mut expecting_sumtype_array := false
		mut is_first_elem_ptr := false
		if c.expected_type != 0 {
			expected_value_type = c.table.value_type(c.expected_type)
			expected_value_sym := c.table.sym(expected_value_type)
			if expected_value_sym.kind == .interface {
				// array of interfaces? (`[dog, cat]`) Save the interface type (`Animal`)
				expecting_interface_array = true
			} else if expected_value_sym.kind == .sum_type {
				expecting_sumtype_array = true
			}
		}
		for i, mut expr in node.exprs {
			mut typ := ast.void_type
			if expr is ast.ArrayInit {
				old_expected_type := c.expected_type
				c.expected_type = c.table.value_type(c.expected_type)
				typ = c.check_expr_option_or_result_call(expr, c.expr(mut expr))
				c.expected_type = old_expected_type
			} else {
				// [none]
				if c.expected_type == ast.none_type && expr is ast.None {
					c.error('invalid expression `none`, it is not an array of Option type',
						expr.pos())
					continue
				}
				typ = c.check_expr_option_or_result_call(expr, c.expr(mut expr))
			}
			if expr is ast.CallExpr {
				ret_sym := c.table.sym(typ)
				if ret_sym.kind == .array_fixed {
					typ = c.cast_fixed_array_ret(typ, ret_sym)
				}
				node.has_callexpr = true
			}
			if typ == ast.void_type {
				c.error('invalid void array element type', expr.pos())
			}
			node.expr_types << typ
			// the first element's type
			if expecting_interface_array {
				if i == 0 {
					elem_type = expected_value_type
					c.expected_type = elem_type
					c.type_implements(typ, elem_type, expr.pos())
				}
				if !typ.is_any_kind_of_pointer() && !c.inside_unsafe {
					typ_sym := c.table.sym(typ)
					if typ_sym.kind != .interface {
						c.mark_as_referenced(mut &expr, true)
					}
				}
				continue
			} else if expecting_sumtype_array {
				if i == 0 {
					if c.table.is_sumtype_or_in_variant(expected_value_type, ast.mktyp(typ)) {
						elem_type = expected_value_type
					} else {
						if expr.is_auto_deref_var() {
							elem_type = ast.mktyp(typ.deref())
						} else {
							elem_type = ast.mktyp(typ)
						}
					}
					c.expected_type = elem_type
				}
				continue
			}
			// the first element's type
			if i == 0 {
				if expr.is_auto_deref_var() {
					elem_type = ast.mktyp(typ.deref())
				} else {
					elem_type = ast.mktyp(typ)
				}
				if typ.is_ptr() && c.in_for_count == 0 {
					is_first_elem_ptr = true
				}
				c.expected_type = elem_type
				continue
			} else {
				if !typ.is_any_kind_of_pointer() && !typ.is_int() && is_first_elem_ptr {
					c.error('cannot have non-pointer of type `${c.table.type_to_str(typ)}` in a pointer array of type `${c.table.type_to_str(elem_type)}`',
						expr.pos())
				}
			}
			if expr !is ast.TypeNode {
				if c.table.type_kind(elem_type) == .interface {
					if c.type_implements(typ, elem_type, expr.pos()) {
						continue
					}
				}
				c.check_expected(typ, elem_type) or {
					c.error('invalid array element: ${err.msg()}', expr.pos())
				}
				if !elem_type.has_flag(.option)
					&& (typ.has_flag(.option) || typ.idx() == ast.none_type_idx) {
					typ_str, elem_type_str := c.get_string_names_of(typ, elem_type)
					if typ.idx() == ast.none_type_idx {
						c.error('cannot use `${typ_str}` as `${elem_type_str}`', expr.pos())
					} else {
						c.error('cannot use `${typ_str}` as `${elem_type_str}`, it must be unwrapped first',
							expr.pos())
					}
				} else if elem_type.has_flag(.option) && !typ.has_flag(.option)
					&& typ.idx() != ast.none_type_idx && !expr.is_pure_literal() {
					typ_str, elem_type_str := c.get_string_names_of(typ, elem_type)
					c.error('cannot use `${typ_str}` as `${elem_type_str}`', expr.pos())
				}
			}
		}
		if node.is_fixed {
			idx := c.table.find_or_register_array_fixed(elem_type, node.exprs.len, ast.empty_expr,
				false)
			if elem_type.has_flag(.generic) {
				node.typ = ast.new_type(idx).set_flag(.generic)
			} else {
				node.typ = ast.new_type(idx)
			}
		} else {
			idx := c.table.find_or_register_array(elem_type)
			if elem_type.has_flag(.generic) {
				node.typ = ast.new_type(idx).set_flag(.generic)
			} else {
				node.typ = ast.new_type(idx)
			}
		}
		node.elem_type = elem_type
	} else if node.is_fixed && node.exprs.len == 1 && node.elem_type != ast.void_type {
		// `[50]u8`
		sym := c.table.sym(node.typ)
		if sym.info !is ast.ArrayFixed
			|| c.array_fixed_has_unresolved_size(sym.info as ast.ArrayFixed) {
			mut size_expr := node.exprs[0]
			node.typ = c.eval_array_fixed_sizes(mut size_expr, 0, node.elem_type)
			if node.is_option {
				node.typ = node.typ.set_flag(.option)
			}
			node.elem_type = (c.table.sym(node.typ).info as ast.ArrayFixed).elem_type
		}
		if node.has_init {
			c.check_array_init_default_expr(mut node)
		}
	}
	return node.typ
}

fn (mut c Checker) check_array_init_default_expr(mut node ast.ArrayInit) {
	mut init_expr := node.init_expr
	c.expected_type = node.elem_type
	init_typ := c.check_expr_option_or_result_call(init_expr, c.expr(mut init_expr))
	node.init_type = init_typ
	if !node.elem_type.has_flag(.option) && init_typ.has_flag(.option) {
		c.error('cannot use unwrapped Option as initializer', init_expr.pos())
	}
	if node.elem_type.is_number() && init_typ.is_number() {
		return
	}
	c.check_expected(init_typ, node.elem_type) or { c.error(err.msg(), init_expr.pos()) }
}

fn (mut c Checker) check_array_init_para_type(para string, mut expr ast.Expr, pos token.Pos) {
	sym := c.table.final_sym(c.unwrap_generic(c.expr(mut expr)))
	if sym.kind !in [.int, .int_literal] {
		c.error('array ${para} needs to be an int', pos)
	}
	if expr is ast.IntegerLiteral {
		lit := expr as ast.IntegerLiteral
		if lit.val.int() < 0 {
			c.error('array ${para} can not be negative', lit.pos)
		}
	}
}

// When the fixed array has multiple dimensions, it needs to be evaluated recursively.
// `[const]int`, `[const][3]int`, `[3][const]int`, `[const + 1][3][const]int`...
fn (mut c Checker) eval_array_fixed_sizes(mut size_expr ast.Expr, size int, elem_type ast.Type) ast.Type {
	elem_sym := c.table.sym(elem_type)
	elem_info := elem_sym.info

	new_elem_typ := if elem_sym.kind == .array_fixed {
		mut info := elem_info as ast.ArrayFixed
		mut elem_size_expr := unsafe { info.size_expr }
		c.eval_array_fixed_sizes(mut elem_size_expr, info.size, info.elem_type)
	} else {
		elem_type
	}

	mut fixed_size := i64(size)
	if fixed_size <= 0 {
		c.expr(mut size_expr)
		match mut size_expr {
			ast.IntegerLiteral {
				fixed_size = size_expr.val.int()
			}
			ast.ComptimeCall {
				if size_expr.is_compile_value {
					size_expr.resolve_compile_value(c.pref.compile_values) or {
						c.error(err.msg(), size_expr.pos)
					}
					if size_expr.result_type != ast.i64_type {
						c.error('value from \$d() can only be positive integers when used as fixed size',
							size_expr.pos)
					}
					fixed_size = size_expr.compile_value.int()
				} else {
					c.error('only \$d() can be used for fixed size arrays', size_expr.pos)
				}
			}
			ast.CastExpr {
				if !size_expr.typ.is_pure_int() {
					c.error('only integer types are allowed', size_expr.pos)
				}
				match mut size_expr.expr {
					ast.IntegerLiteral {
						fixed_size = size_expr.expr.val.int()
					}
					ast.EnumVal {
						if val := c.table.find_enum_field_val(size_expr.expr.enum_name,
							size_expr.expr.val)
						{
							fixed_size = val
						}
					}
					else {}
				}
			}
			ast.EnumVal {
				c.error('${size_expr.enum_name}.${size_expr.val} has to be casted to integer to be used as size',
					size_expr.pos)
			}
			ast.Ident {
				if mut size_expr.obj is ast.ConstField {
					if mut size_expr.obj.expr is ast.EnumVal {
						c.error('${size_expr.obj.expr.enum_name}.${size_expr.obj.expr.val} has to be casted to integer to be used as size',
							size_expr.pos)
					}
					if mut size_expr.obj.expr is ast.CastExpr {
						if !size_expr.obj.expr.typ.is_pure_int() {
							c.error('only integer types are allowed', size_expr.pos)
						}
						if size_expr.obj.expr.expr is ast.IntegerLiteral {
							if comptime_value := c.eval_comptime_const_expr(size_expr.obj.expr.expr,
								0)
							{
								fixed_size = comptime_value.i64() or { fixed_size }
							}
						}
						if size_expr.obj.expr.expr is ast.InfixExpr {
							if comptime_value := c.eval_comptime_const_expr(size_expr.obj.expr.expr,
								0)
							{
								fixed_size = comptime_value.i64() or { fixed_size }
							}
						}
					}
					if comptime_value := c.eval_comptime_const_expr(size_expr.obj.expr,
						0)
					{
						fixed_size = comptime_value.i64() or { fixed_size }
					}
				} else {
					c.error('non-constant array bound `${size_expr.name}`', size_expr.pos)
				}
			}
			ast.InfixExpr {
				if comptime_value := c.eval_comptime_const_expr(size_expr, 0) {
					fixed_size = comptime_value.i64() or { fixed_size }
				}
			}
			else {
				c.error('fixed array size cannot use non-constant value', size_expr.pos())
			}
		}
		if fixed_size <= 0 {
			c.error('fixed size cannot be zero or negative (fixed_size: ${fixed_size})',
				size_expr.pos())
		}
	}

	idx := c.table.find_or_register_array_fixed(new_elem_typ, int(fixed_size), size_expr,
		false)
	return if elem_type.has_flag(.generic) {
		ast.new_type(idx).set_flag(.generic)
	} else {
		ast.new_type(idx)
	}
}

fn (mut c Checker) array_fixed_has_unresolved_size(info &ast.ArrayFixed) bool {
	if info.size <= 0 {
		return true
	}
	mut elem_type := info.elem_type
	mut elem_sym := c.table.sym(elem_type)
	for {
		if mut elem_sym.info is ast.ArrayFixed {
			if elem_sym.info.size <= 0 {
				return true
			}
			elem_type = elem_sym.info.elem_type
			elem_sym = c.table.sym(elem_type)
		} else {
			break
		}
	}
	return false
}

fn (mut c Checker) map_init(mut node ast.MapInit) ast.Type {
	// `map = {}`
	if node.keys.len == 0 && node.vals.len == 0 && !node.has_update_expr && node.typ == 0 {
		sym := c.table.sym(c.expected_type)
		if sym.kind == .map {
			info := sym.map_info()
			node.typ = c.expected_type.clear_option_and_result()
			node.key_type = info.key_type
			node.value_type = info.value_type
			return node.typ
		} else if sym.info is ast.Struct {
			msg := if sym.info.is_anon {
				'`{}` cannot be used to initialize anonymous structs. Use `struct{}` instead.'
			} else {
				'`{}` can not be used for initialising empty structs any more. Use `${c.table.type_to_str(c.expected_type)}{}` instead.'
			}
			c.error(msg, node.pos)
			if sym.info.is_anon {
				return c.expected_type
			}
		} else {
			c.error('invalid empty map initialisation syntax, use e.g. map[string]int{} instead',
				node.pos)
		}
		return ast.void_type
	}
	// `x := map[string]string` - set in parser
	if node.typ != 0 {
		info := c.table.sym(node.typ).map_info()
		if info.value_type != 0 {
			if info.value_type.has_flag(.result) {
				c.error('cannot use Result type as map value type', node.pos)
			}
			val_sym := c.table.sym(info.value_type)
			if val_sym.kind == .struct {
				val_info := val_sym.info as ast.Struct
				if val_info.generic_types.len > 0 && val_info.concrete_types.len == 0
					&& !info.value_type.has_flag(.generic) {
					if c.table.cur_concrete_types.len == 0 {
						c.error('generic struct `${val_sym.name}` must specify type parameter, e.g. ${val_sym.name}[int]',
							node.pos)
					} else {
						c.error('generic struct `${val_sym.name}` must specify type parameter, e.g. ${val_sym.name}[T]',
							node.pos)
					}
				}
			} else if val_sym.info is ast.FnType {
				for param in val_sym.info.func.params {
					if param.typ.has_flag(.result) {
						c.error('result type arguments are not supported', node.pos)
					}
				}
			}
		}
		c.ensure_type_exists(info.key_type, node.pos)
		c.ensure_type_exists(info.value_type, node.pos)
		node.key_type = info.key_type
		node.value_type = info.value_type
		if (c.table.sym(info.key_type).language == .v && info.key_type == ast.any_type)
			|| (c.table.sym(info.value_type).language == .v && info.value_type == ast.any_type) {
			c.note('the `any` type is deprecated and will be removed soon - either use an empty interface, or a sum type',
				node.pos)
			c.error('cannot use type `any` here', node.pos)
		}
		return node.typ
	}

	if (node.keys.len > 0 && node.vals.len > 0) || node.has_update_expr {
		c.table.used_features.map_update = true
		mut map_type := ast.void_type
		use_expected_type := c.expected_type != ast.void_type && !c.inside_const
			&& c.table.sym(c.expected_type).kind == .map && !(c.inside_fn_arg
			&& c.expected_type.has_flag(.generic))
		if use_expected_type {
			map_type = c.expected_type
		}
		if node.has_update_expr {
			update_type := c.expr(mut node.update_expr)
			if map_type != ast.void_type {
				if update_type != map_type {
					msg := c.expected_msg(update_type, map_type)
					c.error('invalid map update: ${msg}', node.update_expr_pos)
				}
			} else if c.table.sym(update_type).kind != .map {
				c.error('invalid map update: non-map type', node.update_expr_pos)
			} else {
				map_type = update_type
			}
		}

		mut map_key_type := ast.void_type
		mut map_val_type := ast.void_type
		if map_type != ast.void_type {
			sym := c.table.sym(map_type)
			info := sym.map_info()
			map_key_type = info.key_type
			map_val_type = info.value_type
		} else if node.keys.len > 0 {
			// `{'age': 20}`
			mut key_ := node.keys[0]
			map_key_type = ast.mktyp(c.expr(mut key_))
			if node.keys[0].is_auto_deref_var() {
				map_key_type = map_key_type.deref()
			}
			mut val_ := node.vals[0]
			map_val_type = ast.mktyp(c.expr(mut val_))
			if node.vals[0].is_auto_deref_var() {
				map_val_type = map_val_type.deref()
			}
			node.val_types << map_val_type
			if node.keys.len == 1 && map_val_type == ast.none_type {
				c.error('map value cannot be only `none`', node.vals[0].pos())
			}
			c.check_expr_option_or_result_call(key_, map_key_type)
			c.check_expr_option_or_result_call(val_, map_val_type)
		}
		map_key_type = c.unwrap_generic(map_key_type)
		map_val_type = c.unwrap_generic(map_val_type)

		node.typ = ast.new_type(c.table.find_or_register_map(map_key_type, map_val_type))
		node.key_type = map_key_type
		node.value_type = map_val_type

		map_value_sym := c.table.sym(map_val_type)
		expecting_interface_map := map_value_sym.kind == .interface
		mut same_key_type := true
		for i, mut key in node.keys {
			if i == 0 && map_type == ast.void_type {
				continue // skip first key/value if we processed them above
			}
			mut val := node.vals[i]
			c.expected_type = map_key_type
			key_type := c.expr(mut key)
			c.expected_type = map_val_type
			val_type := c.expr(mut val)
			node.val_types << val_type
			val_type_sym := c.table.sym(val_type)
			c.check_expr_option_or_result_call(key, key_type)
			c.check_expr_option_or_result_call(val, val_type)
			if !c.check_types(key_type, map_key_type)
				|| (i == 0 && key_type.is_number() && map_key_type.is_number()
				&& map_key_type != ast.mktyp(key_type)) {
				msg := c.expected_msg(key_type, map_key_type)
				c.error('invalid map key: ${msg}', key.pos())
				same_key_type = false
			}
			if expecting_interface_map {
				if val_type == map_val_type {
					continue
				}
				if val_type_sym.kind == .struct
					&& c.type_implements(val_type, map_val_type, val.pos()) {
					node.vals[i] = ast.CastExpr{
						expr:      val
						typname:   c.table.get_type_name(map_val_type)
						typ:       map_val_type
						expr_type: val_type
						pos:       val.pos()
					}
					continue
				} else {
					msg := c.expected_msg(val_type, map_val_type)
					c.error('invalid map value: ${msg}', val.pos())
				}
			}
			if val_type == ast.none_type && map_val_type.has_flag(.option) {
				continue
			}
			if !c.check_types(val_type, map_val_type)
				|| map_val_type.has_flag(.option) != val_type.has_flag(.option)
				|| (i == 0 && val_type.is_number() && map_val_type.is_number()
				&& map_val_type != ast.mktyp(val_type)) {
				msg := c.expected_msg(val_type, map_val_type)
				c.error('invalid map value: ${msg}', val.pos())
			}
		}
		if same_key_type {
			for i in 1 .. node.keys.len {
				c.check_dup_keys(node, i)
			}
		}
	}
	return node.typ
}

// check the element, and its children for ref uninitialized fields
fn (mut c Checker) check_elements_ref_fields_initialized(typ ast.Type, pos &token.Pos) {
	if typ == 0 || c.inside_const {
		return
	}
	sym := c.table.sym(typ)
	mut checked_types := []ast.Type{}
	c.do_check_elements_ref_fields_initialized(sym, mut checked_types, pos)
}

// Recursively check the element, and its children for ref uninitialized fields
fn (mut c Checker) do_check_elements_ref_fields_initialized(sym &ast.TypeSymbol, mut checked_types []ast.Type,
	pos &token.Pos) {
	if sym.info is ast.Struct {
		linked_name := sym.name
		// For now, let's call this method and give a notice instead of an error.
		// After some time, we remove the check_ref_fields_initialized_note() method and
		// simply call check_ref_fields_initialized()
		c.check_ref_fields_initialized_note(sym, mut checked_types, linked_name, pos)
		return
	}
	match sym.info {
		ast.Array {
			elem_type := sym.info.elem_type
			if elem_type in checked_types {
				return
			}
			checked_types << elem_type
			elem_sym := c.table.sym(elem_type)
			c.do_check_elements_ref_fields_initialized(elem_sym, mut checked_types, pos)
		}
		ast.ArrayFixed {
			elem_type := sym.info.elem_type
			if elem_type in checked_types {
				return
			}
			checked_types << elem_type
			elem_sym := c.table.sym(elem_type)
			c.do_check_elements_ref_fields_initialized(elem_sym, mut checked_types, pos)
		}
		ast.Map {
			key_type := sym.info.key_type
			if key_type in checked_types {
				return
			}
			checked_types << key_type
			key_sym := c.table.sym(key_type)
			c.do_check_elements_ref_fields_initialized(key_sym, mut checked_types, pos)
			value_type := sym.info.value_type
			if value_type in checked_types {
				return
			}
			checked_types << value_type
			value_sym := c.table.sym(value_type)
			c.do_check_elements_ref_fields_initialized(value_sym, mut checked_types, pos)
		}
		ast.Alias {
			parent_type := sym.info.parent_type
			if parent_type in checked_types {
				return
			}
			checked_types << parent_type
			parent_sym := c.table.sym(parent_type)
			c.do_check_elements_ref_fields_initialized(parent_sym, mut checked_types,
				pos)
		}
		else {}
	}
}

const err_ref_uninitialized = error('arrays of references need to be initialized right away')
const err_interface_uninitialized = error('arrays of interfaces need to be initialized right away')
const err_sumtype_uninitialized = error('arrays of sumtypes need to be initialized right away')

// check the element, and its children for `ref/interface/sumtype` initialized
fn (mut c Checker) check_elements_initialized(typ ast.Type) ! {
	if typ == 0 || c.inside_unsafe {
		return
	}
	if typ.is_any_kind_of_pointer() {
		if !c.pref.translated && !c.file.is_translated {
			return err_ref_uninitialized
		} else {
			return
		}
	}
	sym := c.table.sym(typ)
	if sym.kind == .interface {
		return err_interface_uninitialized
	} else if sym.kind == .sum_type {
		return err_sumtype_uninitialized
	}

	match sym.info {
		ast.Array {
			elem_type := sym.info.elem_type
			return c.check_elements_initialized(elem_type)
		}
		ast.ArrayFixed {
			elem_type := sym.info.elem_type
			if !c.is_builtin_mod {
				return c.check_elements_initialized(elem_type)
			}
		}
		ast.Map {
			value_type := sym.info.value_type
			if !c.is_builtin_mod {
				return c.check_elements_initialized(value_type)
			}
		}
		ast.Alias {
			parent_type := sym.info.parent_type
			return c.check_elements_initialized(parent_type)
		}
		else {}
	}
}

fn (mut c Checker) check_append(mut node ast.InfixExpr, left_type ast.Type, right_type ast.Type,
	right_final_sym ast.TypeSymbol) ast.Type {
	if !node.is_stmt {
		c.error('array append cannot be used in an expression', node.pos)
	}
	if left_type.has_flag(.option) && node.left is ast.Ident && node.left.or_expr.kind == .absent {
		c.error('unwrapped Option cannot be used in an infix expression', node.pos)
	}
	right_pos := node.right.pos()
	mut right_sym := c.table.sym(right_type)
	mut left_sym := c.table.sym(left_type)
	// `array << elm`
	c.check_expr_option_or_result_call(node.right, right_type)
	node.auto_locked, _ = c.fail_if_immutable(mut node.left)
	left_value_type := c.table.value_type(c.unwrap_generic(left_type))
	left_value_sym := c.table.sym(c.unwrap_generic(left_value_type))
	if !left_value_type.has_flag(.option) && right_type.has_flag(.option) {
		c.error('unwrapped Option cannot be used in an infix expression', node.pos)
	}

	right := node.right
	if right is ast.PrefixExpr && right.op == .amp {
		mut expr2 := right.right
		if mut expr2 is ast.Ident && !node.left.is_blank_ident() && expr2.obj is ast.ConstField {
			c.error('cannot have mutable reference to const `${expr2.name}`', expr2.pos)
		}
	}
	if left_value_sym.kind == .interface {
		if right_final_sym.kind != .array {
			// []Animal << Cat
			if c.type_implements(right_type, left_value_type, right_pos) {
				if !right_type.is_any_kind_of_pointer() && !c.inside_unsafe
					&& right_sym.kind != .interface {
					c.mark_as_referenced(mut &node.right, true)
				}
			}
		} else {
			// []Animal << []Cat
			c.type_implements(c.table.value_type(right_type), left_value_type, right_pos)
		}
		return ast.void_type
	} else if left_value_sym.kind == .sum_type {
		if right_sym.kind != .array {
			if !c.table.is_sumtype_or_in_variant(left_value_type, ast.mktyp(c.unwrap_generic(right_type))) {
				c.error('cannot append `${right_sym.name}` to `${left_sym.name}`', right_pos)
			}
		} else {
			right_value_type := c.table.value_type(c.unwrap_generic(right_type))
			if !c.table.is_sumtype_or_in_variant(left_value_type, ast.mktyp(right_value_type)) {
				c.error('cannot append `${right_sym.name}` to `${left_sym.name}`', right_pos)
			}
		}
		return ast.void_type
	}
	// []T << T or []T << []T
	unwrapped_right_type := c.unwrap_generic(right_type)
	if c.check_types(unwrapped_right_type, left_value_type) {
		// []&T << T is wrong: we check for that, !(T.is_ptr()) && ?(&T).is_ptr()
		if !(!unwrapped_right_type.is_ptr() && left_value_type.is_ptr()
			&& left_value_type.share() == .mut_t) {
			return ast.void_type
		}
	} else if c.check_types(unwrapped_right_type, c.unwrap_generic(left_type)) {
		return ast.void_type
	}
	if left_value_type.has_flag(.option) && right_type == ast.none_type {
		return ast.void_type
	}
	c.error('cannot append `${right_sym.name}` to `${left_sym.name}`', right_pos)
	return ast.void_type
}
