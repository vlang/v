// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.types
import strings

fn is_empty_expr(e ast.Expr) bool {
	return e is ast.EmptyExpr
}

fn is_none_expr(expr ast.Expr) bool {
	match expr {
		ast.Keyword {
			return expr.tok == .key_none
		}
		ast.Ident {
			return expr.name == 'none'
		}
		else {
			return false
		}
	}
}

fn is_none_like_expr(expr ast.Expr) bool {
	if is_none_expr(expr) {
		return true
	}
	if expr is ast.Type && expr is ast.NoneType {
		return true
	}
	return false
}

fn (mut g Gen) gen_interface_cast(type_name string, value_expr ast.Expr) bool {
	// Look up the type in the environment to check if it's an interface
	if g.env == unsafe { nil } {
		return false
	}
	mut is_iface := false
	if mut scope := g.env_scope(g.cur_module) {
		if obj := scope.lookup_parent(type_name, 0) {
			if obj is types.Type && obj is types.Interface {
				is_iface = true
			}
		}
	}
	if !is_iface {
		return false
	}
	// Get the concrete type name
	concrete_type := g.get_expr_type(value_expr)
	if concrete_type == '' || concrete_type == 'int' {
		return false
	}
	// Strip pointer suffix for method name construction
	base_concrete := if concrete_type.ends_with('*') {
		concrete_type[..concrete_type.len - 1]
	} else {
		concrete_type
	}
	// Generate: (InterfaceType){._object = (void*)&expr, .method = ConcreteType__method, ...}
	g.sb.write_string('((${type_name}){._object = ')
	if concrete_type.ends_with('*') {
		// Value is already a pointer-compatible receiver.
		g.sb.write_string('(void*)(')
		g.expr(value_expr)
		g.sb.write_string(')')
	} else {
		g.sb.write_string('(void*)&(')
		g.expr(value_expr)
		g.sb.write_string(')')
	}
	type_short := if base_concrete.contains('__') {
		base_concrete.all_after_last('__')
	} else {
		base_concrete
	}
	type_id := interface_type_id_for_name(type_short)
	g.sb.write_string(', ._type_id = ${type_id}')
	// Generate method function pointers from stored interface info
	if methods := g.interface_methods[type_name] {
		for method in methods {
			fn_name := '${base_concrete}__${method.name}'
			mut target_name := fn_name
			if ptr_params := g.fn_param_is_ptr[fn_name] {
				if ptr_params.len > 0 && !ptr_params[0] {
					target_name = interface_wrapper_name(type_name, base_concrete, method.name)
					g.needed_interface_wrappers[target_name] = true
					if target_name !in g.interface_wrapper_specs {
						g.interface_wrapper_specs[target_name] = InterfaceWrapperSpec{
							fn_name:       fn_name
							concrete_type: base_concrete
							method:        method
						}
					}
				}
			}
			g.sb.write_string(', .${method.name} = (${method.cast_signature})${target_name}')
		}
	}
	g.sb.write_string('})')
	return true
}

fn c_static_v_string_expr(raw string) string {
	return c_static_v_string_expr_from_c_literal(c_string_literal_content_to_c(raw))
}

fn c_empty_v_string_expr() string {
	return c_v_string_expr_from_ptr_len('""', '0', true)
}

fn expr_to_int_str(e ast.Expr) string {
	if e is ast.BasicLiteral {
		return e.value
	}
	return '0'
}

fn (mut g Gen) gen_unwrapped_value_expr(expr ast.Expr) bool {
	expr_type := g.get_expr_type(expr)
	if expr_type.starts_with('_result_') {
		base := g.result_value_type(expr_type)
		if base != '' && base != 'void' {
			is_addressable := match expr {
				ast.Ident, ast.SelectorExpr, ast.IndexExpr {
					true
				}
				else {
					false
				}
			}
			if is_addressable {
				g.sb.write_string('(*(${base}*)(((u8*)(&')
				g.expr(expr)
				g.sb.write_string('.err)) + sizeof(IError)))')
			} else {
				g.sb.write_string('({ ${expr_type} _tmp = ')
				g.expr(expr)
				g.sb.write_string('; (*(${base}*)(((u8*)(&_tmp.err)) + sizeof(IError))); })')
			}
			return true
		}
	}
	if expr_type.starts_with('_option_') {
		base := option_value_type(expr_type)
		if base != '' && base != 'void' {
			is_addressable := match expr {
				ast.Ident, ast.SelectorExpr, ast.IndexExpr {
					true
				}
				else {
					false
				}
			}
			if is_addressable {
				g.sb.write_string('(*(${base}*)(((u8*)(&')
				g.expr(expr)
				g.sb.write_string('.err)) + sizeof(IError)))')
			} else {
				g.sb.write_string('({ ${expr_type} _tmp = ')
				g.expr(expr)
				g.sb.write_string('; (*(${base}*)(((u8*)(&_tmp.err)) + sizeof(IError))); })')
			}
			return true
		}
	}
	return false
}

// Helper to extract FnType from an Expr (handles ast.Type wrapping)
fn (mut g Gen) expr(node ast.Expr) {
	match node {
		ast.BasicLiteral {
			if node.kind == .key_true {
				g.sb.write_string('true')
			} else if node.kind == .key_false {
				g.sb.write_string('false')
			} else if node.kind == .char {
				raw := strip_literal_quotes(node.value)
				if raw.len > 1 && raw[0] != `\\` {
					// Multi-byte UTF-8 character: emit as numeric codepoint
					runes := raw.runes()
					if runes.len > 0 {
						g.sb.write_string(int(runes[0]).str())
					} else {
						g.sb.write_string("'${raw}'")
					}
				} else {
					escaped := escape_char_literal_content(raw)
					g.sb.write_u8(`'`)
					g.sb.write_string(escaped)
					g.sb.write_u8(`'`)
				}
			} else {
				g.sb.write_string(sanitize_c_number_literal(node.value))
			}
		}
		ast.StringLiteral {
			val := strip_literal_quotes(node.value)
			c_lit := c_string_literal_content_to_c(val)
			if node.kind == .c {
				// C string literal: emit raw C string
				g.sb.write_string(c_lit)
			} else {
				// Use sizeof on the emitted C literal so escape sequences
				// (e.g. `\t`) get the correct runtime byte length.
				g.sb.write_string(c_static_v_string_expr_from_c_literal(c_lit))
			}
		}
		ast.Ident {
			g.mark_needed_ierror_wrapper_from_ident(node.name)
			if node.name == 'nil' {
				g.sb.write_string('NULL')
			} else if node.name == '@FN' || node.name == '@METHOD' || node.name == '@FUNCTION' {
				fn_name := g.cur_fn_name
				g.sb.write_string(c_static_v_string_expr(fn_name))
			} else if node.name == '@MOD' {
				mod_name := g.cur_module
				g.sb.write_string(c_static_v_string_expr(mod_name))
			} else if node.name == '@FILE' {
				g.sb.write_string(c_v_string_expr_from_ptr_len('__FILE__', 'sizeof(__FILE__)-1',
					true))
			} else if node.name == '@LINE' {
				g.sb.write_string('__LINE__')
			} else if node.name == '@VCURRENTHASH' || node.name == '@VHASH' {
				hash_name := if node.name == '@VHASH' { 'VHASH' } else { 'VCURRENTHASH' }
				g.sb.write_string(c_static_v_string_expr(hash_name))
			} else if node.name == '@VEXE' {
				g.sb.write_string(c_empty_v_string_expr())
			} else if node.name.starts_with('__type_id_') {
				type_name := node.name['__type_id_'.len..]
				type_id := interface_type_id_for_name(type_name)
				g.sb.write_string('${type_id}')
			} else {
				// Check global_var_modules first - globals may appear as types.Type in scope
				// instead of types.Global, so is_local_var check would incorrectly block them
				if node.name in g.global_var_modules
					&& g.global_var_modules[node.name] == g.cur_module {
					g.sb.write_string('${g.cur_module}__${node.name}')
				} else {
					is_local_var := g.get_local_var_c_type(node.name) != none
					const_key := 'const_${g.cur_module}__${node.name}'
					global_key := 'global_${g.cur_module}__${node.name}'
					if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
						&& !node.name.contains('__') && !is_local_var
						&& ((const_key in g.emitted_types || global_key in g.emitted_types)
						|| g.is_module_local_const_or_global(node.name)) {
						g.sb.write_string('${g.cur_module}__${node.name}')
					} else if g.cur_module != '' && g.cur_module != 'main'
						&& g.cur_module != 'builtin' && !node.name.contains('__') && !is_local_var
						&& !g.is_module_ident(node.name) && g.is_module_local_fn(node.name)
						&& !g.is_type_name(node.name) {
						g.sb.write_string('${g.cur_module}__${sanitize_fn_ident(node.name)}')
					} else {
						mut ident_name := node.name
						if g.cur_module != '' {
							double_prefix := '${g.cur_module}__${g.cur_module}__'
							if ident_name.starts_with(double_prefix) {
								ident_name = ident_name[g.cur_module.len + 2..]
							}
						}
						// Rename V variables that clash with C type names
						if ident_name == 'array' {
							ident_name = '_v_array'
						}
						g.sb.write_string(ident_name)
					}
				}
			}
		}
		ast.ParenExpr {
			g.sb.write_string('(')
			g.expr(node.expr)
			g.sb.write_string(')')
		}
		ast.InfixExpr {
			lhs_type := g.get_expr_type(node.lhs)
			rhs_type := g.get_expr_type(node.rhs)
			// Option none comparison: `opt == none` / `opt != none`.
			if node.op in [.eq, .ne] {
				if lhs_type.starts_with('_option_') && is_none_like_expr(node.rhs) {
					sep := if g.expr_is_pointer(node.lhs) { '->' } else { '.' }
					cmp := if node.op == .eq { '!=' } else { '==' }
					g.sb.write_string('(')
					g.expr(node.lhs)
					g.sb.write_string('${sep}state ${cmp} 0)')
					return
				}
				if rhs_type.starts_with('_option_') && is_none_like_expr(node.lhs) {
					sep := if g.expr_is_pointer(node.rhs) { '->' } else { '.' }
					cmp := if node.op == .eq { '!=' } else { '==' }
					g.sb.write_string('(')
					g.expr(node.rhs)
					g.sb.write_string('${sep}state ${cmp} 0)')
					return
				}
			}
			if node.op in [.eq, .ne, .key_is, .not_is]
				&& ((node.rhs is ast.Ident && node.rhs.name == 'Eof')
				|| (node.rhs is ast.SelectorExpr && node.rhs.rhs.name == 'Eof')) {
				if node.op in [.ne, .not_is] {
					g.sb.write_string('!')
				}
				g.sb.write_string('string__eq(err.type_name(err._object), ${c_static_v_string_expr('Eof')})')
				return
			}
			if node.op in [.eq, .ne, .key_is, .not_is] && node.lhs is ast.Ident
				&& node.lhs.name == 'err' && node.rhs is ast.Ident {
				rhs_ident := node.rhs as ast.Ident
				if rhs_ident.name.len > 0 && rhs_ident.name[0] >= `A` && rhs_ident.name[0] <= `Z` {
					if node.op in [.ne, .not_is] {
						g.sb.write_string('!')
					}
					type_name := rhs_ident.name
					g.sb.write_string('string__eq(err.type_name(err._object), ${c_static_v_string_expr(type_name)})')
					return
				}
			}
			if node.op in [.eq, .ne, .key_is, .not_is] && lhs_type == 'IError'
				&& node.rhs is ast.Ident {
				rhs_ident := node.rhs as ast.Ident
				if g.is_type_name(rhs_ident.name) || (rhs_ident.name.len > 0
					&& rhs_ident.name[0] >= `A` && rhs_ident.name[0] <= `Z`) {
					if node.op in [.ne, .not_is] {
						g.sb.write_string('!')
					}
					type_name := rhs_ident.name
					g.sb.write_string('string__eq(')
					g.expr(node.lhs)
					g.sb.write_string('.type_name(')
					g.expr(node.lhs)
					g.sb.write_string('._object), ${c_static_v_string_expr(type_name)})')
					return
				}
			}
			if node.op in [.key_is, .not_is, .eq, .ne] {
				if raw_type := g.get_raw_type(node.lhs) {
					is_iface := raw_type is types.Interface
						|| (raw_type is types.Pointer && raw_type.base_type is types.Interface)
					if is_iface {
						mut rhs_name := ''
						if node.rhs is ast.Ident {
							rhs_name = node.rhs.name
						} else if node.rhs is ast.SelectorExpr {
							rhs_name = node.rhs.rhs.name
						}
						type_id := interface_type_id_for_name(rhs_name)
						if type_id <= 0 {
							g.sb.write_string(if node.op in [.key_is, .eq] {
								'false'
							} else {
								'true'
							})
							return
						}
						sep := if g.expr_is_pointer(node.lhs) { '->' } else { '.' }
						g.sb.write_string('(')
						g.expr(node.lhs)
						op := if node.op in [.key_is, .eq] { '==' } else { '!=' }
						g.sb.write_string('${sep}_type_id ${op} ${type_id})')
						return
					}
				}
			}
			// Sum type checks: expr is Type and lowered expr ==/!= Type.
			rhs_can_match_sum := node.rhs is ast.Ident
				|| (node.rhs is ast.SelectorExpr && node.rhs.lhs is ast.Ident)
			if node.op in [.key_is, .not_is] || (node.op in [.eq, .ne] && rhs_can_match_sum) {
				mut rhs_name := ''
				if node.rhs is ast.Ident {
					rhs_name = node.rhs.name
				} else if node.rhs is ast.SelectorExpr && node.rhs.lhs is ast.Ident {
					rhs_name = '${(node.rhs.lhs as ast.Ident).name}__${node.rhs.rhs.name}'
				}
				if rhs_name != '' {
					// Find sum type by checking LHS type
					mut lhs_sum_type := g.get_expr_type(node.lhs)
					if raw_lhs := g.get_raw_type(node.lhs) {
						match raw_lhs {
							types.SumType {
								lhs_sum_type = g.types_type_to_c(raw_lhs)
							}
							types.Pointer {
								if raw_lhs.base_type is types.SumType {
									lhs_sum_type = g.types_type_to_c(raw_lhs.base_type)
								}
							}
							types.Alias {
								if raw_lhs.base_type is types.SumType {
									lhs_sum_type = g.types_type_to_c(raw_lhs.base_type)
								}
							}
							else {}
						}
					}
					if lhs_sum_type == '' {
						if lhs_env_type := g.get_expr_type_from_env(node.lhs) {
							lhs_sum_type = lhs_env_type
						}
					}
					if lhs_sum_type == '' && node.lhs is ast.SelectorExpr {
						lhs_sum_type = g.selector_field_type(node.lhs)
					}
					lhs_sum_type = lhs_sum_type.trim_space().trim_right('*')
					mut variants := []string{}
					if vs := g.sum_type_variants[lhs_sum_type] {
						variants = vs.clone()
					} else if lhs_sum_type.contains('__') {
						short_sum := lhs_sum_type.all_after_last('__')
						if vs := g.sum_type_variants[short_sum] {
							variants = vs.clone()
						}
					} else {
						qualified_sum := g.get_qualified_name(lhs_sum_type)
						if vs := g.sum_type_variants[qualified_sum] {
							variants = vs.clone()
						}
					}
					if variants.len == 0 && node.lhs is ast.SelectorExpr {
						lhs_sum_type = g.selector_field_type(node.lhs).trim_space().trim_right('*')
						if lhs_sum_type != '' {
							if vs := g.sum_type_variants[lhs_sum_type] {
								variants = vs.clone()
							} else if lhs_sum_type.contains('__') {
								short_sum := lhs_sum_type.all_after_last('__')
								if vs := g.sum_type_variants[short_sum] {
									variants = vs.clone()
								}
							} else {
								qualified_sum := g.get_qualified_name(lhs_sum_type)
								if vs := g.sum_type_variants[qualified_sum] {
									variants = vs.clone()
								}
							}
						}
					}
					if variants.len > 0 {
						mut tag := -1
						for i, v in variants {
							v_short := if v.contains('__') { v.all_after_last('__') } else { v }
							if v == rhs_name || v_short == rhs_name
								|| rhs_name.ends_with('__${v_short}') {
								tag = i
								break
							}
						}
						if tag >= 0 {
							sep := if g.expr_is_pointer(node.lhs) { '->' } else { '.' }
							op := if node.op in [.key_is, .eq] { '==' } else { '!=' }
							g.sb.write_string('(')
							g.expr(node.lhs)
							g.sb.write_string('${sep}_tag ${op} ${tag})')
							return
						}
					}
				}
			}
			if node.op == .left_shift {
				is_array_append, elem_type := g.array_append_elem_type(node.lhs, node.rhs)
				if is_array_append {
					if g.expr_is_array_value(node.rhs) {
						rhs_tmp := '_arr_append_tmp_${g.tmp_counter}'
						g.tmp_counter++
						arr_rhs_type := g.expr_array_runtime_type(node.rhs)
						g.sb.write_string('({ ${arr_rhs_type} ${rhs_tmp} = ')
						g.expr(node.rhs)
						g.sb.write_string('; array__push_many((array*)')
						if g.expr_is_pointer(node.lhs) {
							g.expr(node.lhs)
						} else {
							g.sb.write_string('&')
							g.expr(node.lhs)
						}
						g.sb.write_string(', ${rhs_tmp}.data, ${rhs_tmp}.len); })')
						return
					}
					g.sb.write_string('array__push((array*)')
					if g.expr_is_pointer(node.lhs) {
						g.expr(node.lhs)
					} else {
						g.sb.write_string('&')
						g.expr(node.lhs)
					}
					g.sb.write_string(', ')
					g.gen_addr_of_expr(node.rhs, elem_type)
					g.sb.write_string(')')
					return
				}
			}
			if node.op == .plus && lhs_type == 'string' && rhs_type == 'string' {
				g.sb.write_string('string__plus(')
				g.expr(node.lhs)
				g.sb.write_string(', ')
				g.expr(node.rhs)
				g.sb.write_string(')')
				return
			}
			if node.op in [.key_in, .not_in] {
				if node.rhs is ast.ArrayInitExpr {
					join_op := if node.op == .key_in { ' || ' } else { ' && ' }
					g.sb.write_string('(')
					if node.rhs.exprs.len == 0 {
						g.sb.write_string(if node.op == .key_in { 'false' } else { 'true' })
					} else {
						for i, elem in node.rhs.exprs {
							if i > 0 {
								g.sb.write_string(join_op)
							}
							if lhs_type == 'string' {
								if node.op == .not_in {
									g.sb.write_string('!')
								}
								g.sb.write_string('string__eq(')
								g.expr(node.lhs)
								g.sb.write_string(', ')
								g.expr(elem)
								g.sb.write_string(')')
							} else {
								cmp_op := if node.op == .key_in { '==' } else { '!=' }
								g.sb.write_string('(')
								g.expr(node.lhs)
								g.sb.write_string(' ${cmp_op} ')
								g.expr(elem)
								g.sb.write_string(')')
							}
						}
					}
					g.sb.write_string(')')
					return
				}
				if rhs_type == 'map' || rhs_type.starts_with('Map_') {
					key_type := if lhs_type == '' { 'int' } else { lhs_type }
					if node.op == .not_in {
						g.sb.write_string('!')
					}
					if node.rhs is ast.Ident || node.rhs is ast.SelectorExpr {
						g.sb.write_string('map__exists(&')
						g.expr(node.rhs)
						g.sb.write_string(', ')
						g.gen_addr_of_expr(node.lhs, key_type)
						g.sb.write_string(')')
					} else {
						tmp_name := '_map_in_tmp_${g.tmp_counter}'
						g.tmp_counter++
						g.sb.write_string('({ map ${tmp_name} = ')
						g.expr(node.rhs)
						g.sb.write_string('; map__exists(&${tmp_name}, ')
						g.gen_addr_of_expr(node.lhs, key_type)
						g.sb.write_string('); })')
					}
					return
				}
				if rhs_type == 'array' || rhs_type.starts_with('Array_') {
					key_type := if lhs_type == '' { 'int' } else { lhs_type }
					if node.op == .not_in {
						g.sb.write_string('!')
					}
					g.sb.write_string('array__contains(')
					g.expr(node.rhs)
					g.sb.write_string(', ')
					g.gen_addr_of_expr(node.lhs, key_type)
					g.sb.write_string(')')
					return
				}
				cmp_op := if node.op == .key_in { '==' } else { '!=' }
				g.sb.write_string('(')
				g.expr(node.lhs)
				g.sb.write_string(' ${cmp_op} ')
				g.expr(node.rhs)
				g.sb.write_string(')')
				return
			}
			// Fixed array comparison: use memcmp on plain C arrays
			is_lhs_fixed := lhs_type.starts_with('Array_fixed_')
			is_rhs_fixed := rhs_type.starts_with('Array_fixed_')
			if node.op in [.eq, .ne] && (is_lhs_fixed || is_rhs_fixed) {
				fixed_type := if is_lhs_fixed { lhs_type } else { rhs_type }
				cmp_op := if node.op == .eq { '==' } else { '!=' }
				g.sb.write_string('(memcmp(')
				g.gen_fixed_array_cmp_operand(node.lhs, fixed_type)
				g.sb.write_string(', ')
				g.gen_fixed_array_cmp_operand(node.rhs, fixed_type)
				g.sb.write_string(', sizeof(${fixed_type})) ${cmp_op} 0)')
				return
			}
			is_lhs_array := lhs_type == 'array'
				|| (lhs_type.starts_with('Array_') && !lhs_type.starts_with('Array_fixed_'))
				|| lhs_type in g.array_aliases
			is_rhs_array := rhs_type == 'array'
				|| (rhs_type.starts_with('Array_') && !rhs_type.starts_with('Array_fixed_'))
				|| rhs_type in g.array_aliases
			if node.op in [.eq, .ne] && is_lhs_array && is_rhs_array {
				if node.op == .ne {
					g.sb.write_string('!')
				}
				g.sb.write_string('__v2_array_eq(')
				if g.expr_is_pointer(node.lhs) {
					g.sb.write_string('*')
				}
				g.expr(node.lhs)
				g.sb.write_string(', ')
				if g.expr_is_pointer(node.rhs) {
					g.sb.write_string('*')
				}
				g.expr(node.rhs)
				g.sb.write_string(')')
				return
			}
			// String comparison: use string__eq instead of C == operator.
			// Only apply when we're confident both sides are strings:
			// - at least one side is a string literal, or
			// - both sides are typed as 'string' and neither is an enum type.
			is_string_cmp := if node.lhs is ast.StringLiteral || node.rhs is ast.StringLiteral {
				true
			} else {
				lhs_type == 'string' && rhs_type == 'string' && !g.is_enum_type(lhs_type)
					&& !g.is_enum_type(rhs_type)
			}
			if node.op in [.eq, .ne] && is_string_cmp {
				if node.op == .ne {
					g.sb.write_string('!')
				}
				g.sb.write_string('string__eq(')
				g.expr(node.lhs)
				g.sb.write_string(', ')
				g.expr(node.rhs)
				g.sb.write_string(')')
				return
			}
			// Map comparison: use memcmp on the map struct
			is_lhs_map := lhs_type == 'map' || lhs_type.starts_with('Map_')
			is_rhs_map := rhs_type == 'map' || rhs_type.starts_with('Map_')
			if node.op in [.eq, .ne] && (is_lhs_map || is_rhs_map) {
				g.tmp_counter++
				cmp_l := '_cmp_l_${g.tmp_counter}'
				g.tmp_counter++
				cmp_r := '_cmp_r_${g.tmp_counter}'
				cmp_op := if node.op == .eq { '==' } else { '!=' }
				g.sb.write_string('({ map ${cmp_l} = ')
				g.expr(node.lhs)
				g.sb.write_string('; map ${cmp_r} = ')
				g.expr(node.rhs)
				g.sb.write_string('; memcmp(&${cmp_l}, &${cmp_r}, sizeof(map)) ${cmp_op} 0; })')
				return
			}
			mut cmp_type := ''
			if g.should_use_memcmp_eq(lhs_type, rhs_type) {
				cmp_type = lhs_type
			} else if node.op in [.eq, .ne] {
				lhs_cast_type := extract_compare_cast_type(node.lhs)
				rhs_cast_type := extract_compare_cast_type(node.rhs)
				if lhs_cast_type != '' && rhs_cast_type == '' {
					cmp_type = lhs_cast_type
				} else if rhs_cast_type != '' && lhs_cast_type == '' {
					cmp_type = rhs_cast_type
				} else if lhs_cast_type != '' && lhs_cast_type == rhs_cast_type {
					cmp_type = lhs_cast_type
				}
				if cmp_type in primitive_types || cmp_type == 'string' || cmp_type.ends_with('*')
					|| cmp_type.ends_with('ptr') {
					cmp_type = ''
				}
			}
			if node.op in [.eq, .ne] && cmp_type != '' {
				ltmp := '_cmp_l_${g.tmp_counter}'
				g.tmp_counter++
				rtmp := '_cmp_r_${g.tmp_counter}'
				g.tmp_counter++
				g.sb.write_string('({ ${cmp_type} ${ltmp} = ')
				if !g.gen_unwrapped_value_expr(node.lhs) {
					g.expr(node.lhs)
				}
				g.sb.write_string('; ${cmp_type} ${rtmp} = ')
				if !g.gen_unwrapped_value_expr(node.rhs) {
					g.expr(node.rhs)
				}
				cmp_op := if node.op == .eq { '== 0' } else { '!= 0' }
				g.sb.write_string('; memcmp(&${ltmp}, &${rtmp}, sizeof(${cmp_type})) ${cmp_op}; })')
				return
			}
			g.sb.write_string('(')
			if !g.gen_unwrapped_value_expr(node.lhs) {
				g.expr(node.lhs)
			}
			op := match node.op {
				.plus { '+' }
				.minus { '-' }
				.mul { '*' }
				.div { '/' }
				.mod { '%' }
				.gt { '>' }
				.lt { '<' }
				.eq { '==' }
				.ne { '!=' }
				.ge { '>=' }
				.le { '<=' }
				.and { '&&' }
				.logical_or { '||' }
				.amp { '&' }
				.pipe { '|' }
				.xor { '^' }
				.left_shift { '<<' }
				.right_shift { '>>' }
				.key_is { '==' }
				.not_is { '!=' }
				else { '?' }
			}
			g.sb.write_string(' ${op} ')
			if !g.gen_unwrapped_value_expr(node.rhs) {
				g.expr(node.rhs)
			}
			g.sb.write_string(')')
		}
		ast.PrefixExpr {
			// &T(x) in unsafe contexts is used as a pointer cast in V stdlib code.
			// Emit it as (T*)(x) so `*unsafe { &T(p) }` becomes `*((T*)p)`.
			if node.op == .amp {
				// &&T(x) is a pointer-to-pointer cast pattern used in builtin code.
				// Lower it directly to (T**)(x) instead of taking address of a cast rvalue.
				if node.expr is ast.PrefixExpr && node.expr.op == .amp {
					inner := node.expr as ast.PrefixExpr
					if inner.expr is ast.CastExpr {
						target_type := g.expr_type_to_c(inner.expr.typ)
						g.sb.write_string('((${target_type}**)(')
						g.expr(inner.expr.expr)
						g.sb.write_string('))')
						return
					}
				}
				if node.expr is ast.IndexExpr {
					idx := node.expr as ast.IndexExpr
					if idx.lhs is ast.Ident {
						if idx.lhs.name in g.fixed_array_globals || idx.lhs.name == 'rune_maps' {
							g.sb.write_string('&')
							g.expr(idx.lhs)
							g.sb.write_string('[')
							g.expr(idx.expr)
							g.sb.write_string(']')
							return
						}
						if raw_type := g.get_raw_type(idx.lhs) {
							if raw_type is types.ArrayFixed {
								// Fixed arrays: &arr[i]
								g.sb.write_string('&')
								g.expr(idx.lhs)
								g.sb.write_string('[')
								g.expr(idx.expr)
								g.sb.write_string(']')
								return
							}
						}
						lhs_type := g.get_expr_type(idx.lhs)
						if lhs_type == 'array' || lhs_type.starts_with('Array_') {
							mut elem_type := g.get_expr_type(idx)
							if elem_type == '' || elem_type == 'int' {
								if lhs_type.starts_with('Array_fixed_') {
									if finfo := g.collected_fixed_array_types[lhs_type] {
										elem_type = finfo.elem_type
									}
								} else if lhs_type.starts_with('Array_') {
									elem_type = lhs_type['Array_'.len..].trim_right('*')
								}
							}
							if elem_type == '' {
								elem_type = 'u8'
							}
							g.sb.write_string('&((')
							g.sb.write_string(elem_type)
							g.sb.write_string('*)')
							g.expr(idx.lhs)
							if lhs_type.ends_with('*') {
								g.sb.write_string('->data)[')
							} else {
								g.sb.write_string('.data)[')
							}
							g.expr(idx.expr)
							g.sb.write_string(']')
							return
						}
					}
				}
				if node.expr is ast.CallExpr {
					if node.expr.args.len == 1 && node.expr.lhs is ast.Ident
						&& g.is_type_name(node.expr.lhs.name) {
						target_type := g.expr_type_to_c(node.expr.lhs)
						g.sb.write_string('((${target_type}*)(')
						g.expr(node.expr.args[0])
						g.sb.write_string('))')
						return
					}
					if node.expr.args.len == 1 && node.expr.lhs is ast.SelectorExpr {
						sel := node.expr.lhs as ast.SelectorExpr
						if sel.lhs is ast.Ident && sel.lhs.name == 'C'
							&& g.is_c_type_name(sel.rhs.name) {
							target_type := g.expr_type_to_c(node.expr.lhs)
							g.sb.write_string('((${target_type}*)(')
							g.expr(node.expr.args[0])
							g.sb.write_string('))')
							return
						}
					}
				}
				if node.expr is ast.CastExpr {
					target_type := g.expr_type_to_c(node.expr.typ)
					g.sb.write_string('((${target_type}*)(')
					g.expr(node.expr.expr)
					g.sb.write_string('))')
					return
				}
				if node.expr is ast.ModifierExpr {
					if node.expr.expr is ast.CastExpr {
						target_type := g.expr_type_to_c(node.expr.expr.typ)
						g.sb.write_string('((${target_type}*)(')
						g.expr(node.expr.expr.expr)
						g.sb.write_string('))')
						return
					}
				}
				if node.expr is ast.ParenExpr {
					if node.expr.expr is ast.CastExpr {
						target_type := g.expr_type_to_c(node.expr.expr.typ)
						g.sb.write_string('((${target_type}*)(')
						g.expr(node.expr.expr.expr)
						g.sb.write_string('))')
						return
					}
					if node.expr.expr is ast.CallExpr {
						if node.expr.expr.args.len == 1 {
							target_type := g.expr_type_to_c(node.expr.expr.lhs)
							g.sb.write_string('((${target_type}*)(')
							g.expr(node.expr.expr.args[0])
							g.sb.write_string('))')
							return
						}
					}
				}
			}
			// Handle &fn_call() where fn_call returns a struct (rvalue)
			// Can't take address of rvalue, use compound statement expression
			if node.op == .amp && node.expr is ast.CallExpr {
				if !(node.expr.args.len == 1 && node.expr.lhs is ast.Ident
					&& g.is_type_name(node.expr.lhs.name)) {
					// This is a function call, not a type cast
					ret_type := g.get_expr_type(node.expr)
					if ret_type != '' && ret_type != 'void' && ret_type != 'int' {
						tmp_name := '_sumtmp${g.tmp_counter}'
						g.tmp_counter++
						g.sb.write_string('({ ${ret_type} ${tmp_name} = ')
						g.expr(node.expr)
						g.sb.write_string('; &${tmp_name}; })')
						return
					}
				}
			}
			// V `&Type{...}` must allocate on the heap.
			// Taking the address of a C compound literal here would create a dangling pointer.
			if node.op == .amp && node.expr is ast.InitExpr {
				type_name := g.expr_type_to_c(node.expr.typ)
				tmp_name := '_heap_t${g.tmp_counter}'
				g.tmp_counter++
				g.sb.write_string('({ ${type_name}* ${tmp_name} = (${type_name}*)malloc(sizeof(${type_name})); *${tmp_name} = ')
				g.expr(node.expr)
				g.sb.write_string('; ${tmp_name}; })')
				return
			}
			if node.op == .mul {
				if raw_type := g.get_raw_type(node.expr) {
					is_iface := raw_type is types.Interface
						|| (raw_type is types.Pointer && raw_type.base_type is types.Interface)
					if is_iface {
						target_type := g.get_expr_type(node)
						if target_type != '' && target_type != 'int' {
							sep := if g.expr_is_pointer(node.expr) { '->' } else { '.' }
							g.sb.write_string('(*((')
							g.sb.write_string(target_type)
							g.sb.write_string('*)(')
							g.expr(node.expr)
							g.sb.write_string('${sep}_object)))')
							return
						}
					}
				}
			}
			op := match node.op {
				.minus { '-' }
				.not { '!' }
				.amp { '&' }
				.mul { '*' }
				.bit_not { '~' }
				else { '' }
			}
			g.sb.write_string(op)
			g.expr(node.expr)
		}
		ast.CallExpr {
			g.call_expr(node.lhs, node.args)
		}
		ast.CallOrCastExpr {
			panic('bug in v2 compiler: CallOrCastExpr should have been lowered in v2.transformer')
		}
		ast.SelectorExpr {
			// typeof(x).name -> just emit the typeof string directly (already a string)
			if node.lhs is ast.KeywordOperator && node.lhs.op == .key_typeof
				&& node.rhs.name == 'name' {
				g.gen_keyword_operator(node.lhs)
				return
			}
			// C.<ident> references C macros/constants directly (e.g. C.EOF -> EOF).
			if node.lhs is ast.Ident && node.lhs.name == 'C' {
				g.sb.write_string(node.rhs.name)
				return
			}
			if node.lhs is ast.Ident
				&& node.lhs.name in ['bool', 'string', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'byte', 'rune'] {
				if enum_name := g.enum_value_to_enum[node.rhs.name] {
					g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
					return
				}
			}
			if node.lhs is ast.Ident {
				mut is_known_var := g.get_local_var_c_type(node.lhs.name) != none
				if !is_known_var && !g.is_module_ident(node.lhs.name) {
					if enum_name := g.get_expr_type_from_env(node) {
						if enum_name != '' && g.is_enum_type(enum_name) {
							g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
							return
						}
					}
					if node.lhs.name in ['bool', 'string', 'int', 'i8', 'i16', 'i32', 'i64', 'u8',
						'u16', 'u32', 'u64', 'f32', 'f64', 'byte', 'rune'] {
						if enum_name := g.enum_value_to_enum[node.rhs.name] {
							g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
							return
						}
					}
				}
			}
			// If checker already resolved this selector as an enum value, use Enum__field.
			if raw_type := g.get_raw_type(node) {
				if raw_type is types.Enum {
					// Verify the field actually belongs to this enum.
					// The checker may annotate branch values with the match expression's
					// enum type instead of the return type's enum.
					mut field_valid := false
					for f in raw_type.fields {
						if f.name == node.rhs.name {
							field_valid = true
							break
						}
					}
					if field_valid {
						mut emit_enum_value := false
						if node.lhs is ast.EmptyExpr {
							emit_enum_value = true
						} else if node.lhs is ast.Ident {
							if g.is_enum_type(node.lhs.name) {
								emit_enum_value = true
							} else if !g.is_module_ident(node.lhs.name) {
								emit_enum_value = g.get_local_var_c_type(node.lhs.name) == none
							}
						}
						if emit_enum_value {
							enum_name := g.types_type_to_c(raw_type)
							g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
							return
						}
					}
				}
			}
			if g.gen_sum_narrowed_selector(node) {
				return
			}
			if g.gen_sum_variant_field_selector(node) {
				return
			}
			lhs_type := g.get_expr_type(node.lhs)
			if node.rhs.name == 'data' {
				if lhs_type.starts_with('_result_') && g.result_value_type(lhs_type) != '' {
					g.gen_unwrapped_value_expr(node.lhs)
					return
				}
				if lhs_type.starts_with('_option_') && option_value_type(lhs_type) != '' {
					g.gen_unwrapped_value_expr(node.lhs)
					return
				}
			}
			// Fixed-size array `.len` becomes compile-time length.
			if node.rhs.name == 'len' {
				if node.lhs is ast.Ident {
					mut fixed_name := node.lhs.name
					module_const_key := 'const_${g.cur_module}__${node.lhs.name}'
					if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
						&& module_const_key in g.emitted_types {
						fixed_name = '${g.cur_module}__${node.lhs.name}'
					}
					if fixed_name in g.fixed_array_globals {
						g.sb.write_string('((int)(sizeof(${fixed_name}) / sizeof(${fixed_name}[0])))')
						return
					}
					if fixed_name in ['strconv__pow5_inv_split_32', 'strconv__pow5_split_32',
						'strconv__pow5_inv_split_64_x', 'strconv__pow5_split_64_x'] {
						g.sb.write_string('((int)(sizeof(${fixed_name}) / sizeof(${fixed_name}[0])))')
						return
					}
				}
				if node.lhs is ast.SelectorExpr {
					if node.lhs.lhs is ast.Ident && g.is_module_ident(node.lhs.lhs.name) {
						mod_name := g.resolve_module_name(node.lhs.lhs.name)
						fixed_name := '${mod_name}__${node.lhs.rhs.name}'
						if fixed_name in g.fixed_array_globals {
							g.sb.write_string('((int)(sizeof(${fixed_name}) / sizeof(${fixed_name}[0])))')
							return
						}
					}
				}
				if node.lhs is ast.SelectorExpr && g.is_fixed_array_selector(node.lhs) {
					g.sb.write_string('((int)(sizeof(')
					g.expr(node.lhs)
					g.sb.write_string(') / sizeof((')
					g.expr(node.lhs)
					g.sb.write_string(')[0])))')
					return
				}
				if raw_type := g.get_raw_type(node.lhs) {
					if raw_type is types.ArrayFixed {
						g.sb.write_string('((int)(sizeof(')
						g.expr(node.lhs)
						g.sb.write_string(') / sizeof((')
						g.expr(node.lhs)
						g.sb.write_string(')[0])))')
						return
					}
				}
			}
			// Enum shorthand: `.field` -> `EnumName__field` (using type checker info).
			if node.lhs is ast.EmptyExpr {
				// The enum_value_to_enum map definitively tells us which enum owns a field.
				// Use it to validate/override type checker results that may be wrong
				// (e.g., match branch values annotated with the match expression's enum type
				// instead of the return type's enum).
				known_enum := g.enum_value_to_enum[node.rhs.name] or { '' }
				if raw_type := g.get_raw_type(node) {
					if raw_type is types.Enum {
						enum_name := g.types_type_to_c(raw_type)
						if g.enum_has_field(enum_name, node.rhs.name) {
							g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
							return
						}
					}
				}
				if enum_name := g.get_expr_type_from_env(node) {
					if enum_name != '' && enum_name != 'int' && g.is_enum_type(enum_name) {
						if g.enum_has_field(enum_name, node.rhs.name) {
							g.sb.write_string('${g.normalize_enum_name(enum_name)}__${node.rhs.name}')
							return
						}
					}
				}
				// Use the definitive enum field mapping
				if known_enum != '' {
					g.sb.write_string('${g.normalize_enum_name(known_enum)}__${node.rhs.name}')
					return
				}
				// Last resort: use function return type as context
				if g.cur_fn_ret_type != '' && g.is_enum_type(g.cur_fn_ret_type) {
					g.sb.write_string('${g.normalize_enum_name(g.cur_fn_ret_type)}__${node.rhs.name}')
					return
				}
			}
			// module.const / module.var => module__const / module__var
			if node.lhs is ast.Ident && g.is_module_ident(node.lhs.name) {
				mod_name := g.resolve_module_name(node.lhs.name)
				if node.rhs.name.starts_with('${mod_name}__') {
					g.sb.write_string(node.rhs.name)
				} else {
					g.sb.write_string('${mod_name}__${node.rhs.name}')
				}
				return
			}
			// Check if LHS is an enum type name -> emit EnumName__field
			if node.lhs is ast.Ident && g.is_enum_type(node.lhs.name) {
				enum_name := g.get_qualified_name(node.lhs.name)
				g.sb.write_string('${enum_name}__${node.rhs.name}')
			} else {
				mut use_ptr := g.expr_is_pointer(node.lhs)
				if node.lhs is ast.Ident {
					if node.lhs.name in g.cur_fn_mut_params {
						use_ptr = true
					} else if local_type := g.get_local_var_c_type(node.lhs.name) {
						// Local declaration type is authoritative for value vs pointer access.
						use_ptr = local_type.ends_with('*')
					}
				}
				lhs_struct := g.selector_struct_name(node.lhs)
				owner := g.embedded_owner_for(lhs_struct, node.rhs.name)
				field_name := escape_c_keyword(node.rhs.name)
				selector := if use_ptr { '->' } else { '.' }
				g.expr(node.lhs)
				if owner != '' {
					g.sb.write_string('${selector}${escape_c_keyword(owner)}.${field_name}')
				} else {
					g.sb.write_string('${selector}${field_name}')
				}
			}
		}
		ast.IfExpr {
			g.gen_if_expr_value(node)
		}
		ast.PostfixExpr {
			g.expr(node.expr)
			op := match node.op {
				.inc { '++' }
				.dec { '--' }
				else { '' }
			}
			g.sb.write_string(op)
		}
		ast.ModifierExpr {
			g.expr(node.expr)
		}
		ast.CastExpr {
			g.gen_cast_expr(node)
		}
		ast.IndexExpr {
			g.gen_index_expr(node)
		}
		ast.ArrayInitExpr {
			g.gen_array_init_expr(node)
		}
		ast.InitExpr {
			g.gen_init_expr(node)
		}
		ast.MapInitExpr {
			panic('bug in v2 compiler: MapInitExpr should have been lowered in v2.transformer')
		}
		ast.MatchExpr {
			panic('bug in v2 compiler: MatchExpr should have been lowered in v2.transformer')
		}
		ast.UnsafeExpr {
			g.gen_unsafe_expr(node)
		}
		ast.OrExpr {
			panic('bug in v2 compiler: OrExpr should have been expanded in v2.transformer')
		}
		ast.AsCastExpr {
			g.gen_as_cast_expr(node)
		}
		ast.StringInterLiteral {
			g.gen_string_inter_literal(node)
		}
		ast.FnLiteral {
			g.gen_fn_literal(node)
		}
		ast.LambdaExpr {
			g.sb.write_string('/* [TODO] LambdaExpr */ NULL')
		}
		ast.ComptimeExpr {
			// $if comptime should be resolved by transformer; @FN etc. handled here
			if node.expr is ast.IfExpr {
				panic('bug in v2 compiler: comptime \$if should have been resolved in v2.transformer')
			}
			g.gen_comptime_expr(node)
		}
		ast.Keyword {
			g.gen_keyword(node)
		}
		ast.KeywordOperator {
			g.gen_keyword_operator(node)
		}
		ast.RangeExpr {
			panic('bug in v2 compiler: RangeExpr should have been lowered in v2.transformer')
		}
		ast.SelectExpr {
			g.sb.write_string('/* [TODO] SelectExpr */ 0')
		}
		ast.LockExpr {
			panic('bug in v2 compiler: LockExpr should have been lowered in v2.transformer')
		}
		ast.Type {
			g.sb.write_string('/* [TODO] Type */ 0')
		}
		ast.AssocExpr {
			panic('bug in v2 compiler: AssocExpr should have been lowered in v2.transformer')
		}
		ast.Tuple {
			tuple_type := g.get_expr_type(node)
			g.sb.write_string('((${tuple_type}){')
			for i, expr in node.exprs {
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.sb.write_string('.arg${i} = ')
				g.expr(expr)
			}
			g.sb.write_string('})')
		}
		ast.FieldInit {
			panic('bug in v2 compiler: FieldInit in expression position should have been lowered in v2.transformer')
		}
		ast.IfGuardExpr {
			panic('bug in v2 compiler: IfGuardExpr should have been expanded in v2.transformer')
		}
		ast.GenericArgs {
			panic('bug in v2 compiler: GenericArgs should have been resolved during type checking')
		}
		ast.GenericArgOrIndexExpr {
			panic('bug in v2 compiler: GenericArgOrIndexExpr should have been resolved during type checking')
		}
		ast.SqlExpr {
			g.sb.write_string('/* [TODO] SqlExpr */ 0')
		}
		ast.EmptyExpr {}
	}
}

fn extract_compare_cast_type(expr ast.Expr) string {
	if expr is ast.PrefixExpr && expr.op == .mul {
		if expr.expr is ast.CastExpr {
			if expr.expr.typ is ast.PrefixExpr && expr.expr.typ.op == .amp
				&& expr.expr.typ.expr is ast.Ident {
				return expr.expr.typ.expr.name
			}
		}
	}
	return ''
}

fn (mut g Gen) gen_stmts_from_expr(e ast.Expr) {
	if e is ast.IfExpr {
		g.gen_stmts(e.stmts)
	}
}

fn (mut g Gen) gen_type_cast_expr(type_name string, expr ast.Expr) {
	expr_type := g.get_expr_type(expr)
	if expr_type.starts_with('_result_') && g.result_value_type(expr_type) != '' {
		g.sb.write_string('((${type_name})(')
		g.gen_unwrapped_value_expr(expr)
		g.sb.write_string('))')
		return
	}
	if expr_type.starts_with('_option_') && option_value_type(expr_type) != '' {
		g.sb.write_string('((${type_name})(')
		g.gen_unwrapped_value_expr(expr)
		g.sb.write_string('))')
		return
	}
	if variants := g.sum_type_variants[type_name] {
		mut inner_type := expr_type
		if inner_type == '' || inner_type == 'int' {
			match expr {
				ast.CallExpr {
					if ret := g.get_call_return_type(expr.lhs, expr.args.len) {
						if ret != '' {
							inner_type = ret
						}
					}
				}
				ast.InitExpr {
					inner_type = g.expr_type_to_c(expr.typ)
				}
				ast.SelectorExpr {
					field_type := g.selector_field_type(expr)
					if field_type != '' {
						inner_type = field_type
					}
				}
				else {}
			}
		}
		// Identity cast: inner type is already the target sum type, no wrapping needed
		if inner_type == type_name {
			g.expr(expr)
			return
		}
		if inner_type != '' {
			mut tag := -1
			mut field_name := ''
			for i, v in variants {
				if v == inner_type || inner_type.ends_with('__${v}')
					|| v.ends_with('__${inner_type}') {
					tag = i
					field_name = v
					break
				}
			}
			// If direct matching failed, try qualifying inner_type with the sum type's module prefix
			// (e.g. 'Array_Attribute' → 'Array_ast__Attribute' when sum type is 'ast__Stmt')
			if tag < 0 && type_name.contains('__') {
				mod_prefix := type_name.all_before_last('__') + '__'
				// Qualify the type: Array_X → Array_mod__X, or just X → mod__X
				qualified := if inner_type.starts_with('Array_') && !inner_type[6..].contains('__') {
					'Array_${mod_prefix}${inner_type[6..]}'
				} else if inner_type.starts_with('Map_') && !inner_type[4..].contains('__') {
					'Map_${mod_prefix}${inner_type[4..]}'
				} else if !inner_type.contains('__') {
					'${mod_prefix}${inner_type}'
				} else {
					''
				}
				if qualified != '' {
					for i, v in variants {
						if v == qualified {
							tag = i
							field_name = v
							break
						}
					}
				}
			}
			// If direct matching failed, check if inner_type is a known sum type
			// that appears as a variant of the target sum type (e.g. ast__Type -> ast__Expr._Type)
			if tag < 0 && inner_type in g.sum_type_variants {
				inner_short := if inner_type.contains('__') {
					inner_type.all_after_last('__')
				} else {
					inner_type
				}
				for i, v in variants {
					if v == inner_short {
						tag = i
						field_name = v
						break
					}
				}
			}
			if tag >= 0 {
				is_primitive :=
					inner_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool', 'rune', 'byte', 'usize', 'isize']
					|| inner_type in g.primitive_type_aliases
				g.gen_sum_type_wrap(type_name, field_name, tag, is_primitive, expr, inner_type)
				return
			}
		}
		// Fallback: try to infer variant from expression structure
		inferred := g.infer_sum_variant_from_expr(type_name, variants, expr)
		if inferred.tag >= 0 {
			g.gen_sum_type_wrap(type_name, inferred.field_name, inferred.tag, inferred.is_primitive,
				expr, inferred.inner_type)
			return
		}
	}
	if g.gen_interface_cast(type_name, expr) {
		return
	}
	// For non-sum-types, use C cast
	g.sb.write_string('((${type_name})(')
	g.expr(expr)
	g.sb.write_string('))')
}

struct SumVariantMatch {
	tag          int
	field_name   string
	is_primitive bool
	inner_type   string
}

fn (mut g Gen) unwrap_addr_of_value_expr(expr ast.Expr) ?ast.Expr {
	match expr {
		ast.PrefixExpr {
			if expr.op == .amp {
				return expr.expr
			}
		}
		ast.CastExpr {
			cast_type := g.expr_type_to_c(expr.typ)
			if cast_type in ['void*', 'voidptr'] {
				if expr.expr is ast.PrefixExpr && expr.expr.op == .amp {
					return expr.expr.expr
				}
			}
		}
		ast.ParenExpr {
			return g.unwrap_addr_of_value_expr(expr.expr)
		}
		else {}
	}
	return none
}

fn (mut g Gen) is_type_reference_expr(node ast.Expr) bool {
	match node {
		ast.Ident {
			if g.get_local_var_c_type(node.name) != none {
				return false
			}
			return g.is_type_name(node.name)
		}
		ast.SelectorExpr {
			if node.lhs is ast.Ident {
				if g.is_module_ident(node.lhs.name) {
					return true
				}
				return g.is_type_name(node.lhs.name)
			}
		}
		else {}
	}
	return false
}

fn (g &Gen) contains_as_cast_expr(node ast.Expr) bool {
	match node {
		ast.AsCastExpr {
			return true
		}
		ast.CastExpr {
			return g.contains_as_cast_expr(node.expr)
		}
		ast.ParenExpr {
			return g.contains_as_cast_expr(node.expr)
		}
		ast.ModifierExpr {
			return g.contains_as_cast_expr(node.expr)
		}
		ast.PrefixExpr {
			return g.contains_as_cast_expr(node.expr)
		}
		else {}
	}
	return false
}

fn (mut g Gen) expr_cast_target_type(node ast.Expr) string {
	match node {
		ast.CastExpr {
			return g.expr_type_to_c(node.typ)
		}
		ast.ParenExpr {
			return g.expr_cast_target_type(node.expr)
		}
		ast.ModifierExpr {
			return g.expr_cast_target_type(node.expr)
		}
		ast.PrefixExpr {
			return g.expr_cast_target_type(node.expr)
		}
		else {}
	}
	return ''
}

fn (mut g Gen) gen_sum_narrowed_selector(node ast.SelectorExpr) bool {
	// Internal sum fields (`_tag`, `_data`, `_Variant`) must never go through
	// smartcast field lowering; they are already the raw representation.
	if node.rhs.name.starts_with('_') {
		return false
	}
	if node.lhs !is ast.Ident {
		return false
	}
	lhs_ident := node.lhs as ast.Ident
	decl_type := g.get_local_var_c_type(lhs_ident.name) or { return false }
	variants := g.sum_type_variants[decl_type] or { return false }
	narrowed := g.get_expr_type_from_env(node.lhs) or { return false }
	if narrowed == '' || narrowed == decl_type {
		return false
	}
	narrowed_short := if narrowed.contains('__') { narrowed.all_after_last('__') } else { narrowed }
	mut variant_field := ''
	for v in variants {
		v_short := if v.contains('__') { v.all_after_last('__') } else { v }
		if v == narrowed || v_short == narrowed_short || narrowed.ends_with('__${v_short}') {
			variant_field = v_short
			break
		}
	}
	if variant_field == '' {
		return false
	}
	field_name := escape_c_keyword(node.rhs.name)
	owner := g.embedded_owner_for(narrowed, node.rhs.name)
	g.sb.write_string('(((${narrowed}*)(((')
	g.expr(node.lhs)
	g.sb.write_string(')._data._${variant_field})))')
	if owner != '' {
		g.sb.write_string('->${escape_c_keyword(owner)}.${field_name}')
	} else {
		g.sb.write_string('->${field_name}')
	}
	g.sb.write_string(')')
	return true
}

fn (mut g Gen) gen_unsafe_expr(node ast.UnsafeExpr) {
	if node.stmts.len == 0 {
		g.sb.write_string('0')
		return
	}
	if node.stmts.len == 1 {
		stmt := node.stmts[0]
		if stmt is ast.ExprStmt {
			g.expr(stmt.expr)
		} else {
			// Single non-expression statement (e.g., return) - emit directly
			g.gen_stmt(stmt)
		}
		return
	}
	// Multi-statement: use GCC compound expression ({ ... })
	g.sb.write_string('({ ')
	for i, stmt in node.stmts {
		if i < node.stmts.len - 1 {
			g.gen_stmt(stmt)
		}
	}
	// Last statement - if it's an ExprStmt, its value is the block's value
	last := node.stmts[node.stmts.len - 1]
	if last is ast.ExprStmt {
		g.expr(last.expr)
		g.sb.write_string('; ')
	} else {
		g.gen_stmt(last)
		g.sb.write_string('0; ')
	}
	g.sb.write_string('})')
}

fn (mut g Gen) gen_index_expr(node ast.IndexExpr) {
	// Slice syntax: arr[a..b], arr[..b], arr[a..], s[a..b]
	if node.expr is ast.RangeExpr {
		panic('bug in v2 compiler: slice IndexExpr should have been lowered in v2.transformer')
	}
	if node.lhs is ast.Ident {
		if node.lhs.name in g.fixed_array_globals || node.lhs.name == 'rune_maps' {
			g.expr(node.lhs)
			g.sb.write_string('[')
			g.expr(node.expr)
			g.sb.write_string(']')
			return
		}
	}
	// Fixed-size array struct fields are emitted as plain C arrays.
	if node.lhs is ast.SelectorExpr && g.is_fixed_array_selector(node.lhs) {
		g.expr(node.lhs)
		g.sb.write_string('[')
		g.expr(node.expr)
		g.sb.write_string(']')
		return
	}
	// Check LHS type from environment to determine indexing strategy
	if raw_type := g.get_raw_type(node.lhs) {
		if raw_type is types.ArrayFixed {
			// Fixed arrays are C arrays - direct indexing
			g.expr(node.lhs)
			g.sb.write_string('[')
			g.expr(node.expr)
			g.sb.write_string(']')
			return
		}
		if raw_type is types.Array {
			// Dynamic arrays: ((elem_type*)arr.data)[idx]
			elem_type := g.types_type_to_c(raw_type.elem_type)
			g.sb.write_string('((${elem_type}*)')
			g.expr(node.lhs)
			g.sb.write_string('.data)[')
			g.expr(node.expr)
			g.sb.write_string(']')
			return
		}
		if raw_type is types.Alias {
			match raw_type.base_type {
				types.Array {
					elem_type := g.types_type_to_c(raw_type.base_type.elem_type)
					g.sb.write_string('((${elem_type}*)')
					g.expr(node.lhs)
					g.sb.write_string('.data)[')
					g.expr(node.expr)
					g.sb.write_string(']')
					return
				}
				types.ArrayFixed {
					g.expr(node.lhs)
					g.sb.write_string('[')
					g.expr(node.expr)
					g.sb.write_string(']')
					return
				}
				types.String {
					g.expr(node.lhs)
					g.sb.write_string('.str[')
					g.expr(node.expr)
					g.sb.write_string(']')
					return
				}
				else {}
			}
		}
		if raw_type is types.Map {
			g.panic_map_index_expr(node)
		}
		if raw_type is types.String {
			if node.lhs is ast.SelectorExpr && g.is_fixed_array_selector(node.lhs) {
				g.expr(node.lhs)
				g.sb.write_string('[')
				g.expr(node.expr)
				g.sb.write_string(']')
				return
			}
			// Distinguish true string indexing (u8 result) from array-like indexing.
			if out_type := g.get_raw_type(node) {
				out_name := g.types_type_to_c(out_type)
				if out_name !in ['u8', 'byte', 'char'] {
					g.expr(node.lhs)
					g.sb.write_string('[')
					g.expr(node.expr)
					g.sb.write_string(']')
					return
				}
			}
			g.expr(node.lhs)
			g.sb.write_string('.str[')
			g.expr(node.expr)
			g.sb.write_string(']')
			return
		}
		if raw_type is types.Pointer {
			// Pointer to array: use -> accessor
			if raw_type.base_type is types.Array {
				elem_type := g.types_type_to_c(raw_type.base_type.elem_type)
				g.sb.write_string('((${elem_type}*)')
				g.expr(node.lhs)
				g.sb.write_string('->data)[')
				g.expr(node.expr)
				g.sb.write_string(']')
				return
			} else if raw_type.base_type is types.ArrayFixed {
				// Pointer to fixed array: dereference then index
				g.sb.write_string('(*')
				g.expr(node.lhs)
				g.sb.write_string(')[')
				g.expr(node.expr)
				g.sb.write_string(']')
				return
			} else if raw_type.base_type is types.Map {
				g.panic_map_index_expr(node)
			} else if raw_type.base_type is types.Pointer || raw_type.base_type is types.String {
				// Pointer to pointer (e.g. &&char) or pointer to string (e.g. &string used as array):
				// plain C pointer arithmetic
				g.expr(node.lhs)
				g.sb.write_string('[')
				g.expr(node.expr)
				g.sb.write_string(']')
				return
			}
		}
	}
	if lhs_raw_type := g.get_raw_type(node.lhs) {
		if lhs_raw_type is types.ArrayFixed {
			// Fixed arrays are C arrays: direct indexing
			g.expr(node.lhs)
			g.sb.write_string('[')
			g.expr(node.expr)
			g.sb.write_string(']')
			return
		}
	}
	lhs_type := g.get_expr_type(node.lhs)
	if lhs_type == 'map' || lhs_type.starts_with('Map_') {
		g.panic_map_index_expr(node)
	}
	if lhs_type == 'string' {
		g.expr(node.lhs)
		g.sb.write_string('.str[')
		g.expr(node.expr)
		g.sb.write_string(']')
		return
	}
	if lhs_type == 'string*' {
		elem_type := g.get_expr_type(node)
		if elem_type in ['u8', 'byte', 'char'] {
			g.expr(node.lhs)
			g.sb.write_string('->str[')
			g.expr(node.expr)
			g.sb.write_string(']')
		} else {
			g.expr(node.lhs)
			g.sb.write_string('[')
			g.expr(node.expr)
			g.sb.write_string(']')
		}
		return
	}
	if lhs_type.trim_right('*') in ['strings__Builder', 'Builder'] {
		g.sb.write_string('((u8*)')
		g.expr(node.lhs)
		if lhs_type.ends_with('*') {
			g.sb.write_string('->data)[')
		} else {
			g.sb.write_string('.data)[')
		}
		g.expr(node.expr)
		g.sb.write_string(']')
		return
	}
	// Fixed arrays (including pointer-to-fixed for mut params): direct C array indexing
	if lhs_type.trim_right('*').starts_with('Array_fixed_') {
		if lhs_type.ends_with('*') {
			g.sb.write_string('(*')
			g.expr(node.lhs)
			g.sb.write_string(')[')
		} else {
			g.expr(node.lhs)
			g.sb.write_string('[')
		}
		g.expr(node.expr)
		g.sb.write_string(']')
		return
	}
	if lhs_type == 'array' || lhs_type.starts_with('Array_') {
		mut elem_type := g.get_expr_type(node)
		if elem_type == '' || elem_type == 'int' {
			if lhs_type.starts_with('Array_fixed_') {
				if finfo := g.collected_fixed_array_types[lhs_type] {
					elem_type = finfo.elem_type
				}
			} else if lhs_type.starts_with('Array_') {
				elem_type = lhs_type['Array_'.len..].trim_right('*')
			} else if lhs_type == 'array' {
				// Unparameterized array - try to infer element type from the call
				if node.lhs is ast.CallExpr {
					call_name := g.resolve_call_name(node.lhs.lhs, node.lhs.args.len)
					if call_name.contains('array_from_c_array') && node.lhs.args.len >= 3 {
						// Extract elem type from sizeof(T) in 3rd arg
						sizeof_arg := node.lhs.args[2]
						if sizeof_arg is ast.KeywordOperator && sizeof_arg.op == .key_sizeof {
							if sizeof_arg.exprs.len > 0 {
								elem_type = g.expr_type_to_c(sizeof_arg.exprs[0])
							}
						}
					} else if node.lhs.args.len > 0 {
						src_type := g.get_expr_type(node.lhs.args[0])
						if src_type.starts_with('Array_') {
							elem_type = src_type['Array_'.len..].trim_right('*')
						}
					}
				}
			}
		}
		if elem_type == '' {
			elem_type = 'u8'
		}
		g.sb.write_string('((${elem_type}*)')
		g.expr(node.lhs)
		if lhs_type.ends_with('*') {
			g.sb.write_string('->data)[')
		} else {
			g.sb.write_string('.data)[')
		}
		g.expr(node.expr)
		g.sb.write_string(']')
		return
	}
	// void*/voidptr/byteptr: cast to u8* for indexing (V treats malloc result as &u8)
	if lhs_type in ['void*', 'voidptr', 'byteptr'] {
		g.sb.write_string('((u8*)')
		g.expr(node.lhs)
		g.sb.write_string(')[')
		g.expr(node.expr)
		g.sb.write_string(']')
		return
	}
	// Fallback: direct C array indexing
	g.expr(node.lhs)
	g.sb.write_string('[')
	g.expr(node.expr)
	g.sb.write_string(']')
}

fn (mut g Gen) panic_map_index_expr(node ast.IndexExpr) {
	lhs_type := g.get_expr_type(node.lhs)
	idx_src := '${node.lhs.name()}[${node.expr.name()}]'
	panic('bug in v2 compiler: map IndexExpr should have been lowered in v2.transformer (file=${g.cur_file_name} fn=${g.cur_fn_name} pos=${node.pos} idx=${idx_src} lhs=${node.lhs.name()} lhs_type=${lhs_type})')
}

fn (mut g Gen) gen_comptime_expr(node ast.ComptimeExpr) {
	if node.expr is ast.Ident {
		name := node.expr.name
		match name {
			'FN', 'METHOD', 'FUNCTION' {
				fn_name := g.cur_fn_name
				g.sb.write_string(c_static_v_string_expr(fn_name))
			}
			'MOD' {
				mod_name := g.cur_module
				g.sb.write_string(c_static_v_string_expr(mod_name))
			}
			'FILE' {
				g.sb.write_string(c_v_string_expr_from_ptr_len('__FILE__', 'sizeof(__FILE__)-1',
					true))
			}
			'LINE' {
				g.sb.write_string('__LINE__')
			}
			'VCURRENTHASH' {
				g.sb.write_string(c_static_v_string_expr('VCURRENTHASH'))
			}
			'VEXE' {
				g.sb.write_string('__vexe_path()')
			}
			else {
				g.sb.write_string('${c_empty_v_string_expr()} /* unknown comptime: ${name} */')
			}
		}
		return
	}
	// Fallback: emit the inner expression
	g.expr(node.expr)
}

fn (mut g Gen) expr_to_string(expr ast.Expr) string {
	saved_sb := g.sb
	mut tmp_sb := strings.new_builder(64)
	g.sb = tmp_sb
	g.expr(expr)
	value := g.sb.str()
	g.sb = saved_sb
	return value
}

fn is_header_type_only_const_expr(expr ast.Expr) bool {
	return match expr {
		ast.Type, ast.SelectorExpr {
			true
		}
		ast.Ident {
			name := expr.name
				name in ['bool', 'byte', 'char', 'f32', 'f64', 'i8', 'i16', 'i32', 'int', 'i64', 'isize', 'rune', 'string', 'u8', 'u16', 'u32', 'u64', 'usize', 'void', 'voidptr', 'byteptr', 'charptr']
				|| name.starts_with('&') || name.starts_with('[]') || name.starts_with('?')
				|| name.starts_with('!') || name.contains('[') || name.contains('__')
		}
		else {
			false
		}
	}
}

fn (mut g Gen) gen_cast_expr(node ast.CastExpr) {
	type_name := g.expr_type_to_c(node.typ)
	if type_name.starts_with('_option_') {
		value_type := option_value_type(type_name)
		if value_type != '' && value_type != 'void' {
			g.sb.write_string('({ ${type_name} _opt = (${type_name}){ .state = 2 }; ${value_type} _val = ')
			g.expr(node.expr)
			g.sb.write_string('; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt; })')
			return
		}
	}
	if type_name.starts_with('_result_') {
		value_type := g.result_value_type(type_name)
		if value_type != '' && value_type != 'void' {
			g.sb.write_string('({ ${type_name} _res = (${type_name}){0}; ${value_type} _val = ')
			g.expr(node.expr)
			g.sb.write_string('; _result_ok(&_val, (_result*)&_res, sizeof(_val)); _res; })')
			return
		}
	}
	if type_name == 'void' {
		// Preserve side effects when discarding expression results.
		// e.g. `(void)(f())` must still call `f()`.
		mut inner := strings.new_builder(64)
		saved := g.sb
		g.sb = inner
		g.expr(node.expr)
		inner_str := g.sb.str()
		g.sb = saved
		if inner_str == '' {
			g.sb.write_string('((void)0)')
		} else {
			g.sb.write_string('((void)(${inner_str}))')
		}
		return
	}
	expr_type := g.get_expr_type(node.expr)
	if expr_type.starts_with('_result_') && g.result_value_type(expr_type) != '' {
		g.sb.write_string('((${type_name})(')
		g.gen_unwrapped_value_expr(node.expr)
		g.sb.write_string('))')
		return
	}
	if expr_type.starts_with('_option_') && option_value_type(expr_type) != '' {
		g.sb.write_string('((${type_name})(')
		g.gen_unwrapped_value_expr(node.expr)
		g.sb.write_string('))')
		return
	}
	// Handle sum type and interface casts
	if type_name in g.sum_type_variants {
		g.gen_type_cast_expr(type_name, node.expr)
		return
	}
	if g.gen_interface_cast(type_name, node.expr) {
		return
	}
	// Capture inner expression to check if it's empty
	mut inner := strings.new_builder(64)
	saved := g.sb
	g.sb = inner
	g.expr(node.expr)
	inner_str := g.sb.str()
	g.sb = saved
	if inner_str == '' {
		// Empty inner expression (e.g. unresolved C function call) - emit no-op
		g.sb.write_string('((void)0)')
	} else {
		g.sb.write_string('((${type_name})(${inner_str}))')
	}
}

fn (mut g Gen) gen_as_cast_expr(node ast.AsCastExpr) {
	type_name := g.expr_type_to_c(node.typ)
	short_name := if type_name.contains('__') {
		type_name.all_after_last('__')
	} else {
		type_name
	}
	// Interface cast: `iface as T` => `*((T*)iface._object)`
	if raw_type := g.get_raw_type(node.expr) {
		is_iface := raw_type is types.Interface
			|| (raw_type is types.Pointer && raw_type.base_type is types.Interface)
		if is_iface {
			sep := if g.expr_is_pointer(node.expr) { '->' } else { '.' }
			g.sb.write_string('(*((${type_name}*)(')
			g.expr(node.expr)
			g.sb.write_string('${sep}_object)))')
			return
		}
	}
	mut inner := strings.new_builder(64)
	saved := g.sb
	g.sb = inner
	g.expr(node.expr)
	inner_str := g.sb.str()
	g.sb = saved
	if g.contains_as_cast_expr(node.expr) {
		g.sb.write_string('((${type_name})(${inner_str}))')
		return
	}
	marker := ')->_data._${short_name}'
	if idx := inner_str.index(marker) {
		simplified := inner_str[..idx + 1]
		g.sb.write_string('(*(${simplified}))')
		return
	}
	marker2 := ')->_data)._${short_name}'
	if idx := inner_str.index(marker2) {
		simplified := inner_str[..idx + 1]
		g.sb.write_string('(*(${simplified}))')
		return
	}
	if inner_str.starts_with('((${type_name}*)') || inner_str.starts_with('(${type_name}*)') {
		g.sb.write_string('(*(${inner_str}))')
		return
	}
	if inner_str.starts_with('(*(') || inner_str.starts_with('*(') {
		g.sb.write_string('((${type_name})(${inner_str}))')
		return
	}
	if g.is_sum_payload_expr(node.expr, short_name) {
		g.sb.write_string('(*((${type_name}*)(${inner_str})))')
		return
	}
	if inner_str.contains('->_data._${short_name}') || inner_str.contains('._data._${short_name}') {
		g.sb.write_string('(*((${type_name}*)(${inner_str})))')
		return
	}
	// Sum type cast: a as Cat => (*((main__Cat*)(((a))._data._Cat)))
	sep := if g.expr_is_pointer(node.expr) { '->' } else { '.' }
	g.sb.write_string('(*((${type_name}*)(((${inner_str})${sep}_data._${short_name}))))')
}

fn (g &Gen) unwrap_parens(expr ast.Expr) ast.Expr {
	if expr is ast.ParenExpr {
		return g.unwrap_parens(expr.expr)
	}
	return expr
}
