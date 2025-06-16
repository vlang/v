// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.token

fn (mut c Checker) for_c_stmt(mut node ast.ForCStmt) {
	c.in_for_count++
	prev_loop_labels := c.loop_labels
	if node.has_init {
		c.stmt(mut node.init)
	}
	c.expr(mut node.cond)
	if node.has_inc {
		if mut node.inc is ast.AssignStmt {
			assign := node.inc

			if assign.op == .decl_assign {
				c.error('for loop post statement cannot be a variable declaration', assign.pos)
			}
		}
		c.stmt(mut node.inc)
	}
	c.check_loop_labels(node.label, node.pos)
	c.stmts(mut node.stmts)
	c.loop_labels = prev_loop_labels
	c.in_for_count--
}

fn (mut c Checker) for_in_stmt(mut node ast.ForInStmt) {
	c.in_for_count++
	prev_loop_labels := c.loop_labels
	mut typ := c.expr(mut node.cond)
	if node.key_var.len > 0 && node.key_var != '_' {
		c.check_valid_snake_case(node.key_var, 'variable name', node.pos)
		if reserved_type_names_chk.matches(node.key_var) {
			c.error('invalid use of reserved type `${node.key_var}` as key name', node.pos)
		}
	}
	if node.val_var.len > 0 && node.val_var != '_' {
		c.check_valid_snake_case(node.val_var, 'variable name', node.pos)
		if reserved_type_names_chk.matches(node.val_var) {
			c.error('invalid use of reserved type `${node.val_var}` as value name', node.pos)
		}
	}
	if _ := c.file.global_scope.find_const('${c.mod}.${node.key_var}') {
		c.error('duplicate of a const name `${c.mod}.${node.key_var}`', node.kv_pos)
	}

	if _ := c.file.global_scope.find_const('${c.mod}.${node.val_var}') {
		c.error('duplicate of a const name `${c.mod}.${node.val_var}`', node.vv_pos)
	}

	if node.is_range {
		typ_idx := typ.idx()
		high_type := c.expr(mut node.high)
		high_type_idx := high_type.idx()
		if typ_idx in ast.integer_type_idxs && high_type_idx !in ast.integer_type_idxs
			&& high_type_idx != ast.void_type_idx {
			c.error('range types do not match', node.cond.pos())
		} else if c.table.final_sym(typ).kind == .multi_return
			&& c.table.final_sym(high_type).kind == .multi_return {
			c.error('multi-returns cannot be used in ranges. A range is from a single value to a single higher value.',
				node.cond.pos().extend(node.high.pos()))
		} else if typ_idx !in ast.integer_type_idxs {
			c.error('range type can only be an integer type', node.cond.pos().extend(node.high.pos()))
		} else if high_type.has_option_or_result() {
			c.error('the `high` value in a `for x in low..high {` loop, cannot be Result or Option',
				node.high.pos())
		} else if node.cond is ast.Ident && node.val_var == node.cond.name {
			if node.is_range {
				c.error('in a `for x in <range>` loop, the key or value iteration variable `${node.val_var}` can not be the same as the low variable',
					node.cond.pos())
			} else {
				c.error('in a `for x in array` loop, the key or value iteration variable `${node.val_var}` can not be the same as the low variable',
					node.cond.pos())
			}
		} else if node.high is ast.Ident && node.val_var == node.high.name {
			c.error('in a `for x in <range>` loop, the key or value iteration variable `${node.val_var}` can not be the same as the high variable',
				node.high.pos())
		}

		if high_type in [ast.int_type, ast.int_literal_type] {
			node.val_type = typ
		} else {
			node.val_type = high_type
		}
		node.high_type = high_type
		node.scope.update_var_type(node.val_var, node.val_type)
	} else {
		if node.cond is ast.Ident && node.cond.name in [node.key_var, node.val_var] {
			c.error('in a `for x in array` loop, the key or value iteration variable `${node.val_var}` can not be the same as the low variable',
				node.cond.pos())
		}
		mut is_comptime := false
		if (node.cond is ast.Ident && node.cond.ct_expr) || node.cond is ast.ComptimeSelector {
			ctyp := c.type_resolver.get_type(node.cond)
			if ctyp != ast.void_type {
				is_comptime = true
				typ = ctyp
			}
		}

		mut sym := c.table.final_sym(typ)
		if sym.kind != .string {
			match mut node.cond {
				ast.PrefixExpr {
					node.val_is_ref = node.cond.op == .amp
				}
				ast.ComptimeSelector {
					comptime_typ := c.type_resolver.get_comptime_selector_type(node.cond,
						ast.void_type)
					if comptime_typ != ast.void_type {
						sym = c.table.final_sym(comptime_typ)
						typ = comptime_typ
					}
				}
				ast.Ident {
					match mut node.cond.info {
						ast.IdentVar {
							node.val_is_ref = !node.cond.is_mut() && node.cond.info.typ.is_ptr()
						}
						else {}
					}
				}
				else {}
			}
		} else if node.val_is_mut {
			c.error('string type is immutable, it cannot be changed', node.pos)
			return
		}
		if sym.kind in [.struct, .interface] {
			// iterators
			next_fn := sym.find_method_with_generic_parent('next') or {
				kind_str := if sym.kind == .struct { 'struct' } else { 'interface' }
				c.error('a ${kind_str} must have a `next()` method to be an iterator',
					node.cond.pos())
				return
			}
			if !next_fn.return_type.has_flag(.option) {
				c.error('iterator method `next()` must return an Option', node.cond.pos())
			}
			return_sym := c.table.sym(next_fn.return_type)
			if return_sym.kind == .multi_return {
				c.error('iterator method `next()` must not return multiple values', node.cond.pos())
			}
			// the receiver
			if next_fn.params.len != 1 {
				c.error('iterator method `next()` must have 0 parameters', node.cond.pos())
			}
			mut val_type := next_fn.return_type.clear_option_and_result()
			if node.val_is_mut {
				val_type = val_type.ref()
			}
			node.cond_type = typ
			node.kind = sym.kind
			node.val_type = val_type
			if node.val_type.has_flag(.generic) {
				if c.table.sym(c.unwrap_generic(node.val_type)).kind == .any {
					c.add_error_detail('type parameters defined by `next()` method should be bounded by method owner type')
					c.error('cannot infer from generic type `${c.table.get_type_name(c.unwrap_generic(node.val_type))}`',
						node.vv_pos)
				}
			}
			node.scope.update_var_type(node.val_var, val_type)

			if is_comptime {
				c.type_resolver.update_ct_type(node.val_var, val_type)
				node.scope.update_ct_var_kind(node.val_var, .value_var)

				defer {
					c.type_resolver.type_map.delete(node.val_var)
				}
			}
		} else if sym.kind == .any {
			node.cond_type = typ
			node.kind = sym.kind

			unwrapped_typ := c.unwrap_generic(typ)
			unwrapped_sym := c.table.sym(unwrapped_typ)

			c.table.used_features.comptime_calls['${int(unwrapped_typ)}.next'] = true

			if node.key_var.len > 0 {
				key_type := match unwrapped_sym.kind {
					.map { unwrapped_sym.map_info().key_type }
					else { ast.int_type }
				}
				node.key_type = key_type
				node.scope.update_var_type(node.key_var, key_type)

				if is_comptime {
					c.type_resolver.update_ct_type(node.key_var, key_type)
					node.scope.update_ct_var_kind(node.key_var, .key_var)

					defer {
						c.type_resolver.type_map.delete(node.key_var)
					}
				}
			}

			mut value_type := c.table.value_type(unwrapped_typ)
			if node.val_is_mut {
				value_type = value_type.ref()
			}
			node.scope.update_var_type(node.val_var, value_type)
			node.val_type = value_type

			if is_comptime {
				c.type_resolver.update_ct_type(node.val_var, value_type)
				node.scope.update_ct_var_kind(node.val_var, .value_var)

				defer {
					c.type_resolver.type_map.delete(node.val_var)
				}
			}
		} else {
			if sym.kind == .map && !(node.key_var.len > 0 && node.val_var.len > 0) {
				c.error(
					'declare a key and a value variable when ranging a map: `for key, val in map {`\n' +
					'use `_` if you do not need the variable', node.pos)
			}
			if !c.is_builtin_mod && c.mod != 'strings' {
				c.table.used_features.used_maps++
			}
			if node.key_var.len > 0 {
				key_type := match sym.kind {
					.map { sym.map_info().key_type }
					else { ast.int_type }
				}
				node.key_type = key_type
				node.scope.update_var_type(node.key_var, key_type)

				if is_comptime {
					c.type_resolver.update_ct_type(node.key_var, key_type)
					node.scope.update_ct_var_kind(node.key_var, .key_var)

					defer {
						c.type_resolver.type_map.delete(node.key_var)
					}
				}
			}
			mut value_type := c.table.value_type(typ)
			if sym.kind == .string {
				value_type = ast.u8_type
			} else if sym.kind == .aggregate&& (sym.info as ast.Aggregate).types.all(c.table.type_kind(it) in [.array, .array_fixed, .string, .map]) {
				value_type = c.table.value_type((sym.info as ast.Aggregate).types[0])
			}
			if value_type == ast.void_type || typ.has_flag(.result) {
				if typ != ast.void_type {
					c.error('for in: cannot index `${c.table.type_to_str(typ)}`', node.cond.pos())
				}
			}
			if node.val_is_mut {
				value_type = value_type.ref()
				match mut node.cond {
					ast.Ident {
						if mut node.cond.obj is ast.Var {
							if !node.cond.obj.is_mut {
								c.error('`${node.cond.obj.name}` is immutable, it cannot be changed',
									node.cond.pos)
							}
						}
					}
					ast.ArrayInit {
						c.error('array literal is immutable, it cannot be changed', node.cond.pos)
					}
					ast.MapInit {
						c.error('map literal is immutable, it cannot be changed', node.cond.pos)
					}
					ast.SelectorExpr {
						if root_ident := node.cond.root_ident() {
							if root_ident.kind != .unresolved {
								if var := node.scope.find_var(root_ident.name) {
									if !var.is_mut {
										sym2 := c.table.sym(root_ident.obj.typ)
										c.error('field `${sym2.name}.${node.cond.field_name}` is immutable, it cannot be changed',
											node.cond.pos)
									}
								}
							}
						}
					}
					else {}
				}
			} else if node.val_is_ref {
				value_type = value_type.ref()
			}
			node.cond_type = typ
			node.kind = sym.kind
			node.val_type = value_type
			node.scope.update_var_type(node.val_var, value_type)
			if is_comptime {
				c.type_resolver.update_ct_type(node.val_var, value_type)
				node.scope.update_ct_var_kind(node.val_var, .value_var)

				defer {
					c.type_resolver.type_map.delete(node.val_var)
				}
			}
		}
	}
	c.check_loop_labels(node.label, node.pos)
	c.stmts(mut node.stmts)
	c.loop_labels = prev_loop_labels
	c.in_for_count--
}

fn (mut c Checker) for_stmt(mut node ast.ForStmt) {
	c.in_for_count++
	prev_loop_labels := c.loop_labels
	c.expected_type = ast.bool_type
	if node.cond !is ast.EmptyExpr {
		typ := c.expr(mut node.cond)
		if !node.is_inf && typ.idx() != ast.bool_type_idx && !c.pref.translated
			&& !c.file.is_translated {
			c.error('non-bool used as for condition', node.pos)
		}
	}
	if mut node.cond is ast.InfixExpr && node.cond.op == .key_is {
		if node.cond.right is ast.TypeNode && node.cond.left in [ast.Ident, ast.SelectorExpr] {
			if c.table.type_kind(node.cond.left_type) in [.sum_type, .interface] {
				c.smartcast(mut node.cond.left, node.cond.left_type, node.cond.right_type, mut
					node.scope, false, false)
			}
		}
	}
	// TODO: update loop var type
	// how does this work currently?
	c.check_loop_labels(node.label, node.pos)
	c.stmts(mut node.stmts)
	c.loop_labels = prev_loop_labels
	c.in_for_count--
	if c.smartcast_mut_pos != token.Pos{} {
		c.smartcast_mut_pos = token.Pos{}
	}
}
