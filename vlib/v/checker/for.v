// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.table
import v.pref

fn (mut c Checker) for_c_stmt(node ast.ForCStmt) {
	c.in_for_count++
	prev_loop_label := c.loop_label
	c.stmt(node.init)
	c.expr(node.cond)
	c.stmt(node.inc)
	c.check_loop_label(node.label, node.pos)
	c.stmts(node.stmts)
	c.loop_label = prev_loop_label
	c.in_for_count--
}

fn (mut c Checker) for_in_stmt(mut node ast.ForInStmt) {
	c.in_for_count++
	prev_loop_label := c.loop_label
	typ := c.expr(node.cond)
	typ_idx := typ.idx()
	if node.key_var.len > 0 && node.key_var != '_' {
		c.check_valid_snake_case(node.key_var, 'variable name', node.pos)
	}
	if node.val_var.len > 0 && node.val_var != '_' {
		c.check_valid_snake_case(node.val_var, 'variable name', node.pos)
	}
	if node.is_range {
		high_type := c.expr(node.high)
		high_type_idx := high_type.idx()
		if typ_idx in table.integer_type_idxs && high_type_idx !in table.integer_type_idxs {
			c.error('range types do not match', node.cond.position())
		} else if typ_idx in table.float_type_idxs || high_type_idx in table.float_type_idxs {
			c.error('range type can not be float', node.cond.position())
		} else if typ_idx == table.bool_type_idx || high_type_idx == table.bool_type_idx {
			c.error('range type can not be bool', node.cond.position())
		} else if typ_idx == table.string_type_idx || high_type_idx == table.string_type_idx {
			c.error('range type can not be string', node.cond.position())
		}
	} else {
		sym := c.table.get_type_symbol(typ)
		if sym.kind == .struct_ {
			// iterators
			next_fn := sym.find_method('next') or {
				c.error('a struct must have a `next()` method to be an iterator', node.cond.position())
				return
			}
			if !next_fn.return_type.has_flag(.optional) {
				c.error('iterator method `next()` must return an optional', node.cond.position())
			}
			// the receiver
			if next_fn.params.len != 1 {
				c.error('iterator method `next()` must have 0 parameters', node.cond.position())
			}
			val_type := next_fn.return_type.clear_flag(.optional)
			node.cond_type = typ
			node.kind = sym.kind
			node.val_type = val_type
			node.scope.update_var_type(node.val_var, val_type)
		} else {
			if sym.kind == .map && !(node.key_var.len > 0 && node.val_var.len > 0) {
				c.error(
					'declare a key and a value variable when ranging a map: `for key, val in map {`\n' +
					'use `_` if you do not need the variable', node.pos)
			}
			if node.key_var.len > 0 {
				key_type := match sym.kind {
					.map { sym.map_info().key_type }
					else { table.int_type }
				}
				node.key_type = key_type
				node.scope.update_var_type(node.key_var, key_type)
			}
			mut value_type := c.table.value_type(typ)
			if value_type == table.void_type || typ.has_flag(.optional) {
				if typ != table.void_type {
					c.error('for in: cannot index `${c.table.type_to_str(typ)}`', node.cond.position())
				}
			}
			if node.val_is_mut {
				value_type = value_type.to_ptr()
				match node.cond {
					ast.Ident {
						if node.cond.obj is ast.Var {
							obj := node.cond.obj as ast.Var
							if !obj.is_mut {
								c.error('`$obj.name` is immutable, it cannot be changed',
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
					else {}
				}
			}
			node.cond_type = typ
			node.kind = sym.kind
			node.val_type = value_type
			node.scope.update_var_type(node.val_var, value_type)
		}
	}
	c.check_loop_label(node.label, node.pos)
	c.stmts(node.stmts)
	c.loop_label = prev_loop_label
	c.in_for_count--
}

fn (mut c Checker) for_stmt(mut node ast.ForStmt) {
	c.in_for_count++
	prev_loop_label := c.loop_label
	c.expected_type = table.bool_type
	typ := c.expr(node.cond)
	if !node.is_inf && typ.idx() != table.bool_type_idx && !c.pref.translated {
		c.error('non-bool used as for condition', node.pos)
	}
	if node.cond is ast.InfixExpr {
		infix := node.cond
		if infix.op == .key_is {
			if (infix.left is ast.Ident || infix.left is ast.SelectorExpr)
				&& infix.right is ast.Type {
				right_expr := infix.right as ast.Type
				is_variable := if mut infix.left is ast.Ident {
					infix.left.kind == .variable
				} else {
					true
				}
				left_type := c.expr(infix.left)
				left_sym := c.table.get_type_symbol(left_type)
				if is_variable {
					if left_sym.kind == .sum_type {
						c.smartcast_sumtype(infix.left, infix.left_type, right_expr.typ, mut
							node.scope)
					}
				}
			}
		}
	}
	// TODO: update loop var type
	// how does this work currenly?
	c.check_loop_label(node.label, node.pos)
	c.stmts(node.stmts)
	c.loop_label = prev_loop_label
	c.in_for_count--
}
