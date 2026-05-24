// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module transformer

import v2.ast
import v2.types

// is_interface_var checks if a variable is an interface type by looking up its type in scope
fn (t &Transformer) is_interface_var(name string) bool {
	// Special case: 'err' in or-blocks is always IError interface
	if name == 'err' {
		return true
	}
	typ := t.lookup_var_type(name) or { return false }
	return typ is types.Interface
}

// is_interface_receiver checks if an expression's type is an interface type
fn (t &Transformer) is_interface_receiver(expr ast.Expr) bool {
	match expr {
		ast.Ident {
			return t.is_interface_var(expr.name)
		}
		ast.ParenExpr {
			return t.is_interface_receiver(expr.expr)
		}
		ast.ModifierExpr {
			return t.is_interface_receiver(expr.expr)
		}
		else {}
	}

	if typ := t.get_interface_receiver_type(expr) {
		base := t.unwrap_alias_and_pointer_type(typ)
		return base is types.Interface
	}
	return false
}

fn (t &Transformer) get_interface_receiver_type(expr ast.Expr) ?types.Type {
	if typ := t.get_expr_type(expr) {
		return typ
	}
	if typ := t.resolve_expr_type(expr) {
		return typ
	}
	if expr is ast.IndexExpr {
		if expr.expr is ast.RangeExpr {
			return none
		}
		if container_type := t.get_expr_type(expr.lhs) {
			base := t.unwrap_alias_and_pointer_type(container_type)
			if base is types.Array {
				return base.elem_type
			}
			if base is types.ArrayFixed {
				return base.elem_type
			}
			if base is types.Map {
				return base.value_type
			}
		}
	}
	return none
}

// is_interface_cast checks if an expression is an interface cast like Calculator(value)
fn (t &Transformer) is_interface_cast(expr ast.Expr) bool {
	// Interface casts appear as CallOrCastExpr: InterfaceType(value)
	if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			// Look up the type name in the environment
			type_name := (expr.lhs as ast.Ident).name
			mut scope := t.get_current_scope() or { return false }
			obj := scope.lookup_parent(type_name, 0) or { return false }
			if obj is types.Type {
				return obj is types.Interface
			}
		}
	}
	return false
}

// get_expr_type_name returns the concrete type name for an expression using the type checker env
fn (t &Transformer) get_expr_type_name(expr ast.Expr) ?string {
	if typ := t.get_expr_type(expr) {
		name := t.type_to_c_name(typ)
		if name != '' {
			return name
		}
	}
	// For simple identifiers, try scope lookup
	if expr is ast.Ident {
		if typ := t.lookup_var_type(expr.name) {
			name := t.type_to_c_name(typ)
			if name != '' {
				return name
			}
		}
	}
	return none
}

fn (t &Transformer) get_interface_cast_concrete_type(expr ast.Expr) ?string {
	match expr {
		ast.ParenExpr {
			return t.get_interface_cast_concrete_type(expr.expr)
		}
		ast.CallOrCastExpr {
			if t.is_interface_cast(expr) {
				if concrete := t.get_expr_type_name(expr.expr) {
					return concrete
				}
			}
		}
		ast.CastExpr {
			type_name := t.expr_to_type_name(expr.typ)
			if type_name != '' {
				if typ := t.lookup_type(type_name) {
					if typ is types.Interface {
						if concrete := t.get_expr_type_name(expr.expr) {
							return concrete
						}
					}
				}
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) get_interface_cast_inner_expr(expr ast.Expr) ?ast.Expr {
	match expr {
		ast.ParenExpr {
			return t.get_interface_cast_inner_expr(expr.expr)
		}
		ast.CallOrCastExpr {
			if t.is_interface_cast(expr) {
				return expr.expr
			}
		}
		ast.CastExpr {
			type_name := t.expr_to_type_name(expr.typ)
			if type_name != '' {
				if typ := t.lookup_type(type_name) {
					if typ is types.Interface {
						return expr.expr
					}
				}
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) get_interface_array_init_concrete_type(expr ast.ArrayInitExpr) ?string {
	mut concrete_type := ''
	for e in expr.exprs {
		concrete := t.get_interface_cast_concrete_type(e) or { return none }
		if concrete_type == '' {
			concrete_type = concrete
		} else if concrete_type != concrete {
			return none
		}
	}
	if concrete_type != '' {
		return concrete_type
	}
	return none
}

fn (t &Transformer) get_interface_array_expr_concrete_type(expr ast.Expr) ?string {
	match expr {
		ast.Ident {
			return t.get_interface_concrete_type(expr.name)
		}
		ast.ParenExpr {
			return t.get_interface_array_expr_concrete_type(expr.expr)
		}
		ast.ArrayInitExpr {
			return t.get_interface_array_init_concrete_type(expr)
		}
		ast.CallExpr {
			if expr.lhs is ast.SelectorExpr {
				sel := expr.lhs as ast.SelectorExpr
				if sel.rhs.name in ['repeat', 'clone'] {
					return t.get_interface_array_expr_concrete_type(sel.lhs)
				}
			}
		}
		ast.CallOrCastExpr {
			if expr.lhs is ast.SelectorExpr {
				sel := expr.lhs as ast.SelectorExpr
				if sel.rhs.name in ['repeat', 'clone'] {
					return t.get_interface_array_expr_concrete_type(sel.lhs)
				}
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) get_interface_assignment_concrete_type(expr ast.Expr) ?string {
	if concrete := t.get_interface_cast_concrete_type(expr) {
		return concrete
	}
	if concrete := t.get_interface_array_expr_concrete_type(expr) {
		return concrete
	}
	if expr is ast.Ident {
		return t.get_interface_concrete_type(expr.name)
	}
	if expr is ast.ParenExpr {
		return t.get_interface_assignment_concrete_type(expr.expr)
	}
	return none
}

fn (t &Transformer) get_interface_concrete_type_for_expr(expr ast.Expr) ?string {
	match expr {
		ast.Ident {
			return t.get_interface_concrete_type(expr.name)
		}
		ast.ParenExpr {
			return t.get_interface_concrete_type_for_expr(expr.expr)
		}
		ast.ModifierExpr {
			return t.get_interface_concrete_type_for_expr(expr.expr)
		}
		ast.IndexExpr {
			return t.get_interface_array_expr_concrete_type(expr.lhs)
		}
		else {}
	}

	return none
}

fn (t &Transformer) get_native_default_interface_concrete_type(expr ast.Expr, method_name string) ?string {
	prng_methods := ['seed', 'u8', 'u16', 'u32', 'u64', 'block_size', 'free']
	if method_name !in prng_methods {
		return none
	}
	if t.cur_module == 'rand' {
		return 'wyrand__WyRandRNG'
	}
	receiver_type := t.get_interface_receiver_type(expr) or { return none }
	base := t.unwrap_alias_and_pointer_type(receiver_type)
	if base !is types.Interface {
		return none
	}
	iface_name := t.type_to_c_name(base)
	if iface_name !in ['PRNG', 'rand__PRNG'] {
		return none
	}
	return 'wyrand__WyRandRNG'
}

fn (mut t Transformer) native_interface_receiver_arg(expr ast.Expr, concrete string) ast.Expr {
	concrete_type := t.c_name_to_type(concrete) or { return expr }
	return t.native_interface_receiver_arg_with_type(expr, concrete_type)
}

fn (mut t Transformer) native_interface_receiver_arg_with_type(expr ast.Expr, concrete_type types.Type) ast.Expr {
	match expr {
		ast.IndexExpr {
			pos := t.next_synth_pos()
			t.register_synth_type(pos, concrete_type)
			return ast.Expr(ast.IndexExpr{
				lhs:      expr.lhs
				expr:     expr.expr
				is_gated: expr.is_gated
				pos:      pos
			})
		}
		ast.ParenExpr {
			return ast.Expr(ast.ParenExpr{
				expr: t.native_interface_receiver_arg_with_type(expr.expr, concrete_type)
				pos:  expr.pos
			})
		}
		ast.ModifierExpr {
			return ast.Expr(ast.ModifierExpr{
				kind: expr.kind
				expr: t.native_interface_receiver_arg_with_type(expr.expr, concrete_type)
				pos:  expr.pos
			})
		}
		else {}
	}

	return expr
}

// get_interface_concrete_type returns the concrete type name for an interface variable
// if it was tracked during interface cast lowering (native backend only)
fn (t &Transformer) get_interface_concrete_type(name string) ?string {
	if concrete := t.interface_concrete_types[name] {
		return concrete
	}
	return none
}
