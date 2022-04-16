module checker

import v.ast
import v.token
import v.util

// unwrap_generic unwrap the concrete type.
fn (mut c Checker) unwrap_generic(typ ast.Type) ast.Type {
	if typ.has_flag(.generic) {
		if t_typ := c.table.resolve_generic_to_concrete(typ, c.table.cur_fn.generic_names,
			c.table.cur_concrete_types)
		{
			return t_typ
		}
	}
	return typ
}

// ensure_type_exists check if the type was found somewhere before.
fn (mut c Checker) ensure_type_exists(typ ast.Type, pos token.Pos) ? {
	if typ == 0 {
		c.error('unknown type', pos)
		return
	}
	sym := c.table.sym(typ)
	match sym.kind {
		.placeholder {
			if sym.language == .v && !sym.name.starts_with('C.') {
				c.error(util.new_suggestion(sym.name, c.table.known_type_names()).say('unknown type `$sym.name`'),
					pos)
				return
			}
		}
		.int_literal, .float_literal {
			// Separate error condition for `int_literal` and `float_literal` because `util.suggestion` may give different
			// suggestions due to f32 comparision issue.
			if !c.is_builtin_mod {
				msg := if sym.kind == .int_literal {
					'unknown type `$sym.name`.\nDid you mean `int`?'
				} else {
					'unknown type `$sym.name`.\nDid you mean `f64`?'
				}
				c.error(msg, pos)
				return
			}
		}
		.array {
			c.ensure_type_exists((sym.info as ast.Array).elem_type, pos) ?
		}
		.array_fixed {
			c.ensure_type_exists((sym.info as ast.ArrayFixed).elem_type, pos) ?
		}
		.map {
			info := sym.info as ast.Map
			c.ensure_type_exists(info.key_type, pos) ?
			c.ensure_type_exists(info.value_type, pos) ?
		}
		.sum_type {
			info := sym.info as ast.SumType
			for concrete_typ in info.concrete_types {
				c.ensure_type_exists(concrete_typ, pos) ?
			}
		}
		else {}
	}
}
