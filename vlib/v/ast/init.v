module ast

pub fn resolve_init(node StructInit, typ Type, t &Table) Expr {
	type_sym := t.get_type_symbol(typ)
	if type_sym.kind == .array {
		array_info := type_sym.info as Array
		mut has_len := false
		mut has_cap := false
		mut has_default := false
		mut len_expr := empty_expr()
		mut cap_expr := empty_expr()
		mut default_expr := empty_expr()
		mut exprs := []Expr{}
		for field in node.fields {
			match field.name {
				'len' {
					has_len = true
					len_expr = field.expr
				}
				'cap' {
					has_cap = true
					len_expr = field.expr
				}
				'default' {
					has_default = true
					len_expr = field.expr
				}
				else {
					exprs << field.expr
				}
			}
		}
		return ArrayInit{
			// TODO: mod is not being set for now, we could need this in future
			// mod: mod
			pos: node.pos
			typ: typ
			elem_type: array_info.elem_type
			has_len: has_len
			has_cap: has_cap
			has_default: has_default
			len_expr: len_expr
			cap_expr: cap_expr
			default_expr: default_expr
			exprs: exprs
		}
	} else if type_sym.kind == .map {
		map_info := type_sym.info as Map
		mut keys := []Expr{}
		mut vals := []Expr{}
		for field in node.fields {
			keys << StringLiteral{
				val: field.name
			}
			vals << field.expr
		}
		return MapInit{
			typ: typ
			key_type: map_info.key_type
			value_type: map_info.value_type
			keys: keys
			vals: vals
		}
	}
	// struct / other (sumtype?)
	return StructInit{
		...node
		unresolved: false
	}
}
