module transform

import v3.flat

// transform_sql_expr transforms SQL/ORM expressions before C generation.
// Full ORM lowering will build driver calls here; until then, keep the existing
// v3 fallback behavior by replacing SQL expressions with a successful typed
// default value.
fn (mut t Transformer) transform_sql_expr(_id flat.NodeId, node flat.Node) flat.NodeId {
	checker_typ := t.raw_checker_node_type(_id)
	typ := if checker_typ.len > 0 {
		checker_typ
	} else if node.typ.len > 0 {
		node.typ
	} else {
		'!void'
	}
	if t.is_optional_type_name(typ) {
		opt_type := t.qualify_optional_type(typ)
		value_type := t.optional_base_type(opt_type)
		value := if value_type.len > 0 && value_type != 'void' {
			t.zero_value_for_type(value_type)
		} else {
			flat.empty_node
		}
		return t.make_optional_some(value, opt_type)
	}
	return t.zero_value_for_type(typ)
}
