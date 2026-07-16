module transform

import v3.flat
import v3.types

struct SqlTransformTableInfo {
	name   string
	fields []types.StructField
}

// transform_sql_expr transforms SQL/ORM expressions before C generation.
// Full ORM lowering will build driver calls here; until then, keep the existing
// v3 fallback behavior by replacing SQL expressions with a successful typed
// default value.
fn (mut t Transformer) transform_sql_expr(_id flat.NodeId, node flat.Node) flat.NodeId {
	if t.transform_sql_expr_is_basic_sqlite(node) {
		return _id
	}
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

fn (t &Transformer) transform_sql_expr_is_basic_sqlite(node flat.Node) bool {
	tokens := node.value.split(' ')
	if tokens.len == 0 || node.children_count == 0 {
		return false
	}
	if !t.sql_expr_uses_sqlite_db(node) {
		return false
	}
	if tokens[0] == 'select' {
		return t.transform_sql_select_is_basic(tokens)
	}
	mut i := 0
	mut saw_supported := false
	for i < tokens.len {
		if i + 2 < tokens.len && tokens[i] == 'create' && tokens[i + 1] == 'table' {
			if !t.sql_table_is_basic_sqlite_supported(tokens[i + 2]) {
				return false
			}
			saw_supported = true
			i += 3
			continue
		}
		if i + 3 < tokens.len && tokens[i] == 'insert' && tokens[i + 2] == 'into' {
			if !t.sql_table_is_basic_sqlite_supported(tokens[i + 3])
				|| !t.sql_insert_value_is_scalar_struct(tokens[i + 1], tokens[i + 3]) {
				return false
			}
			saw_supported = true
			i += 4
			continue
		}
		return false
	}
	return saw_supported
}

fn (t &Transformer) sql_expr_uses_sqlite_db(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	db_id := t.a.child(&node, 0)
	raw := t.raw_checker_node_type(db_id)
	for typ in [raw, t.normalize_type_alias(raw), t.node_type(db_id)] {
		clean := t.trim_pointer_type(typ)
		if clean == 'sqlite.DB' {
			return true
		}
	}
	return false
}

fn (t &Transformer) transform_sql_select_is_basic(tokens []string) bool {
	if tokens.len == 3 {
		return tokens[1] == 'from' && t.sql_table_is_basic_sqlite_supported(tokens[2])
	}
	return tokens.len == 5 && tokens[1] == 'from' && tokens[3] == 'limit'
		&& t.sql_table_is_basic_sqlite_supported(tokens[2])
		&& t.sql_limit_is_basic_sqlite_supported(tokens[4])
}

fn (t &Transformer) sql_limit_is_basic_sqlite_supported(token string) bool {
	if token.len == 0 {
		return false
	}
	if token.bytes().all(it >= `0` && it <= `9`) {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	if _ := t.tc.const_int_value_in_module(token, t.cur_module, []string{}) {
		return true
	}
	if _ := t.tc.const_int_value(token, []string{}) {
		return true
	}
	return false
}

fn (t &Transformer) sql_table_is_basic_sqlite_supported(table string) bool {
	if !sql_token_is_plain_ident(table) || isnil(t.tc) {
		return false
	}
	fields := t.sql_table_fields(table) or { return false }
	for field in fields {
		if !sql_field_is_basic_sqlite_supported(field) {
			return false
		}
	}
	return true
}

fn (t &Transformer) sql_table_fields(table string) ?[]types.StructField {
	info := t.sql_table_info(table) or { return none }
	return info.fields
}

fn (t &Transformer) sql_table_info(table string) ?SqlTransformTableInfo {
	if fields := t.tc.structs[table] {
		return SqlTransformTableInfo{
			name:   table
			fields: fields
		}
	}
	if t.tc.cur_module.len > 0 && t.tc.cur_module !in ['', 'main', 'builtin'] {
		qualified := '${t.tc.cur_module}.${table}'
		if fields := t.tc.structs[qualified] {
			return SqlTransformTableInfo{
				name:   qualified
				fields: fields
			}
		}
	}
	return none
}

fn sql_field_is_basic_sqlite_supported(field types.StructField) bool {
	if field.name == 'id' && field.typ.is_integer() {
		return true
	}
	if field.typ is types.String {
		return true
	}
	return field.typ.is_integer()
}

fn (t &Transformer) sql_insert_value_is_scalar_struct(value_name string, table_name string) bool {
	if !sql_token_is_plain_ident(value_name) || !sql_token_is_plain_ident(table_name) {
		return false
	}
	typ := t.sql_insert_value_type_name(value_name)
	if typ.len == 0 || typ.starts_with('[]') || typ.starts_with('&') {
		return false
	}
	table_info := t.sql_table_info(table_name) or { return false }
	return typ == table_info.name
}

fn (t &Transformer) sql_insert_value_type_name(value_name string) string {
	mut typ := t.var_type(value_name)
	if typ.len == 0 && !isnil(t.tc) {
		if current := t.tc.cur_scope.lookup(value_name) {
			typ = current.name()
		} else if file := t.tc.file_scope.lookup(value_name) {
			typ = file.name()
		}
	}
	return typ
}

fn sql_token_is_plain_ident(token string) bool {
	if token.len == 0 {
		return false
	}
	for ch in token.bytes() {
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch == `_` || ch == `.`) {
			return false
		}
	}
	return true
}
