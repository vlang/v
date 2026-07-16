module c

import v3.flat
import v3.types

struct SqlTableInfo {
	name   string
	fields []types.StructField
}

fn (mut g FlatGen) gen_sql_expr(_id flat.NodeId, node flat.Node) bool {
	tokens := node.value.split(' ')
	if tokens.len == 0 || node.children_count == 0 {
		return false
	}
	db_id := g.a.child(&node, 0)
	db_expr := g.expr_to_string(db_id)
	db_type := g.tc.resolve_type(db_id)
	db_conn := if db_type is types.Pointer {
		'(${db_expr})->conn'
	} else {
		'(${db_expr}).conn'
	}
	if tokens[0] == 'select' {
		return g.gen_sql_select_expr(tokens, db_conn)
	}
	return g.gen_sql_exec_expr(tokens, db_conn)
}

fn (mut g FlatGen) gen_sql_exec_expr(tokens []string, db_conn string) bool {
	mut parts := []string{}
	parts << sql_sqlite_externs()
	mut i := 0
	for i < tokens.len {
		if i + 2 < tokens.len && tokens[i] == 'create' && tokens[i + 1] == 'table' {
			table := sql_clean_ident(tokens[i + 2])
			query := g.sql_create_table_query(table) or { return false }
			sid := g.intern_string(query)
			parts << 'sqlite3_exec(${db_conn}, (const char*)_str_${sid}.str, NULL, NULL, NULL);'
			i += 3
			continue
		}
		if i + 3 < tokens.len && tokens[i] == 'insert' && tokens[i + 2] == 'into' {
			value_name := sql_clean_ident(tokens[i + 1])
			table := sql_clean_ident(tokens[i + 3])
			insert := g.sql_insert_stmt(table, value_name) or { return false }
			parts << insert.replace(g.sql_current_db_conn_placeholder(), db_conn)
			i += 4
			continue
		}
		return false
	}
	g.write('({ ${parts.join(' ')} (Optional){.ok = true}; })')
	return true
}

fn (mut g FlatGen) gen_sql_select_expr(tokens []string, db_conn string) bool {
	if tokens.len < 3 || tokens[1] != 'from' {
		return false
	}
	table := sql_clean_ident(tokens[2])
	table_info := g.sql_table_info(table) or { return false }
	fields := table_info.fields
	table_ct := g.cname(table_info.name)
	query := g.sql_select_query(tokens, table, fields) or { return false }
	sid := g.intern_string(query)
	arr_name := g.tmp_name()
	stmt_name := g.tmp_name()
	row_name := g.tmp_name()
	parts := [
		sql_sqlite_externs(),
		'Array ${arr_name} = array_new(sizeof(${table_ct}), 0, 0);',
		'struct sqlite3_stmt* ${stmt_name} = NULL;',
		'if (sqlite3_prepare_v2(${db_conn}, (const char*)_str_${sid}.str, -1, &${stmt_name}, NULL) == 0) {',
		'while (sqlite3_step(${stmt_name}) == 100) {',
		'${table_ct} ${row_name} = (${table_ct}){0};',
	]
	mut mutable_parts := parts.clone()
	for idx, field in fields {
		read_stmt := g.sql_read_column_stmt(row_name, field, idx) or { return false }
		mutable_parts << read_stmt.replace(g.sql_current_stmt_placeholder(), stmt_name)
	}
	mutable_parts << 'array_push(&${arr_name}, &${row_name});'
	mutable_parts << '} }'
	mutable_parts << 'if (${stmt_name} != NULL) sqlite3_finalize(${stmt_name});'
	mutable_parts << '(Optional_Array){.ok = true, .value = ${arr_name}};'
	g.write('({ ${mutable_parts.join(' ')} })')
	return true
}

fn sql_sqlite_externs() string {
	return 'extern int sqlite3_exec(struct sqlite3*, const char*, int (*)(void*, int, char**, char**), void*, char**); extern int sqlite3_prepare_v2(struct sqlite3*, const char*, int, struct sqlite3_stmt**, const char**); extern int sqlite3_step(struct sqlite3_stmt*); extern int sqlite3_finalize(struct sqlite3_stmt*); extern const unsigned char* sqlite3_column_text(struct sqlite3_stmt*, int); extern int sqlite3_column_bytes(struct sqlite3_stmt*, int); extern int sqlite3_column_int(struct sqlite3_stmt*, int); extern long long sqlite3_column_int64(struct sqlite3_stmt*, int); extern int sqlite3_bind_int(struct sqlite3_stmt*, int, int); extern int sqlite3_bind_int64(struct sqlite3_stmt*, int, long long); extern int sqlite3_bind_text(struct sqlite3_stmt*, int, const char*, int, void (*)(void*));'
}

fn (mut g FlatGen) sql_insert_stmt(table string, value_name string) ?string {
	fields := g.sql_insert_fields(table) or { return none }
	if fields.len == 0 {
		return none
	}
	mut names := []string{}
	mut placeholders := []string{}
	for field in fields {
		names << '"${field.name}"'
		placeholders << '?'
	}
	query := 'INSERT INTO "${table}" (${names.join(', ')}) VALUES (${placeholders.join(', ')});'
	sid := g.intern_string(query)
	stmt_name := g.tmp_name()
	mut parts := [
		'struct sqlite3_stmt* ${stmt_name} = NULL;',
		'if (sqlite3_prepare_v2(${g.sql_current_db_conn_placeholder()}, (const char*)_str_${sid}.str, -1, &${stmt_name}, NULL) == 0) {',
	]
	for idx, field in fields {
		bind_stmt := g.sql_bind_field_stmt(stmt_name, value_name, field, idx + 1) or { return none }
		parts << bind_stmt
	}
	parts << 'sqlite3_step(${stmt_name});'
	parts << '}'
	parts << 'if (${stmt_name} != NULL) sqlite3_finalize(${stmt_name});'
	return parts.join(' ')
}

fn (g &FlatGen) sql_current_db_conn_placeholder() string {
	return '__SQL_DB_CONN__'
}

fn (mut g FlatGen) sql_create_table_query(table string) ?string {
	fields := g.sql_table_fields(table) or { return none }
	mut columns := []string{}
	for field in fields {
		column := g.sql_column_decl(field) or { return none }
		columns << column
	}
	return 'CREATE TABLE IF NOT EXISTS "${table}" (${columns.join(', ')});'
}

fn (mut g FlatGen) sql_select_query(tokens []string, table string, fields []types.StructField) ?string {
	mut columns := []string{}
	for field in fields {
		columns << '"${field.name}"'
	}
	mut query := 'SELECT ${columns.join(', ')} FROM "${table}"'
	if limit_idx := sql_token_index(tokens, 'limit') {
		if limit_idx + 1 < tokens.len {
			limit := g.sql_limit_value(tokens[limit_idx + 1]) or { return none }
			query += ' LIMIT ${limit}'
		}
	}
	return '${query};'
}

fn (g &FlatGen) sql_limit_value(token string) ?int {
	if token.len == 0 {
		return none
	}
	if token.bytes().all(it >= `0` && it <= `9`) {
		return token.int()
	}
	if v := g.tc.const_int_value_in_module(token, g.tc.cur_module, []string{}) {
		return v
	}
	return g.tc.const_int_value(token, []string{})
}

fn sql_token_index(tokens []string, needle string) ?int {
	for i, tok in tokens {
		if tok == needle {
			return i
		}
	}
	return none
}

fn (mut g FlatGen) sql_insert_fields(table string) ?[]types.StructField {
	fields := g.sql_table_fields(table) or { return none }
	mut out := []types.StructField{}
	for field in fields {
		if field.name == 'id' && field.typ.is_integer() {
			continue
		}
		out << field
	}
	return out
}

fn (mut g FlatGen) sql_table_fields(table string) ?[]types.StructField {
	info := g.sql_table_info(table) or { return none }
	return info.fields
}

fn (mut g FlatGen) sql_table_info(table string) ?SqlTableInfo {
	if fields := g.tc.structs[table] {
		return SqlTableInfo{
			name:   table
			fields: fields
		}
	}
	if g.tc.cur_module.len > 0 && g.tc.cur_module !in ['', 'main', 'builtin'] {
		qualified := '${g.tc.cur_module}.${table}'
		if fields := g.tc.structs[qualified] {
			return SqlTableInfo{
				name:   qualified
				fields: fields
			}
		}
	}
	return none
}

fn (g &FlatGen) sql_column_decl(field types.StructField) ?string {
	if field.name == 'id' && field.typ.is_integer() {
		return '"${field.name}" INTEGER PRIMARY KEY AUTOINCREMENT'
	}
	if field.typ is types.String {
		return '"${field.name}" TEXT'
	}
	if field.typ.is_integer() {
		return '"${field.name}" INTEGER'
	}
	return none
}

fn (g &FlatGen) sql_bind_field_stmt(stmt_name string, value_name string, field types.StructField, idx int) ?string {
	field_expr := '(${value_name}).${c_field_name(field.name)}'
	if field.typ is types.String {
		return 'sqlite3_bind_text(${stmt_name}, ${idx}, (const char*)${field_expr}.str, ${field_expr}.len, (void (*)(void*))-1);'
	}
	if field.typ.is_integer() {
		if sql_integer_uses_int64(field.typ) {
			return 'sqlite3_bind_int64(${stmt_name}, ${idx}, (i64)${field_expr});'
		}
		return 'sqlite3_bind_int(${stmt_name}, ${idx}, (int)${field_expr});'
	}
	return none
}

fn (mut g FlatGen) sql_read_column_stmt(row_name string, field types.StructField, idx int) ?string {
	field_name := c_field_name(field.name)
	if field.typ is types.String {
		tmp := '${row_name}_${field_name}_${idx}'
		empty_sid := g.intern_string('')
		return 'const unsigned char* ${tmp}_txt = sqlite3_column_text(${g.sql_current_stmt_placeholder()}, ${idx}); int ${tmp}_len = sqlite3_column_bytes(${g.sql_current_stmt_placeholder()}, ${idx}); if (${tmp}_txt != NULL) { u8* ${tmp}_copy = malloc_noscan(${tmp}_len + 1); memcpy(${tmp}_copy, ${tmp}_txt, ${tmp}_len); ${tmp}_copy[${tmp}_len] = 0; ${row_name}.${field_name} = (string){.str = ${tmp}_copy, .len = ${tmp}_len, .is_lit = 0}; } else { ${row_name}.${field_name} = _str_${empty_sid}; }'
	}
	if field.typ.is_integer() {
		if sql_integer_uses_int64(field.typ) {
			ct := g.tc.c_type(field.typ)
			return '${row_name}.${field_name} = (${ct})sqlite3_column_int64(${g.sql_current_stmt_placeholder()}, ${idx});'
		}
		return '${row_name}.${field_name} = sqlite3_column_int(${g.sql_current_stmt_placeholder()}, ${idx});'
	}
	return none
}

fn sql_integer_uses_int64(typ types.Type) bool {
	if typ is types.Primitive {
		return typ.props.has(.integer)
			&& (typ.size == 64 || (typ.size == 32 && typ.props.has(.unsigned)))
	}
	return typ is types.ISize || typ is types.USize
}

fn (g &FlatGen) sql_current_stmt_placeholder() string {
	return '__SQL_STMT__'
}

fn sql_clean_ident(token string) string {
	mut out := ''
	for ch in token.bytes() {
		if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch == `_` || ch == `.` {
			out += ch.ascii_str()
		}
	}
	return out
}
