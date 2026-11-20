import orm

fn mssql_type_from_v(typ int) !string {
	return if typ == orm.type_idx['int'] { 'INT' } else { 'NVARCHAR(255)' }
}

fn test_mssql_empty_insert_uses_default_values() {
	query, _ := orm.orm_stmt_gen(.mssql, orm.Table{ name: 'users' }, '"', .insert, false, '?', 1,
		orm.QueryData{}, orm.QueryData{})
	assert query == 'INSERT INTO "users" DEFAULT VALUES;', query
}

fn test_mssql_select_limit_only_adds_order_by() {
	cfg := orm.SelectConfig{
		table:     orm.Table{ name: 'users' }
		fields:    ['id']
		types:     [orm.type_idx['int']]
		has_limit: true
	}
	query := orm.orm_select_gen_with_dialect(.mssql, cfg, '"', false, '?', 1, orm.QueryData{})
	assert query == 'SELECT "id" FROM "users" ORDER BY (SELECT NULL) OFFSET 0 ROWS FETCH NEXT ? ROWS ONLY;', query
}

fn test_mssql_select_keeps_existing_order_by() {
	cfg := orm.SelectConfig{
		table:      orm.Table{ name: 'users' }
		fields:     ['id']
		types:      [orm.type_idx['int']]
		has_order:  true
		order:      'id'
		order_type: .asc
		has_limit:  true
	}
	query := orm.orm_select_gen_with_dialect(.mssql, cfg, '"', false, '?', 1, orm.QueryData{})
	assert query == 'SELECT "id" FROM "users" ORDER BY "id" ASC OFFSET 0 ROWS FETCH NEXT ? ROWS ONLY;', query
}

fn test_mssql_select_offset_precedes_fetch_with_numbered_markers() {
	cfg := orm.SelectConfig{
		table:      orm.Table{ name: 'users' }
		fields:     ['id']
		types:      [orm.type_idx['int']]
		has_limit:  true
		has_offset: true
	}
	query := orm.orm_select_gen_with_dialect(.mssql, cfg, '"', true, '@p', 1, orm.QueryData{})
	assert query.ends_with('OFFSET @p2 ROWS FETCH NEXT @p1 ROWS ONLY;'), query
}

fn test_mssql_table_gen_uses_string_literals_in_guard() {
	fields := [
		orm.TableField{
			name: 'id'
			typ:  orm.type_idx['int']
		},
	]
	query := orm.orm_table_gen(.mssql, orm.Table{ name: 'users' }, '"', true, 0, fields,
		mssql_type_from_v, true)!
	assert query.starts_with('IF NOT EXISTS (SELECT * FROM sysobjects WHERE name=\'users\' and xtype=\'U\') CREATE TABLE "users" ('), query
}

fn test_mssql_table_gen_escapes_quote_in_name_lookup() {
	fields := [
		orm.TableField{
			name: 'id'
			typ:  orm.type_idx['int']
		},
	]
	query := orm.orm_table_gen(.mssql, orm.Table{ name: "o'brien" }, '"', true, 0, fields,
		mssql_type_from_v, true)!
	assert query.contains("WHERE name='o''brien' and xtype='U'"), query
	assert query.contains('CREATE TABLE "o\'brien" ('), query
}
