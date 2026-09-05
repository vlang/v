import orm

fn test_orm_table_gen_string_defaults_are_escaped_per_dialect() {
	table := orm.Table{
		name: 'test_table'
	}
	// MySQL treats backslash as an escape character inside string literals (unless
	// the server runs with NO_BACKSLASH_ESCAPES), so a literal backslash has to be
	// doubled there. Standard SQL dialects store it as is.
	// See https://github.com/vlang/v/issues/27987
	fields := [
		orm.TableField{
			name:     'win_path'
			typ:      typeof[string]().idx
			nullable: true
			attrs:    [
				VAttribute{
					name:    'default'
					has_arg: true
					arg:     r'`C:\tmp\new`'
					kind:    .string
				},
			]
		},
		orm.TableField{
			name:     'mixed'
			typ:      typeof[string]().idx
			nullable: true
			attrs:    [
				VAttribute{
					name:    'default'
					has_arg: true
					arg:     r"`o'brien\x`"
					kind:    .string
				},
			]
		},
	]
	mysql_query := orm.orm_table_gen(.mysql, table, '`', true, 0, fields, dialect_sql_type_from_v,
		false) or { panic(err) }
	assert mysql_query.contains(r"DEFAULT 'C:\\tmp\\new'"), mysql_query
	assert mysql_query.contains(r"DEFAULT 'o''brien\\x'"), mysql_query

	for dialect in [orm.SQLDialect.default, .pg, .sqlite, .h2] {
		query := orm.orm_table_gen(dialect, table, '`', true, 0, fields, dialect_sql_type_from_v,
			false) or { panic(err) }
		assert query.contains(r"DEFAULT 'C:\tmp\new'"), '${dialect}: ${query}'
		assert query.contains(r"DEFAULT 'o''brien\x'"), '${dialect}: ${query}'
	}
}

fn dialect_sql_type_from_v(typ int) !string {
	return if typ == orm.type_idx['int'] { 'INT' } else { 'TEXT' }
}
