import orm

// A backtick delimited `default:` value is rendered as a quoted SQL literal.
// Single quotes are doubled on every dialect. A backslash has no single MySQL
// spelling - its meaning depends on the server's NO_BACKSLASH_ESCAPES sql_mode,
// which orm_table_gen() cannot know - so it is rejected there instead of being
// stored as one or two backslashes depending on the mode.
// See https://github.com/vlang/v/issues/27987
fn dialect_fields(default_arg string) []orm.TableField {
	return [
		orm.TableField{
			name:     'val'
			typ:      typeof[string]().idx
			nullable: true
			attrs:    [
				VAttribute{
					name:    'default'
					has_arg: true
					arg:     default_arg
					kind:    .string
				},
			]
		},
	]
}

fn dialect_sql_type_from_v(typ int) !string {
	return if typ == orm.type_idx['int'] { 'INT' } else { 'TEXT' }
}

fn gen(dialect orm.SQLDialect, default_arg string) !string {
	table := orm.Table{
		name: 'test_table'
	}
	fields := dialect_fields(default_arg)
	return orm.orm_table_gen(dialect, table, '`', true, 0, fields, dialect_sql_type_from_v, false)
}

fn test_quotes_are_doubled_on_every_dialect() {
	for dialect in [orm.SQLDialect.default, .mysql, .pg, .sqlite, .h2] {
		query := gen(dialect, r"`o'brien`")!
		assert query.contains("DEFAULT 'o''brien'"), '${dialect}: ${query}'
	}
}

fn test_backslashes_are_kept_verbatim_on_standard_dialects() {
	for dialect in [orm.SQLDialect.default, .pg, .sqlite, .h2] {
		query := gen(dialect, r'`C:\tmp\new`')!
		assert query.contains(r"DEFAULT 'C:\tmp\new'"), '${dialect}: ${query}'
	}
}

fn test_backslash_in_a_mysql_literal_default_is_rejected() {
	gen(.mysql, r'`C:\tmp\new`') or {
		assert err.msg().contains('backslash')
		assert err.msg().contains('NO_BACKSLASH_ESCAPES')
		return
	}
	assert false, 'a backslash in a MySQL literal default should be rejected'
}
