import orm

fn test_render_in_subquery_with_order_expr_and_limit() {
	mut inner := orm.new_sql_query('items')
	inner.items = [orm.SqlSelectItem{ field: 'id' }]
	inner.order_by = [orm.SqlOrderItem{ expr: 'RANDOM()' }]
	inner.limit = 1

	mut outer := orm.new_sql_query('items')
	outer.items = [orm.SqlSelectItem{ field: 'title' }]
	outer.where = orm.sql_in_query('id', &inner)

	sql, params := orm.render_sql_query(.sqlite, '"', false, '?', 1, outer)
	assert sql == 'SELECT "title" FROM "items" WHERE "id" IN (SELECT "id" FROM "items" ORDER BY RANDOM() LIMIT ?)', sql
	assert params.len == 1, '${params}'
	assert params[0] is int
	assert (params[0] as int) == 1
}

fn test_render_numbered_dialect_keeps_placeholder_order() {
	mut inner := orm.new_sql_query('items')
	inner.items = [orm.SqlSelectItem{ field: 'id' }]
	inner.where = orm.sql_predicate('kind', .eq, orm.Primitive('x'))
	inner.order_by = [orm.SqlOrderItem{ expr: 'RANDOM()' }]
	inner.limit = 1

	mut outer := orm.new_sql_query('items')
	outer.items = [orm.SqlSelectItem{ field: 'title' }]
	outer.where = orm.sql_and(
		orm.sql_predicate('tenant', .eq, orm.Primitive(7)),
		orm.sql_in_query('id', &inner),
	)

	sql, params := orm.render_sql_query(.pg, '"', true, '$', 1, outer)
	assert sql.contains('("tenant" = $1 AND'), sql
	assert sql.contains('"kind" = $2'), sql
	assert sql.contains('LIMIT $3'), sql
	assert params.len == 3, '${params}'
}

fn test_render_mssql_offset_fetch_bind_order() {
	mut q := orm.new_sql_query('items')
	q.items = [orm.SqlSelectItem{ field: 'id' }]
	q.limit = 5
	q.offset = 10
	sql, params := orm.render_sql_query(.vsql, '"', false, '?', 1, q)
	assert sql.ends_with('OFFSET ? ROWS FETCH FIRST ? ROWS ONLY'), sql
	assert (params[0] as int) == 10
	assert (params[1] as int) == 5
}

fn test_render_cte_union_group_by_having() {
	mut cte := orm.new_sql_query('items')
	cte.items = [orm.SqlSelectItem{ field: 'cat' }, orm.SqlSelectItem{ field: 'n' }]
	mut other := orm.new_sql_query('other')

	mut q := orm.new_sql_query('c')
	q.ctes = [orm.SqlCTE{ name: 'c', query: &cte }]
	q.items = [orm.SqlSelectItem{ field: 'cat' }]
	q.group_by = [orm.sql_raw('cat')]
	q.having = orm.sql_predicate('n', .gt, orm.Primitive(1))
	q.set_ops = [orm.SqlSetOp{ kind: .union_all, query: &other }]

	sql, params := orm.render_sql_query(.sqlite, '"', false, '?', 1, q)
	assert sql.starts_with('WITH "c" AS (SELECT "cat", "n" FROM "items") SELECT "cat" FROM "c"'), sql
	assert sql.contains('GROUP BY cat'), sql
	assert sql.contains('HAVING "n" > ?'), sql
	assert sql.contains('UNION ALL SELECT * FROM "other"'), sql
	assert params.len == 1, '${params}'
}

fn test_render_exists_scalar_derived_join() {
	mut derived := orm.new_sql_query('items')
	derived.items = [orm.SqlSelectItem{ field: 'id' }]

	mut counters := orm.new_sql_query('counters')
	mut flags := orm.new_sql_query('flags')
	mut q := orm.new_sql_query('users')
	q.items = [orm.SqlSelectItem{ expr: orm.sql_scalar(&counters) }]
	q.joins = [orm.SqlJoin{
		from: orm.SqlFromItem{ query: &derived, alias: 'd' }
		on:   orm.sql_predicate('users.id', .eq, orm.Primitive(1))
	}]
	q.where = orm.sql_exists(&flags)
	sql, _ := orm.render_sql_query(.sqlite, '"', false, '?', 1, q)
	assert sql.contains('FROM "users" INNER JOIN (SELECT "id" FROM "items") "d"'), sql
	assert sql.contains('EXISTS (SELECT * FROM "flags")'), sql
	assert sql.contains('(SELECT * FROM "counters")'), sql
}
