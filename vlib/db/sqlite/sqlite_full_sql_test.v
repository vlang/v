// vtest build: present_sqlite3? && !sanitize-memory-clang
import db.sqlite
import orm

struct FullSqlItem {
	id    int @[primary; sql: serial]
	title string
}

fn setup_full_sql_items(mut db sqlite.DB) {
	sql db {
		create table FullSqlItem
	}!
	for title in ['alpha', 'beta', 'gamma'] {
		mut it := FullSqlItem{ title: title }
		sql db {
			insert it into FullSqlItem
		}!
	}
}

fn test_in_subquery_with_order_expr_and_limit() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	setup_full_sql_items(mut db)

	rows := sql db {
		select title from FullSqlItem where id in (select from FullSqlItem order by random() limit 1)
	}!
	assert rows.len == 1, 'expected 1 row, got ${rows.len}'
	assert rows[0].title in ['alpha', 'beta', 'gamma']
}

fn test_group_by_dsl() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	setup_full_sql_items(mut db)

	rows := sql db {
		select title from FullSqlItem group by title
	}!
	assert rows.len == 3, 'expected 3 groups, got ${rows.len}'
}

fn test_union_dsl() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	setup_full_sql_items(mut db)

	rows := sql db {
		select title from FullSqlItem where id == 1 union select title from FullSqlItem where id == 2
	}!
	assert rows.len == 2, 'expected 2 rows, got ${rows.len}'
}

fn test_having_dsl() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	setup_full_sql_items(mut db)

	rows := sql db {
		select title from FullSqlItem group by title having id > 0
	}!
	assert rows.len == 3, 'expected 3 groups, got ${rows.len}'
}

fn test_exists_dsl() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	setup_full_sql_items(mut db)

	// EXISTS with a nested SELECT as a predicate.
	rows := sql db {
		select title from FullSqlItem where id in (select from FullSqlItem where id == 1)
	}!
	assert rows.len == 1, 'expected 1 row, got ${rows.len}'
}

fn test_scalar_subquery_dsl() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	setup_full_sql_items(mut db)

	rows := sql db {
		select title from FullSqlItem where id = (select from FullSqlItem order by id limit 1)
	}!
	assert rows.len == 1, 'expected 1 row, got ${rows.len}'
}

fn test_cte_dsl() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	setup_full_sql_items(mut db)

	rows := sql db {
		with c as (select title from FullSqlItem) select title from c
	}!
	assert rows.len == 3, 'expected 3 rows, got ${rows.len}'
}

fn test_exists_subquery_dsl() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	setup_full_sql_items(mut db)

	rows := sql db {
		select title from FullSqlItem where exists (select from FullSqlItem where id == 1)
	}!
	assert rows.len == 3, 'expected 3 rows, got ${rows.len}'
}

fn test_derived_join_dsl() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	setup_full_sql_items(mut db)

	rows := sql db {
		select title from FullSqlItem join (select from FullSqlItem) as d on FullSqlItem.id == d.id
	}!
	assert rows.len == 3, 'expected 3 rows, got ${rows.len}'
}

fn test_derived_join_then_where() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	setup_full_sql_items(mut db)

	// A clause after the JOIN must not be dropped by the full-SQL lowering.
	rows := sql db {
		select title from FullSqlItem join (select from FullSqlItem) as d on FullSqlItem.id == d.id where title == 'beta'
	}!
	assert rows.len == 1, 'JOIN followed by WHERE should yield 1 row, got ${rows.len}'
	assert rows[0].title == 'beta'
}

fn test_group_by_having_and_union_plan() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	setup_full_sql_items(mut db)

	mut plan := orm.new_sql_query_heap('FullSqlItem')
	plan.item('title')
	plan.group(orm.sql_raw('title'))
	plan.having_expr(orm.sql_predicate('id', .gt, orm.Primitive(0)))
	cfg := orm.SelectConfig{
		table: orm.Table{ name: 'FullSqlItem' }
		types: [orm.type_idx['string']]
	}
	rows := db.select_plan(cfg, plan)!
	assert rows.len == 3, 'expected 3 groups, got ${rows.len}'
}

fn test_union_plan() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	setup_full_sql_items(mut db)

	mut right := orm.new_sql_query_heap('FullSqlItem')
	right.item('title')
	mut plan := orm.new_sql_query_heap('FullSqlItem')
	plan.item('title')
	plan.union(right, false)
	cfg := orm.SelectConfig{
		table: orm.Table{ name: 'FullSqlItem' }
		types: [orm.type_idx['string']]
	}
	rows := db.select_plan(cfg, plan)!
	assert rows.len == 3, 'expected 3 distinct rows, got ${rows.len}'
}
