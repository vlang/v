import arrays
import db.sqlite

struct MapSqlRow {
	id   int @[primary]
	name string
}

struct MapSqlKey {
	id int
}

fn test_sql_select_inside_array_map_uses_current_element() ! {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	sql db { create table MapSqlRow }!
	rows := [MapSqlRow{1, 'first'}, MapSqlRow{2, 'second'}, MapSqlRow{3, 'third'}]
	sql db { insert rows into MapSqlRow }!
	keys := [MapSqlKey{3}, MapSqlKey{1}]
	selected := arrays.flatten(keys.map(sql db {
		select from MapSqlRow where id == it.id
	}!))
	assert selected.map(it.name) == ['third', 'first']
	ids := [2, 1]
	scalar_selected := arrays.flatten(ids.map(sql db {
		select from MapSqlRow where id == it
	}!))
	assert scalar_selected.map(it.name) == ['second', 'first']

	filtered := [0, 2, 4].filter((sql db {
		select count from MapSqlRow where id == it
	}!) > 0)
	assert filtered == [2]
	assert [0, 1].any((sql db {
		select count from MapSqlRow where id == it
	}!) > 0)
	assert ![0, 4].any((sql db {
		select count from MapSqlRow where id == it
	}!) > 0)

	nested := [[3, 1], [2]].map(it.map(sql db {
		select from MapSqlRow where id == it
	}!))
	assert nested[0][0][0].name == 'third'
	assert nested[0][1][0].name == 'first'
	assert nested[1][0][0].name == 'second'

	it := 2
	after := sql db { select from MapSqlRow where id == it }!
	assert after.len == 1
	assert after[0].name == 'second'
}
