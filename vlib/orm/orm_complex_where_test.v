// vtest build: present_sqlite3? && !sanitize-memory-clang
import db.sqlite
import orm

struct ComplexWhere {
pub mut:
	id   int
	name string
	rank f32
}

fn test_create_without_id_field() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table ComplexWhere
	}!

	datas := [
		ComplexWhere{
			id:   0
			name: 'test1'
			rank: 1.5
		},
		ComplexWhere{
			id:   1
			name: 'test2'
			rank: 2.5
		},
		ComplexWhere{
			id:   2
			name: 'test3'
			rank: 3.5
		},
	]

	for data in datas {
		sql db {
			insert data into ComplexWhere
		}!
	}

	res := sql db {
		select from ComplexWhere where name == 'a' && (id > 1 || (rank > 2.5 && rank < 3.33))
	} or { assert false, err.msg() }
}

fn test_chained_where_groups_an_or_condition() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	mut qb := orm.new_query[ComplexWhere](db)
	qb.create()!
	qb.insert(ComplexWhere{
		id:   1
		name: 'keep'
		rank: 1.0
	})!
	qb.insert(ComplexWhere{
		id:   2
		name: 'drop'
		rank: 2.0
	})!

	// the second call carries a top level `OR`, so it has to be grouped before it is
	// `AND`ed to the first one, or `name = ? AND rank = ? OR rank = ?` matches `drop`
	rows :=
		qb.where('name = ?', 'keep')!.where('rank = ? || rank = ?', f32(1.0), f32(2.0))!.query()!
	assert rows.len == 1
	assert rows[0].name == 'keep'

	// a lone condition still needs no grouping
	single := qb.where('rank = ? || rank = ?', f32(1.0), f32(2.0))!.query()!
	assert single.len == 2
}
