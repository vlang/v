// vtest build: present_sqlite3? && !sanitize-memory-clang
import db.sqlite

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
