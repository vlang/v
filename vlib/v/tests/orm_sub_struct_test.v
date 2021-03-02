import sqlite

struct Upper {
	id int
	sub SubStruct
}

struct SubStruct {
	id int
	name string
}

fn test_orm_sub_structs() {
	db := sqlite.connect(':memory:') or { panic(err) }
	db.exec('create table Upper (id integer primary key, sub int default 0)')
	db.exec('create table SubStruct (id integer primary key, name string default "")')

	upper_1 := Upper{
		sub: SubStruct{
			name: 'test123'
		}
	}

	sql db {
		insert upper_1 into Upper
	}

	upper_s := sql db {
		select from Upper where id == 1
	}

	assert upper_s.sub.name == upper_1.sub.name
}
