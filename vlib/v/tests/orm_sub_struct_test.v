// vtest build: present_sqlite3?
import db.sqlite

struct Upper {
	id  int @[primary; sql: serial]
	sub SubStruct
}

struct SubStruct {
	id   int @[primary; sql: serial]
	name string
}

fn test_orm_sub_structs() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	sql db {
		create table Upper
	}!
	sql db {
		create table SubStruct
	}!

	upper_1 := Upper{
		sub: SubStruct{
			name: 'test123'
		}
	}

	sql db {
		insert upper_1 into Upper
	}!

	uppers := sql db {
		select from Upper where id == 1
	}!

	assert uppers.first().sub.name == upper_1.sub.name
	db.close()!
}
