import sqlite

struct Upper {
	id  int       [primary; sql: serial]
	sub SubStruct
}

struct SubStruct {
	id   int    [primary; sql: serial]
	name string
}

fn test_orm_sub_structs() {
	db := sqlite.connect(':memory:') or { panic(err) }
	sql db {
		create table Upper
	}

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
