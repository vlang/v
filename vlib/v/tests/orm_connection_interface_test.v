import sqlite
import orm

struct Foo {
	id   int    [primary; sql: serial]
	name string
}

fn test_orm_sub_structs() {
	sqlite_db := sqlite.connect(':memory:') or { panic(err) }
	db := orm.Connection(sqlite_db)
	sql db {
		create table Foo
	}

	foo := Foo{
		name: 'test'
	}

	sql db {
		insert foo into Foo
	}

	foo_s := sql db {
		select from Foo where id == 1
	}

	assert foo.name == foo.name
}
