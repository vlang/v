// vtest retry: 3
import db.sqlite
import time

@[table: 'foos']
struct Foo {
	id         int @[primary; sql: serial]
	name       string
	created_at time.Time @[default: 'CURRENT_TIME']
	updated_at ?string   @[sql_type: 'TIMESTAMP']
	deleted_at ?time.Time
	children   []Child @[fkey: 'parent_id']
}

struct Child {
	id        int @[primary; sql: serial]
	parent_id int
	name      string
}

fn test_main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table Foo
		create table Child
	}!
	foo := Foo{
		name:       'abc'
		created_at: time.now()
		// updated_at defaults to none
		// deleted_at defaults to none
		children: [
			Child{
				name: 'abc'
			},
			Child{
				name: 'def'
			},
		]
	}

	sql db {
		insert foo into Foo
	}!

	data := sql db {
		select from Foo
	}![0]
	assert data.id == 1
	assert data.updated_at == none
	assert data.deleted_at == none
}
