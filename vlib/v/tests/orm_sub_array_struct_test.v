import sqlite

struct Parent {
	id       int     [primary; sql: serial]
	name     string
	children []Child [fkey: 'parent_id']
}

struct Child {
	id        int    [primary; sql: serial]
	parent_id int
	name      string
}

fn test_orm_array() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	sql db {
		create table Parent
	}

	par := Parent{
		name: 'test'
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
		insert par into Parent
	}

	parent := sql db {
		select from Parent where id == 1
	}

	sql db {
		drop table Parent
	}

	assert parent.name == par.name
	assert parent.children.len == par.children.len
	assert parent.children[0].name == 'abc'
	assert parent.children[1].name == 'def'
}
