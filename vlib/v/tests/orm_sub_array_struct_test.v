import db.sqlite

struct Parent {
	id       int     [primary; sql: serial]
	name     string
	children []Child [fkey: 'parent_id']
}

struct Child {
mut:
	id        int    [primary; sql: serial]
	parent_id int
	name      string
}

fn test_orm_array() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	sql db {
		create table Parent
	}
	sql db {
		create table Child
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

	parents := sql db {
		select from Parent where id == 1
	}

	sql db {
		drop table Parent
	}

	parent := parents.first()
	assert parent.name == par.name
	assert parent.children.len == par.children.len
	assert parent.children[0].name == 'abc'
	assert parent.children[1].name == 'def'
}

fn test_orm_relationship() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	sql db {
		create table Parent
	}
	sql db {
		create table Child
	}

	mut child := Child{
		name: 'abc'
	}

	par := Parent{
		name: 'test'
		children: []
	}

	sql db {
		insert par into Parent
	}

	mut parents := sql db {
		select from Parent where id == 1
	}

	mut parent := parents.first()
	child.parent_id = parent.id
	child.name = 'atum'

	sql db {
		insert child into Child
	}

	child.name = 'bacon'

	sql db {
		insert child into Child
	}

	assert parent.name == par.name
	assert parent.children.len == 0

	parents = sql db {
		select from Parent where id == 1
	}

	parent = parents.first()
	assert parent.name == par.name
	assert parent.children.len == 2
	assert parent.children[0].name == 'atum'
	assert parent.children[1].name == 'bacon'

	mut children := sql db {
		select from Child
	}

	assert children.len == 2

	sql db {
		drop table Parent
	}

	children = sql db {
		select from Child
	}

	assert children.len == 2
}
