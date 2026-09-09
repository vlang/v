// vtest build: present_sqlite3?
import db.sqlite

struct Parent {
	id       int @[primary; sql: serial]
	name     string
	children []Child @[fkey: 'parent_id']
}

struct Child {
	id        int @[primary; sql: serial]
	name      string
	parent_id int
	babies    []Baby @[fkey: 'child_id']
}

struct Baby {
	id       int @[primary; sql: serial]
	name     string
	child_id int
}

fn test_main() {
	mut db := sqlite.connect(':memory:')!

	sql db {
		create table Parent
		create table Child
		create table Baby
	} or { panic(err) }

	new_parent := Parent{
		name:     'first parent'
		children: [
			Child{
				name: 'first child'
			},
			Child{
				name: 'second_child'
			},
		]
	}

	sql db {
		insert new_parent into Parent
	} or { panic(err) }

	babies := [
		Baby{
			name:     'first baby'
			child_id: 1
		},
		Baby{
			name:     'second baby'
			child_id: 1
		},
		Baby{
			name:     'third baby'
			child_id: 2
		},
		Baby{
			name:     'fourth baby'
			child_id: 2
		},
		Baby{
			name:     'fifth baby'
			child_id: 2
		},
	]

	for v in babies {
		sql db {
			insert v into Baby
		} or { panic(err) }
	}

	parent := sql db {
		select from Parent
	} or { panic(err) }

	assert parent[0].children[0].id == 1
	assert parent[0].children[1].id == 2
	assert parent[0].children.len == 2
	assert parent[0].children[0].babies.len == 2
	assert parent[0].children[1].babies.len == 3
	assert parent[0].children[0].babies[0].name == 'first baby'
	assert parent[0].children[0].babies[1].name == 'second baby'
	assert parent[0].children[1].babies[0].name == 'third baby'
	assert parent[0].children[1].babies[1].name == 'fourth baby'
	assert parent[0].children[1].babies[2].name == 'fifth baby'

	db.close()!
}
