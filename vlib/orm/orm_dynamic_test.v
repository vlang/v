// vtest retry: 3
import db.sqlite

@[table: 'dynamic_members']
struct DynamicMember {
mut:
	id     int @[primary; sql: serial]
	name   string
	email  string
	age    int
	status string
}

fn test_dynamic_select_with_inline_where_block() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table DynamicMember
	}!

	first := DynamicMember{
		name:   'Alice'
		email:  'alice@example.com'
		age:    31
		status: 'active'
	}
	second := DynamicMember{
		name:   'Bob'
		email:  'bob@example.com'
		age:    24
		status: 'pending'
	}
	third := DynamicMember{
		name:   'Alice'
		email:  'alice-two@example.com'
		age:    19
		status: 'inactive'
	}

	sql db {
		insert first into DynamicMember
		insert second into DynamicMember
		insert third into DynamicMember
	}!

	name_filter := 'Alice'
	min_age := 30
	status_filter := ''

	rows := sql db {
		dynamic select from DynamicMember where {
				if name_filter != '' {
						name == name_filter
				},
				if min_age > 0 {
						age >= min_age
				},
				if status_filter != '' {
						status == status_filter
				}
		} order by id
	}!

	assert rows.len == 1
	assert rows[0].name == 'Alice'
	assert rows[0].email == 'alice@example.com'
	assert rows[0].age == 31
}

fn test_dynamic_update_with_alias_set_block() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table DynamicMember
	}!

	member := DynamicMember{
		name:   'Alice'
		email:  'alice@example.com'
		age:    31
		status: 'active'
	}

	sql db {
		insert member into DynamicMember
	}!

	id := db.last_id()
	next_name := 'Alicia'
	next_email := ''
	next_status := 'inactive'
	update_expr := {
				if next_name != '' {
						name == next_name
				},
				if next_email != '' {
						email == next_email
				},
				status == next_status
		}

	sql db {
		dynamic update DynamicMember set update_expr where id == id
	}!

	rows := sql db {
		select from DynamicMember where id == id
	}!

	assert rows.len == 1
	assert rows[0].name == next_name
	assert rows[0].email == 'alice@example.com'
	assert rows[0].status == next_status
}
