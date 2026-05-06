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

@[table: 'dynamic_cast_members']
struct DynamicCastMember {
mut:
	id          int @[primary; sql: serial]
	name        string
	is_required u8
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

fn test_dynamic_update_with_alias_set_block_cast_expr() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table DynamicCastMember
	}!

	member := DynamicCastMember{
		name:        'Alice'
		is_required: 0
	}

	sql db {
		insert member into DynamicCastMember
	}!

	id := db.last_id()
	next_name := 'Alicia'
	next_required := true
	update_expr := {
		name == next_name,
		is_required == u8(if next_required { 1 } else { 0 })
	}

	sql db {
		dynamic update DynamicCastMember set update_expr where id == id
	}!

	rows := sql db {
		select from DynamicCastMember where id == id
	}!

	assert rows.len == 1
	assert rows[0].name == next_name
	assert rows[0].is_required == 1
}

fn test_dynamic_select_with_in_operator_and_additional_condition() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table DynamicMember
	}!

	members := [
		DynamicMember{
			name:   'Alice'
			email:  'alice@example.com'
			age:    31
			status: 'active'
		},
		DynamicMember{
			name:   'Bob'
			email:  'bob@example.com'
			age:    24
			status: 'pending'
		},
		DynamicMember{
			name:   'Charlie'
			email:  'charlie@example.com'
			age:    29
			status: 'active'
		},
		DynamicMember{
			name:   'Diana'
			email:  'diana@example.com'
			age:    35
			status: 'inactive'
		},
		DynamicMember{
			name:   'Eve'
			email:  'eve@example.com'
			age:    22
			status: 'pending'
		},
	]

	for member in members {
		sql db {
			insert member into DynamicMember
		}!
	}

	valid_names := ['Alice', 'Charlie', 'Eve']
	min_age := 25

	rows := sql db {
		dynamic select from DynamicMember where {
		if valid_names.len > 0 {
			name in valid_names
		},
		if min_age > 0 {
			age >= min_age
		}
	} order by id
	}!

	assert rows.len == 2
	assert rows[0].name == 'Alice'
	assert rows[0].age == 31
	assert rows[1].name == 'Charlie'
	assert rows[1].age == 29
}

fn test_dynamic_select_with_explicit_order_by_asc() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table DynamicMember
	}!

	members := [
		DynamicMember{
			name:   'Alice'
			email:  'alice@example.com'
			age:    31
			status: 'active'
		},
		DynamicMember{
			name:   'Bob'
			email:  'bob@example.com'
			age:    19
			status: 'pending'
		},
		DynamicMember{
			name:   'Charlie'
			email:  'charlie@example.com'
			age:    44
			status: 'inactive'
		},
	]

	for member in members {
		sql db {
			insert member into DynamicMember
		}!
	}

	min_age := 19
	// vfmt off
	rows := sql db {
		dynamic select from DynamicMember where {
				if min_age > 0 {
						age >= min_age
				}
		} order by age asc limit 2
	}!
	// vfmt on

	assert rows.len == 2
	assert rows[0].name == 'Bob'
	assert rows[0].age == 19
	assert rows[1].name == 'Alice'
	assert rows[1].age == 31
}
