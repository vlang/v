import db.sqlite

struct Child {
	id        int @[primary; serial]
	parent_id int
}

struct PlainNoArg {
	id       int @[primary; serial; sql: 'custom_id']
	name     string
	children []Child @[fkey: 'parent_id']
}

fn test_plain_no_arg() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }

	sql db {
		create table PlainNoArg
	}!

	first := PlainNoArg{
		name: 'first'
	}
	second := PlainNoArg{
		name: 'second'
	}

	sql db {
		insert first into PlainNoArg
		insert second into PlainNoArg
	}!

	rows := sql db {
		select from PlainNoArg order by id desc
	}!

	assert rows[0].id == 2
	assert rows[0].name == 'second'
}

fn test_plain_no_arg_with_children() {
	mut db := sqlite.connect(':memory:')!

	parent := PlainNoArg{
		children: [Child{}, Child{}]
	}

	sql db {
		create table Child
		create table PlainNoArg
		insert parent into PlainNoArg
	}!

	parents := sql db {
		select from PlainNoArg limit 1
	}!

	assert parent.children.len == parents[0].children.len
}

struct SqlSerial {
	id   int @[primary; sql: serial]
	name string
}

fn test_sql_serial() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }

	sql db {
		create table SqlSerial
	}!

	first := SqlSerial{
		name: 'first'
	}
	second := SqlSerial{
		name: 'second'
	}

	sql db {
		insert first into SqlSerial
		insert second into SqlSerial
	}!

	rows := sql db {
		select from SqlSerial order by id desc
	}!

	assert rows[0].id == 2
	assert rows[0].name == 'second'
}
