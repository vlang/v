module main

import db.pg

@[table: 'demo']
struct Demo {
	id      int @[primary; sql: serial]
	number  f64
	number2 f32
}

fn test_float_field() {
	conn := 'host=localhost user=test password=test' // insert own connection string
	db := pg.connect_with_conninfo(conn)!
	defer {
		db.close()
	}

	sql db {
		create table Demo
	}!

	demo := Demo{0, 9.58815, 9.58815}
	sql db {
		insert demo into Demo
	}!
	rows := sql db {
		select from Demo
	}!

	assert rows[0].number == 9.58815
	assert rows[0].number == 9.58815

	sql db {
		drop table Demo
	}!

	println(rows)
}
