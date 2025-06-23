// vtest build: started_postgres?
module main

import db.pg

fn test_large_exec() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working postgres server running on localhost.')
		return
	}

	db := pg.connect(pg.Config{ user: 'postgres', password: '12345678', dbname: 'postgres' })!
	defer {
		db.close() or {}
	}

	assert db.validate()!

	rows := db.exec('
SELECT ischema.table_schema, c.relname, a.attname, t.typname, t.typalign, t.typlen
  FROM pg_class c
  JOIN information_schema.tables ischema on ischema.table_name = c.relname
  JOIN pg_attribute a ON (a.attrelid = c.oid)
  JOIN pg_type t ON (t.oid = a.atttypid)
WHERE
  a.attnum >= 0
 ')!
	for row in rows {
		// We just need to access the memory to ensure it's properly allocated
		row.str()
	}
}

fn test_prepared() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working postgres server running on localhost.')
		return
	}
	db := pg.connect(pg.Config{ user: 'postgres', password: '12345678', dbname: 'postgres' })!
	defer {
		db.close() or {}
	}

	db.prepare('test_prepared', 'SELECT NOW(), $1 AS NAME', 1) or { panic(err) }

	result := db.exec_prepared('test_prepared', ['hello world']) or { panic(err) }

	assert result.len == 1
}

fn test_transaction() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working postgres server running on localhost.')
		return
	}

	db := pg.connect(pg.Config{ user: 'postgres', password: '12345678', dbname: 'postgres' })!
	defer {
		db.close() or {}
	}
	db.exec('drop table if exists users')!
	db.exec('create table if not exists users (
                        id SERIAL PRIMARY KEY,
                        username TEXT,
						last_name TEXT NULL DEFAULT NULL
                      )')!
	db.begin()!
	db.exec("insert into users (username) values ('jackson')")!
	db.savepoint('savepoint1')!
	db.exec("insert into users (username) values ('kitty')")!
	db.rollback_to('savepoint1')!
	db.exec("insert into users (username) values ('mars')")!
	db.commit()!
	rows := db.exec('select * from users')!
	for row in rows {
		// We just need to access the memory to ensure it's properly allocated
		dump(row.str())
	}
}
