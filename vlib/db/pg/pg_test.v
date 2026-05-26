// vtest build: started_postgres?
module main

import db.pg
import orm

fn test_large_exec() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working postgres server running on localhost.')
		return
	}

	mut db := pg.connect(pg.Config{ user: 'postgres', password: '12345678', dbname: 'postgres' })!
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
	mut db := pg.connect(pg.Config{ user: 'postgres', password: '12345678', dbname: 'postgres' })!
	defer {
		db.close() or {}
	}

	// Prepared statements are session-scoped, so pin a single conn.
	mut c := db.conn()!
	defer {
		c.close() or {}
	}
	c.prepare('test_prepared', 'SELECT NOW(), $1 AS NAME', 1) or { panic(err) }

	result := c.exec_prepared('test_prepared', ['hello world']) or { panic(err) }

	assert result.len == 1
}

fn test_transaction() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working postgres server running on localhost.')
		return
	}

	mut db := pg.connect(pg.Config{ user: 'postgres', password: '12345678', dbname: 'postgres' })!
	defer {
		db.close() or {}
	}
	db.exec('drop table if exists users')!
	db.exec('create table if not exists users (
                        id SERIAL PRIMARY KEY,
                        username TEXT,
						last_name TEXT NULL DEFAULT NULL
                      )')!

	// orm.TransactionalConnection requires a pinned conn (DB is pool-backed
	// and cannot guarantee BEGIN/COMMIT land on the same physical conn).
	mut c := db.conn()!
	mut tc := orm.TransactionalConnection(c)
	mut otx := orm.begin(mut tc)!
	otx.transaction[int](fn (mut tx orm.Tx) !int {
		return 1
	})!
	otx.commit()!
	c.close()!

	mut tx := db.begin()!
	tx.exec("insert into users (username) values ('jackson')")!
	tx.savepoint('savepoint1')!
	tx.exec("insert into users (username) values ('kitty')")!
	tx.rollback_to('savepoint1')!
	tx.exec("insert into users (username) values ('mars')")!
	tx.commit()!

	rows := db.exec('select * from users')!
	for row in rows {
		// We just need to access the memory to ensure it's properly allocated
		dump(row.str())
	}
}

fn test_listen_notify() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working postgres server running on localhost.')
		return
	}

	mut db := pg.connect(pg.Config{ user: 'postgres', password: '12345678', dbname: 'postgres' })!
	defer {
		db.close() or {}
	}

	// LISTEN/NOTIFY is session-scoped; pin a single conn for the test.
	mut c := db.conn()!
	defer {
		c.close() or {}
	}

	// Test listen
	c.listen('test_channel')!

	// Test notify with payload
	c.notify('test_channel', 'hello world')!

	// Consume input to process the notification
	c.consume_input()!

	// Get the notification
	if notification := c.get_notification() {
		assert notification.channel == 'test_channel'
		assert notification.payload == 'hello world'
		assert notification.pid > 0
	} else {
		assert false, 'Expected a notification but got none'
	}

	// Test notify without payload
	c.notify('test_channel', '')!
	c.consume_input()!

	if notification := c.get_notification() {
		assert notification.channel == 'test_channel'
		assert notification.payload == ''
	} else {
		assert false, 'Expected a notification but got none'
	}

	// Test that no more notifications are pending
	assert c.get_notification() == none

	// Test unlisten
	c.unlisten('test_channel')!

	// Test unlisten_all
	c.listen('channel1')!
	c.listen('channel2')!
	c.unlisten_all()!

	// Test socket (should return valid fd)
	socket_fd := c.socket()
	assert socket_fd >= 0
}
