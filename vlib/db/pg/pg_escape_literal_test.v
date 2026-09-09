// vtest build: started_postgres?
module main

import db.pg

fn test_escape_literal() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working postgres server running on localhost.')
		return
	}
	mut db := pg.connect(pg.Config{ user: 'postgres', password: '12345678', dbname: 'postgres' })!
	defer {
		db.close() or {}
	}
	mut conn := db.conn()!
	defer {
		conn.close() or {}
	}

	escaped := conn.escape_literal("O'Reilly")!
	assert escaped == "'O''Reilly'"
	rows := conn.exec('select ${escaped}')!
	assert rows[0].val(0) == "O'Reilly"
}
