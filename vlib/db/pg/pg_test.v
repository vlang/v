module main

import db.pg

fn test_large_exec() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working postgres server running on localhost.')
		return
	}

	db := pg.connect(pg.Config{ user: 'postgres', password: 'secret', dbname: 'postgres' })!
	defer {
		db.close()
	}

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
	db := pg.connect(pg.Config{ user: 'postgres', password: 'secret', dbname: 'postgres' })!
	defer {
		db.close()
	}

	db.prepare('test_prepared', 'SELECT NOW(), $1 AS NAME', 1) or { panic(err) }

	result := db.exec_prepared('test_prepared', ['hello world']) or { panic(err) }

	assert result.len == 1
}
