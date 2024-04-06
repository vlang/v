module main

import db.pg

const query = 'SELECT ischema.table_schema, c.relname, a.attname, t.typname, t.typalign, t.typlen
  FROM pg_class c
  JOIN information_schema.tables ischema on ischema.table_name = c.relname
  JOIN pg_attribute a ON (a.attrelid = c.oid)
  JOIN pg_type t ON (t.oid = a.atttypid)
WHERE
  a.attnum >= 0'

fn test_large_exec() {
	db := pg.connect(pg.Config{ user: 'postgres', password: 'secret', dbname: 'postgres' })!
	defer {
		db.close()
	}

	rows := db.exec(query)!
	for row in rows {
		// We just need to access the memory to ensure it's properly allocated
		row.str()
	}
}
