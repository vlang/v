// vtest build: started_postgres?
module main

import db.pg

struct Info {
	table_schema string
	relname      string
	attname      string
	typename     string
	typealign    string
	typlen       int
}

fn deref(val ?string) string {
	return val or { panic('no value') }
}

fn row_mapper(res pg.Result, row pg.Row) !Info {
	return Info{
		table_schema: deref(row.vals[res.cols['table_schema']])
		relname:      deref(row.vals[res.cols['relname']])
		attname:      deref(row.vals[res.cols['attname']])
		typename:     deref(row.vals[res.cols['typename']])
		typealign:    deref(row.vals[res.cols['typealign']])
		typlen:       deref(row.vals[res.cols['typlen']]).int()
	}
}

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

	result := db.exec_result('
SELECT ischema.table_schema, c.relname, a.attname, t.typname, t.typalign, t.typlen
  FROM pg_class c
  JOIN information_schema.tables ischema on ischema.table_name = c.relname
  JOIN pg_attribute a ON (a.attrelid = c.oid)
  JOIN pg_type t ON (t.oid = a.atttypid)
WHERE
  a.attnum >= 0
 ')!

	infos := result.as_structs(row_mapper)!

	assert result.rows.len > 0 && infos.len == result.rows.len

	// println(infos)
}
