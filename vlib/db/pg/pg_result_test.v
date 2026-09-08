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

	mut db := pg.connect(pg.Config{ user: 'postgres', password: '12345678', dbname: 'postgres' })!
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

fn test_empty_result_set_returns_col_names() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working postgres server running on localhost.')
		return
	}

	mut db := pg.connect(pg.Config{
		user:     'postgres'
		password: '12345678'
		dbname:   'postgres'
	})!
	defer {
		db.close() or {}
	}

	// Query that returns column metadata but zero tuples
	result := db.exec_result('SELECT 1 AS id WHERE false')!

	assert result.names.len == 1
	assert result.names[0] == 'id'
	assert result.cols == {
		'id': 0
	}
	assert result.rows.len == 0
	assert result.fields == [
		pg.Field{
			name:          'id'
			type_oid:      23
			type_modifier: -1
			size:          4
			format:        0
			table_oid:     0
			table_column:  0
		},
	]
}

fn test_mixed_result_field_metadata() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working postgres server running on localhost.')
		return
	}

	mut db := pg.connect(pg.Config{ user: 'postgres', password: '12345678', dbname: 'postgres' })!
	defer {
		db.close() or {}
	}

	result := db.exec_result("SELECT
		1::int4 AS int4_value,
		2::int8 AS int8_value,
		3.14::numeric(10, 2) AS numeric_value,
		true::boolean AS bool_value,
		now()::timestamptz AS timestamptz_value,
		'hello'::text AS text_value,
		'{}'::jsonb AS jsonb_value,
		'fixed'::varchar(32) AS varchar_value")!

	assert result.fields.map(it.name) == [
		'int4_value',
		'int8_value',
		'numeric_value',
		'bool_value',
		'timestamptz_value',
		'text_value',
		'jsonb_value',
		'varchar_value',
	]
	assert result.fields.map(it.type_oid) == [u32(23), 20, 1700, 16, 1184, 25, 3802, 1043]
	assert result.fields.map(it.type_modifier) == [-1, -1, 655366, -1, -1, -1, -1, 36]
	assert result.fields.map(it.size) == [4, 8, -1, 1, 8, -1, -1, -1]
	assert result.fields.all(it.format == 0)
	assert result.fields.all(it.table_oid == 0)
	assert result.fields.all(it.table_column == 0)
	assert result.rows.len == 1
	assert result.rows[0].val(0) == '1'
	assert result.rows[0].val(1) == '2'
	assert result.rows[0].val(2) == '3.14'
	assert result.rows[0].val(3) == 't'
	assert result.rows[0].val(5) == 'hello'
	assert result.rows[0].val(6) == '{}'
	assert result.rows[0].val(7) == 'fixed'
}

fn assert_parameterized_result_fields(result pg.Result) {
	assert result.fields.map(it.name) == ['id', 'label']
	assert result.fields.map(it.type_oid) == [u32(23), 1043]
	assert result.fields.map(it.type_modifier) == [-1, 16]
	assert result.rows.len == 1
	assert result.rows[0].val(0) == '7'
	assert result.rows[0].val(1) == 'hello'
}

fn test_parameterized_result_field_metadata() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working postgres server running on localhost.')
		return
	}

	mut db := pg.connect(pg.Config{ user: 'postgres', password: '12345678', dbname: 'postgres' })!
	defer {
		db.close() or {}
	}
	query := 'SELECT $1::int4 AS id, $2::varchar(12) AS label'
	params := ['7', 'hello']

	param_result := db.exec_param_many_result(query, params)!
	assert_parameterized_result_fields(param_result)

	db.prepare('pg_field_metadata_stmt', query, params.len)!
	prepared_result := db.exec_prepared_result('pg_field_metadata_stmt', params)!
	assert_parameterized_result_fields(prepared_result)
}

fn test_result_field_table_origin() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working postgres server running on localhost.')
		return
	}

	mut db := pg.connect(pg.Config{ user: 'postgres', password: '12345678', dbname: 'postgres' })!
	defer {
		db.close() or {}
	}
	db.exec('CREATE TEMP TABLE pg_field_metadata_test (id int4, name varchar(20))')!
	result := db.exec_result('SELECT id, name FROM pg_field_metadata_test WHERE false')!
	assert result.rows.len == 0
	assert result.fields.len == 2
	assert result.fields[0].table_oid != 0
	assert result.fields[0].table_oid == result.fields[1].table_oid
	assert result.fields[0].table_column == 1
	assert result.fields[1].table_column == 2
	assert result.fields[0].type_oid == 23
	assert result.fields[1].type_oid == 1043
	assert result.fields[1].type_modifier == 24
}
