// vtest build: !windows
module main

import db.pg
import db.sqlite

fn test_pg_connection_user_aliases() {
	assert pg.Config{
		user: 'alice'
	}.connection_user()! == 'alice'
	assert pg.Config{
		username: 'alice'
	}.connection_user()! == 'alice'
	assert pg.Config{
		user:     'alice'
		username: 'alice'
	}.connection_user()! == 'alice'
	if _ := pg.Config{
		user:     'alice'
		username: 'bob'
	}.connection_user()
	{
		assert false
	} else {
		assert err.msg().contains('must match')
	}
}

fn test_pg_row_value_helpers() {
	mut vals := []?string{}
	vals << none
	vals << 'hello'
	vals << ''
	row := pg.Row{
		vals: vals
	}
	assert row.val(0) == ''
	assert row.val(1) == 'hello'
	assert row.values() == ['', 'hello', '']
	assert row.val_opt(0) == none
	assert row.val_opt(1) or { '' } == 'hello'
}

fn test_sqlite_row_value_helpers() {
	sqlite_row := sqlite.Row{
		vals:  ['hello', '']
		names: ['first', 'second']
	}
	assert sqlite_row.val(0) == 'hello'
	assert sqlite_row.values() == ['hello', '']
}

fn test_pg_result_empty_rows_synthetic() {
	// Construct a Result directly with column names but no rows.
	// This simulates what exec_result() should return for queries like
	// "SELECT 1 AS id WHERE false" and verifies the downstream
	// contract of Result with zero tuples.
	res := pg.Result{
		cols:  {
			'id': 0
		}
		names: ['id']
		rows:  []
	}

	assert res.names.len == 1
	assert res.names[0] == 'id'
	assert res.cols == {
		'id': 0
	}
	assert res.rows.len == 0
}

fn test_pg_result_multi_col_empty_rows() {
	// Multiple columns, zero rows — the fix scenario with more than one column
	res := pg.Result{
		cols:  {
			'id':   0
			'name': 1
		}
		names: ['id', 'name']
		rows:  []
	}

	assert res.names.len == 2
	assert res.names[0] == 'id'
	assert res.names[1] == 'name'
	assert res.cols == {
		'id':   0
		'name': 1
	}
	assert res.rows.len == 0

	// Verify column lookup via cols map
	assert res.cols['id'] == 0
	assert res.cols['name'] == 1
}

fn test_pg_result_no_cols_no_rows() {
	// Zero columns, zero rows — edge case for DDL / command results
	res := pg.Result{
		cols:  map[string]int{}
		names: []string{}
		rows:  []
	}

	assert res.names.len == 0
	assert res.cols.len == 0
	assert res.rows.len == 0
}

fn test_pg_result_as_structs_empty_rows() {
	// as_structs must return an empty slice when there are no rows,
	// even when column metadata is present.
	res := pg.Result{
		cols:  {
			'id': 0
		}
		names: ['id']
		rows:  []
	}

	structs := res.as_structs[map[string]string](fn (res pg.Result, row pg.Row) !map[string]string {
		return map[string]string{}
	})!

	assert structs.len == 0
}
