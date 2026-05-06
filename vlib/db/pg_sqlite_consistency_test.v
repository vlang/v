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
