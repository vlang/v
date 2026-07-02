// vtest build: db_pg?
module db

import db.pg

fn test_pg_result_to_rows_copies_column_names() {
	mut vals := []?string{}
	vals << 'alice'
	vals << none
	result := pg.Result{
		cols:  {
			'name':    0
			'deleted': 1
		}
		names: ['name', 'deleted']
		rows:  [
			pg.Row{
				vals: vals
			},
		]
	}

	rows := pg_result_to_rows(result)
	assert rows.len == 1
	assert rows[0].names == ['name', 'deleted']
	assert rows[0].val(0) == 'alice'
	assert rows[0].get_string('name') == 'alice'
	assert rows[0].get_string('deleted') == ''
}
