// vtest build: db_mysql?
module db

import db.mysql

fn test_mysql_rows_to_rows_copies_column_names() {
	result := mysql.RowSet{
		names: ['name', 'status']
		rows:  [
			mysql.Row{
				vals: ['alice', 'active']
			},
		]
	}

	normalized := mysql_row_set_to_rows(result)
	assert normalized.len == 1
	assert normalized[0].names == ['name', 'status']
	assert normalized[0].val(0) == 'alice'
	assert normalized[0].get_string('name') == 'alice'
	assert normalized[0].get_string('status') == 'active'
}
