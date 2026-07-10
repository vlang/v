// vtest build: db_mssql?
module db

import db.mssql

fn test_mssql_result_to_rows_copies_column_names() {
	result := mssql.Result{
		names: ['name', 'status']
		rows:  [
			mssql.Row{
				vals: ['alice', 'active']
			},
		]
	}

	normalized := mssql_result_to_rows(result)
	assert normalized.len == 1
	assert normalized[0].names == ['name', 'status']
	assert normalized[0].val(0) == 'alice'
	assert normalized[0].get_string('name') == 'alice'
	assert normalized[0].get_string('status') == 'active'
}
