// vtest build: started_mysqld?
module main

import db.mysql

fn test_mysql_connection_user_aliases() {
	assert mysql.Config{
		user: 'alice'
	}.connection_user()! == 'alice'
	assert mysql.Config{
		username: 'alice'
	}.connection_user()! == 'alice'
	assert mysql.Config{
		user:     'alice'
		username: 'alice'
	}.connection_user()! == 'alice'
	if _ := mysql.Config{
		user:     'alice'
		username: 'bob'
	}.connection_user()
	{
		assert false
	} else {
		assert err.msg().contains('must match')
	}
}

fn test_mysql_row_value_helpers() {
	mysql_row := mysql.Row{
		vals: ['hello', '']
	}
	assert mysql_row.val(0) == 'hello'
	assert mysql_row.values() == ['hello', '']
}
