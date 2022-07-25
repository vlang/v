module main

import time

[table: 'usersxqa']
struct User {
mut:
	id         int       [primary; sql: serial]
	username   string    [required; sql_type: 'varchar(191)']
	password   string    [required; sql_type: 'longtext']
	name       string    [sql_type: 'varchar(191)']
	created_at time.Time [sql_type: 'datetime(3)']
	updated_at time.Time [sql_type: 'datetime(3)']
	deleted_at time.Time [sql_type: 'datetime(3)']
	active     bool
}
