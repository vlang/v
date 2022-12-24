module main

[table: 'users']
pub struct User {
mut:
	id         int    [primary; sql: serial]
	username   string [unique; required; sql_type: 'TEXT']
	password   string [required; sql_type: 'TEXT']
	active     bool
}
