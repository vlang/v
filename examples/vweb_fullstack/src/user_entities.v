module main

@[table: 'users']
pub struct User {
mut:
	id       int    @[primary; sql: serial]
	username string @[sql_type: 'TEXT'; unique]
	password string @[sql_type: 'TEXT']
	active   bool
	products []Product @[fkey: 'user_id']
}
