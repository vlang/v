module main

[table: 'products']
struct Product {
	id         int    [primary; sql: serial]
	user_id    int
	name       string [nonull; sql_type: 'TEXT']
	created_at string [default: 'CURRENT_TIMESTAMP']
}
