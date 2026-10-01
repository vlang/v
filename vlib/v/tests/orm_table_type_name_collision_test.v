import db.sqlite

@[table: 'information_schema"."tables']
pub struct Table {
	name   string @[sql: 'table_name']
	schema string @[sql: 'table_schema']
}

fn test_orm_table_type_name_collision() {
	mut db := sqlite.connect(':memory:')!
	tables := sql db {
		select from Table where schema == 'public'
	} or { []Table{} }
	assert tables.len == 0
	db.close()!
}
