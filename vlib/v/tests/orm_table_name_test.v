import db.sqlite

@[table: '"specific name"']
struct ORMTableSpecificName {
	dummy int
}

fn test_orm_table_name() {
	db := sqlite.connect(':memory:') or { panic(err) }
	r := sql db {
		select from ORMTableSpecificName
	} or {
		assert true
		return
	}
	assert false
}
