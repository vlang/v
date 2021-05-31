import orm
import sqlite

fn test_sql_defaults_update() {
	query := orm.orm_stmt_gen('Test', "'", .update, true, '?', orm.OrmQueryData{
		fields: ['test', 'a']
		data: []
		types: []
		kinds: []
	}, orm.OrmQueryData{
		fields: ['id', 'name']
		data: []
		types: []
		kinds: [.ge, .eq]
	})
	assert query == "UPDATE 'Test' SET 'test' = ?0, 'a' = ?1 WHERE 'id' >= ?2 AND 'name' == ?3;"
}

fn test_sql_defaults_insert() {
	query := orm.orm_stmt_gen('Test', "'", .insert, true, '?', orm.OrmQueryData{
		fields: ['test', 'a']
		data: []
		types: []
		kinds: []
	}, orm.OrmQueryData{})
	assert query == "INSERT INTO 'Test' ('test', 'a') VALUES (?0, ?1);"
}

fn test_sql_defaults_delete() {
		query := orm.orm_stmt_gen('Test', "'", .delete, true, '?', orm.OrmQueryData{
		fields: ['test', 'a']
		data: []
		types: []
		kinds: []
	}, orm.OrmQueryData{
		fields: ['id', 'name']
		data: []
		types: []
		kinds: [.ge, .eq]
	})
		assert query == "DELETE FROM 'Test' WHERE 'id' >= ?0 AND 'name' == ?1;"

}