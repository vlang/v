import orm
import sqlite
import v.ast
import time

struct TestCustomSqlType {
	id      int       [primary; sql: serial]
	custom  string    [sql_type: 'INTEGER']
	custom1 string    [sql_type: 'TEXT']
	custom2 string    [sql_type: 'REAL']
	custom3 string    [sql_type: 'NUMERIC']
	custom4 string
	custom5 int
	custom6 time.Time
}

struct TestDefaultAtribute {
	id          string [primary; sql: serial]
	name        string
	created_at  string [default: 'CURRENT_TIME']
	created_at1 string [default: 'CURRENT_DATE']
	created_at2 string [default: 'CURRENT_TIMESTAMP']
}

fn test_sqlite_orm() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}
	db.create('Test', [
		orm.TableField{
			name: 'id'
			typ: ast.int_type_idx
			attrs: [
				StructAttribute{
					name: 'primary'
				},
				StructAttribute{
					name: 'sql'
					has_arg: true
					kind: .plain
					arg: 'serial'
				},
			]
		},
		orm.TableField{
			name: 'name'
			typ: ast.string_type_idx
			attrs: []
		},
		orm.TableField{
			name: 'age'
			typ: ast.i64_type_idx
		},
	]) or { panic(err) }

	db.insert('Test', orm.QueryData{
		fields: ['name', 'age']
		data: [orm.string_to_primitive('Louis'), orm.i64_to_primitive(100)]
	}) or { panic(err) }

	res := db.@select(orm.SelectConfig{
		table: 'Test'
		has_where: true
		fields: ['id', 'name', 'age']
		types: [ast.int_type_idx, ast.string_type_idx, ast.i64_type_idx]
	}, orm.QueryData{}, orm.QueryData{
		fields: ['name', 'age']
		data: [orm.Primitive('Louis'), i64(100)]
		types: [ast.string_type_idx, ast.i64_type_idx]
		is_and: [true, true]
		kinds: [.eq, .eq]
	}) or { panic(err) }

	id := res[0][0]
	name := res[0][1]
	age := res[0][2]

	assert id is int
	if id is int {
		assert id == 1
	}

	assert name is string
	if name is string {
		assert name == 'Louis'
	}

	assert age is i64
	if age is i64 {
		assert age == 100
	}

	/** test orm sql type
	* - verify if all type create by attribute sql_type has created
	*/

	sql db {
		create table TestCustomSqlType
	}

	mut result_custom_sql, mut exec_custom_code := db.exec('
		pragma table_info(TestCustomSqlType);
	')

	assert exec_custom_code == 101
	mut table_info_types_results := []string{}
	information_schema_custom_sql := ['INTEGER', 'INTEGER', 'TEXT', 'REAL', 'NUMERIC', 'TEXT',
		'INTEGER', 'INTEGER']

	for data_type in result_custom_sql {
		table_info_types_results << data_type.vals[2]
	}
	assert table_info_types_results == information_schema_custom_sql

	sql db {
		drop table TestCustomSqlType
	}

	/** test default attribute
	*/

	sql db {
		create table TestDefaultAtribute
	}

	mut result_default_sql, mut code := db.exec('
			pragma table_info(TestDefaultAtribute);
		')

	assert code == 101
	mut information_schema_data_types_results := []string{}
	information_schema_default_sql := ['', '', 'CURRENT_TIME', 'CURRENT_DATE', 'CURRENT_TIMESTAMP']

	for data_type in result_default_sql {
		information_schema_data_types_results << data_type.vals[4]
	}
	assert information_schema_data_types_results == information_schema_default_sql

	test_default_atribute := TestDefaultAtribute{
		name: 'Hitalo'
	}

	sql db {
		insert test_default_atribute into TestDefaultAtribute
	}

	result_test_default_atribute := sql db {
		select from TestDefaultAtribute limit 1
	}

	assert result_test_default_atribute.name == 'Hitalo'
	assert test_default_atribute.created_at.len == 0
	assert test_default_atribute.created_at1.len == 0
	assert test_default_atribute.created_at2.len == 0
	assert result_test_default_atribute.created_at.len == 8 // HH:MM:SS
	assert result_test_default_atribute.created_at1.len == 10 // YYYY-MM-DD
	assert result_test_default_atribute.created_at2.len == 19 // YYYY-MM-DD HH:MM:SS

	sql db {
		drop table TestDefaultAtribute
	}
}
