module main

import orm
import pg
import v.ast
import time

struct TestCustomSqlType {
	id      int    [primary; sql: serial]
	custom  string [sql_type: 'TEXT']
	custom1 string [sql_type: 'VARCHAR(191)']
	custom2 string [sql_type: 'TIMESTAMP']
	custom3 string [sql_type: 'uuid']
}

struct TestCustomWrongSqlType {
	id      int    [primary; sql: serial]
	custom  string
	custom1 string [sql_type: 'VARCHAR']
	custom2 string [sql_type: 'money']
	custom3 string [sql_type: 'xml']
}

struct TestTimeType {
mut:
	id         int       [primary; sql: serial]
	username   string
	created_at time.Time [sql_type: 'TIMESTAMP']
	updated_at string    [sql_type: 'TIMESTAMP']
	deleted_at time.Time
}

struct TestDefaultAtribute {
	id         string [default: 'gen_random_uuid()'; primary; sql_type: 'uuid']
	name       string
	created_at string [default: 'CURRENT_TIMESTAMP'; sql_type: 'TIMESTAMP']
}

fn test_pg_orm() {
	mut db := pg.connect(
		host: 'localhost'
		user: 'postgres'
		password: 'password'
		dbname: 'postgres'
	) or { panic(err) }

	defer {
		db.close()
	}

	db.create('Test', [
		orm.TableField{
			name: 'id'
			typ: ast.string_type_idx
			is_time: false
			default_val: ''
			is_arr: false
			attrs: [
				StructAttribute{
					name: 'primary'
					has_arg: false
					arg: ''
					kind: .plain
				},
				StructAttribute{
					name: 'sql'
					has_arg: true
					arg: 'serial'
					kind: .plain
				},
			]
		},
		orm.TableField{
			name: 'name'
			typ: ast.string_type_idx
			is_time: false
			default_val: ''
			is_arr: false
			attrs: []
		},
		orm.TableField{
			name: 'age'
			typ: ast.i64_type_idx
			is_time: false
			default_val: ''
			is_arr: false
			attrs: []
		},
	]) or { panic(err) }

	db.insert('Test', orm.QueryData{
		fields: ['name', 'age']
		data: [orm.string_to_primitive('Louis'), orm.int_to_primitive(101)]
	}) or { panic(err) }

	res := db.@select(orm.SelectConfig{
		table: 'Test'
		is_count: false
		has_where: true
		has_order: false
		order: ''
		order_type: .asc
		has_limit: false
		primary: 'id'
		has_offset: false
		fields: ['id', 'name', 'age']
		types: [ast.int_type_idx, ast.string_type_idx, ast.i64_type_idx]
	}, orm.QueryData{}, orm.QueryData{
		fields: ['name', 'age']
		data: [orm.Primitive('Louis'), orm.Primitive(101)]
		types: []
		kinds: [.eq, .eq]
		is_and: [true]
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
		assert age == 101
	}

	/** test orm sql type
	* - verify if all type create by attribute sql_type has created
	*/

	sql db {
		create table TestCustomSqlType
	}

	mut result_custom_sql := db.exec("
		SELECT DATA_TYPE
		FROM INFORMATION_SCHEMA.COLUMNS
		WHERE TABLE_NAME = 'TestCustomSqlType'
		ORDER BY ORDINAL_POSITION
	") or {
		println(err)
		panic(err)
	}
	mut information_schema_data_types_results := []string{}
	information_schema_custom_sql := ['integer', 'text', 'character varying',
		'timestamp without time zone', 'uuid']
	for data_type in result_custom_sql {
		information_schema_data_types_results << data_type.vals[0]
	}

	sql db {
		drop table TestCustomSqlType
	}

	assert information_schema_data_types_results == information_schema_custom_sql

	/** test_orm_time_type
	* - test time.Time v type with sql_type: 'TIMESTAMP'
	* - test string v type with sql_type: 'TIMESTAMP'
	* - test time.Time v type without
	*/
	today := time.parse('2022-07-16 15:13:27') or {
		println(err)
		panic(err)
	}

	model := TestTimeType{
		username: 'hitalo'
		created_at: today
		updated_at: today.str()
		deleted_at: today
	}

	sql db {
		create table TestTimeType
	}

	sql db {
		insert model into TestTimeType
	}

	results := sql db {
		select from TestTimeType where username == 'hitalo'
	}

	sql db {
		drop table TestTimeType
	}

	assert results[0].username == model.username
	assert results[0].created_at == model.created_at
	assert results[0].updated_at == model.updated_at
	assert results[0].deleted_at == model.deleted_at

	/** test default attribute
	*/
	sql db {
		create table TestDefaultAtribute
	}

	mut result_defaults := db.exec("
		SELECT column_default
		FROM INFORMATION_SCHEMA.COLUMNS
		WHERE TABLE_NAME = 'TestDefaultAtribute'
		ORDER BY ORDINAL_POSITION
	") or {
		println(err)
		panic(err)
	}
	mut information_schema_defaults_results := []string{}

	for defaults in result_defaults {
		information_schema_defaults_results << defaults.vals[0]
	}
	sql db {
		drop table TestDefaultAtribute
	}
	assert ['gen_random_uuid()', '', 'CURRENT_TIMESTAMP'] == information_schema_defaults_results
}
