import orm
import mysql
import time

struct TestCustomSqlType {
	id      int    [primary; sql: serial]
	custom  string [sql_type: 'TEXT']
	custom1 string [sql_type: 'VARCHAR(191)']
	custom2 string [sql_type: 'datetime(3)']
	custom3 string [sql_type: 'MEDIUMINT']
	custom4 string [sql_type: 'DATETIME']
	custom5 string [sql_type: 'datetime']
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
	created_at time.Time [sql_type: 'DATETIME']
	updated_at string    [sql_type: 'DATETIME']
	deleted_at time.Time
}

fn test_mysql_orm() {
	mut mdb := mysql.Connection{
		host: 'localhost'
		port: 3306
		username: 'root'
		password: ''
		dbname: 'mysql'
	}
	mdb.connect() or { panic(err) }
	db := orm.Connection(mdb)
	db.create('Test', [
		orm.TableField{
			name: 'id'
			typ: 7
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
			typ: 20
			attrs: []
		},
		orm.TableField{
			name: 'age'
			typ: 7
		},
	]) or { panic(err) }

	db.insert('Test', orm.QueryData{
		fields: ['name', 'age']
		data: [orm.string_to_primitive('Louis'), orm.int_to_primitive(101)]
	}) or { panic(err) }

	res := db.@select(orm.SelectConfig{
		table: 'Test'
		has_where: true
		fields: ['id', 'name', 'age']
		types: [7, 20, 8]
	}, orm.QueryData{}, orm.QueryData{
		fields: ['name', 'age']
		data: [orm.Primitive('Louis'), i64(101)]
		types: [20, 8]
		is_and: [true, true]
		kinds: [.eq, .eq]
	}) or { panic(err) }

	id := res[0][0]
	name := res[0][1]
	age := res[0][2]

	mdb.close()

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
}

fn test_orm() {
	mut db := mysql.Connection{
		host: 'localhost'
		port: 3306
		username: 'root'
		password: ''
		dbname: 'mysql'
	}

	db.connect() or {
		println(err)
		panic(err)
	}

	sql db {
		create table TestCustomSqlType
	}

	mut result_custom_sql := db.query("
		SELECT DATA_TYPE, COLUMN_TYPE 
		FROM INFORMATION_SCHEMA.COLUMNS
		WHERE TABLE_NAME = 'TestCustomSqlType'
		ORDER BY ORDINAL_POSITION
	") or {
		println(err)
		panic(err)
	}

	information_schema_custom_sql := [
		{
			'DATA_TYPE':   'bigint'
			'COLUMN_TYPE': 'bigint unsigned'
		},
		{
			'DATA_TYPE':   'text'
			'COLUMN_TYPE': 'text'
		},
		{
			'DATA_TYPE':   'varchar'
			'COLUMN_TYPE': 'varchar(191)'
		},
		{
			'DATA_TYPE':   'datetime'
			'COLUMN_TYPE': 'datetime(3)'
		},
		{
			'DATA_TYPE':   'mediumint'
			'COLUMN_TYPE': 'mediumint'
		},
		{
			'DATA_TYPE':   'datetime'
			'COLUMN_TYPE': 'datetime'
		},
		{
			'DATA_TYPE':   'datetime'
			'COLUMN_TYPE': 'datetime'
		},
	]

	sql db {
		drop table TestCustomSqlType
	}
	db.close()

	assert result_custom_sql.maps() == information_schema_custom_sql
}

fn test_orm_time_type() ? {
	mut db := mysql.Connection{
		host: 'localhost'
		port: 3306
		username: 'root'
		password: ''
		dbname: 'mysql'
	}

	db.connect() or {
		println(err)
		panic(err)
	}

	today := time.parse('2022-07-16 15:13:27')?

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

	db.close()

	assert results[0].username == model.username
	assert results[0].created_at == model.created_at
	assert results[0].updated_at == model.updated_at
	assert results[0].deleted_at == model.deleted_at
}
