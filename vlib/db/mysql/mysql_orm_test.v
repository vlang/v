// vtest build: started_mysqld?
import orm
import db.mysql
import time

struct TestCustomSqlType {
	id      int    @[primary; sql: serial]
	custom  string @[sql_type: 'TEXT']
	custom1 string @[sql_type: 'VARCHAR(191)']
	custom2 string @[sql_type: 'datetime(3)']
	custom3 string @[sql_type: 'MEDIUMINT']
	custom4 string @[sql_type: 'DATETIME']
	custom5 string @[sql_type: 'datetime']
}

struct TestCustomWrongSqlType {
	id      int @[primary; sql: serial]
	custom  string
	custom1 string @[sql_type: 'VARCHAR']
	custom2 string @[sql_type: 'money']
	custom3 string @[sql_type: 'xml']
}

struct TestTimeType {
mut:
	id         int @[primary; sql: serial]
	username   string
	created_at time.Time @[sql_type: 'DATETIME']
	updated_at string    @[sql_type: 'DATETIME']
	deleted_at time.Time
	null_date  ?time.Time @[sql_type: 'DATETIME']
}

struct TestDefaultAttribute {
	id         string @[primary; sql: serial]
	name       string
	created_at string @[default: 'CURRENT_TIMESTAMP'; sql_type: 'TIMESTAMP']
}

@[comment: 'This is a table comment']
struct TestCommentAttribute {
	id         string @[primary; sql: serial]
	name       string @[comment: 'real user name']
	created_at string @[default: 'CURRENT_TIMESTAMP'; sql_type: 'TIMESTAMP']
}

fn test_mysql_orm() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	mut db := mysql.connect(
		host:     '127.0.0.1'
		port:     3306
		username: 'root'
		password: '12345678'
		dbname:   'mysql'
	)!
	defer {
		db.close() or {}
	}
	table := orm.Table{
		name: 'Test'
	}
	db.drop(table) or {}
	db.create(table, [
		orm.TableField{
			name:  'id'
			typ:   typeof[int]().idx
			attrs: [
				VAttribute{
					name: 'primary'
				},
				VAttribute{
					name:    'sql'
					has_arg: true
					kind:    .plain
					arg:     'serial'
				},
			]
		},
		orm.TableField{
			name:  'name'
			typ:   typeof[string]().idx
			attrs: []
		},
		orm.TableField{
			name: 'age'
			typ:  typeof[int]().idx
		},
	]) or { panic(err) }

	db.insert(table, orm.QueryData{
		fields: ['name', 'age']
		data:   [orm.string_to_primitive('Louis'), orm.int_to_primitive(101)]
	}) or { panic(err) }

	res := db.select(orm.SelectConfig{
		table:     table
		has_where: true
		fields:    ['id', 'name', 'age']
		types:     [typeof[int]().idx, typeof[string]().idx, typeof[i64]().idx]
	}, orm.QueryData{}, orm.QueryData{
		fields: ['name', 'age']
		data:   [orm.Primitive('Louis'), i64(101)]
		types:  [typeof[string]().idx, typeof[i64]().idx]
		is_and: [true, true]
		kinds:  [.eq, .eq]
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
	}!

	mut result_custom_sql := db.query("
		SELECT DATA_TYPE, COLUMN_TYPE
		FROM INFORMATION_SCHEMA.COLUMNS
		WHERE TABLE_NAME = 'TestCustomSqlType'
		ORDER BY ORDINAL_POSITION
	") or {
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
	}!

	assert result_custom_sql.maps() == information_schema_custom_sql

	/** test_orm_time_type
	* - test time.Time v type with sql_type: 'TIMESTAMP'
	* - test string v type with sql_type: 'TIMESTAMP'
	* - test time.Time v type without
	*/
	today := time.parse('2022-07-16 15:13:27') or {
		println(err)
		panic(err)
	}

	model1 := TestTimeType{
		username:   'hitalo'
		created_at: today
		updated_at: today.str()
		deleted_at: today
		// null_date is null
	}
	model2 := TestTimeType{
		username:   'tom'
		created_at: today
		updated_at: today.str()
		deleted_at: today
		null_date:  today
	}
	model3 := TestTimeType{
		username:   'kitty'
		created_at: today
		updated_at: today.str()
		deleted_at: today
		// null_date is null
	}

	sql db {
		create table TestTimeType
	}!

	sql db {
		insert model1 into TestTimeType
		insert model2 into TestTimeType
		insert model3 into TestTimeType
	}!

	results := sql db {
		select from TestTimeType
	}!

	sql db {
		drop table TestTimeType
	}!

	assert results[0].created_at == model1.created_at
	assert results[0].username == model1.username
	assert results[0].updated_at == model1.updated_at
	assert results[0].deleted_at == model1.deleted_at
	assert results[0].null_date == none

	assert results[1].created_at == model2.created_at
	assert results[1].username == model2.username
	assert results[1].updated_at == model2.updated_at
	assert results[1].deleted_at == model2.deleted_at
	if x := results[1].null_date {
		// should not by `none`/`NULL`
		assert x == model2.deleted_at
	} else {
		assert false
	}

	assert results[2].created_at == model3.created_at
	assert results[2].username == model3.username
	assert results[2].updated_at == model3.updated_at
	assert results[2].deleted_at == model3.deleted_at
	assert results[2].null_date == none

	/** test default attribute
	*/
	sql db {
		create table TestDefaultAttribute
	}!

	mut result_defaults := db.query("
		SELECT COLUMN_DEFAULT
		FROM INFORMATION_SCHEMA.COLUMNS
		WHERE TABLE_NAME = 'TestDefaultAttribute'
		ORDER BY ORDINAL_POSITION
	") or {
		println(err)
		panic(err)
	}
	mut information_schema_defaults_results := []string{}

	sql db {
		drop table TestDefaultAttribute
	}!

	information_schema_column_default_sql := [{
		'COLUMN_DEFAULT': ''
	}, {
		'COLUMN_DEFAULT': ''
	}, {
		'COLUMN_DEFAULT': 'CURRENT_TIMESTAMP'
	}]
	assert information_schema_column_default_sql == result_defaults.maps()

	/** test comment attribute
	*/
	sql db {
		create table TestCommentAttribute
	}!

	mut column_comments := db.query("
		SELECT COLUMN_COMMENT
		FROM INFORMATION_SCHEMA.COLUMNS
		WHERE TABLE_NAME = 'TestCommentAttribute'
		ORDER BY ORDINAL_POSITION
	") or {
		println(err)
		panic(err)
	}

	mut table_comment := db.query("
		SELECT TABLE_COMMENT
		FROM INFORMATION_SCHEMA.TABLES
		WHERE TABLE_NAME = 'TestCommentAttribute'
	") or {
		println(err)
		panic(err)
	}

	sql db {
		drop table TestCommentAttribute
	}!

	information_schema_column_comment_sql := [{
		'COLUMN_COMMENT': ''
	}, {
		'COLUMN_COMMENT': 'real user name'
	}, {
		'COLUMN_COMMENT': ''
	}]
	assert information_schema_column_comment_sql == column_comments.maps()

	information_schema_table_comment_sql := [
		{
			'TABLE_COMMENT': 'This is a table comment'
		},
	]
	assert information_schema_table_comment_sql == table_comment.maps()
}
