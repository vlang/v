import mysql

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

fn test_orm() {

	mut db := mysql.Connection{
		host: '127.0.0.1'
		username: 'root'
		password: 'pw'
		dbname: 'v'
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

	information_schema_custom_sql := [{
		'DATA_TYPE':   'bigint'
		'COLUMN_TYPE': 'bigint unsigned'
	}, {
		'DATA_TYPE':   'text'
		'COLUMN_TYPE': 'text'
	}, {
		'DATA_TYPE':   'varchar'
		'COLUMN_TYPE': 'varchar(191)'
	}, {
		'DATA_TYPE':   'datetime'
		'COLUMN_TYPE': 'datetime(3)'
	}, {
		'DATA_TYPE':   'mediumint'
		'COLUMN_TYPE': 'mediumint'
	}, {
		'DATA_TYPE':   'datetime'
		'COLUMN_TYPE': 'datetime'
	}, {
		'DATA_TYPE':   'datetime'
		'COLUMN_TYPE': 'datetime'
	}]

	assert result_custom_sql.maps() == information_schema_custom_sql

	sql db {
		drop table TestCustomSqlType
	}
}
