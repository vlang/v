// vtest build: started_mssql?
module main

import db.mssql
import os
import time

const default_mssql_conn_str = 'Driver={FreeTDS};Server=127.0.0.1;Port=1433;UID=sa;PWD=Vlang12345678!;Database=master;TDS_Version=7.4;ClientCharset=UTF-8'

fn test_config_get_conn_str() {
	assert mssql.Config{
		driver: 'ODBC Driver 18 for SQL Server'
		server: 'tcp:localhost'
		uid:    'sa'
		pwd:    'secret'
		dbname: 'master'
	}.get_conn_str() == 'Driver=ODBC Driver 18 for SQL Server;Server=tcp:localhost;UID=sa;PWD=secret;Database=master'
	assert mssql.Config{
		driver: 'FreeTDS'
		server: '127.0.0.1'
		uid:    'sa'
		pwd:    'secret'
	}.get_conn_str() == 'Driver=FreeTDS;Server=127.0.0.1;UID=sa;PWD=secret'
}

fn test_connection_and_query() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working sql server running on localhost.')
		return
	}

	mut conn := connect_with_retry()!
	defer {
		conn.close()
	}

	conn.query('drop table if exists vlang_mssql_test') or {}
	defer {
		conn.query('drop table if exists vlang_mssql_test') or {}
	}

	create_result := conn.query('create table vlang_mssql_test (
		id int identity(1,1) primary key,
		name varchar(32) not null
	)')!
	assert create_result.rows == []mssql.Row{}

	first_insert := conn.query("insert into vlang_mssql_test (name) values ('alice')")!
	assert first_insert.rows == []mssql.Row{}
	assert first_insert.num_rows_affected == 1

	second_insert := conn.query("insert into vlang_mssql_test (name) values ('bob')")!
	assert second_insert.rows == []mssql.Row{}
	assert second_insert.num_rows_affected == 1

	result := conn.query('select id, name from vlang_mssql_test order by id')!
	assert result.rows == [
		mssql.Row{
			vals: ['1', 'alice']
		},
		mssql.Row{
			vals: ['2', 'bob']
		},
	]
}

fn connect_with_retry() !mssql.Connection {
	conn_str := os.getenv_opt('VMSSQL_CONN_STR') or { default_mssql_conn_str }
	mut last_err := 'could not connect to sql server'
	for _ in 0 .. 30 {
		mut conn := mssql.Connection{}
		if _ := conn.connect(conn_str) {
			return conn
		} else {
			last_err = err.msg()
		}
		time.sleep(2 * time.second)
	}
	return error(last_err)
}
