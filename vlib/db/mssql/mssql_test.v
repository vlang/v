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
	}.get_conn_str() == 'Driver={ODBC Driver 18 for SQL Server};Server=tcp:localhost;UID=sa;PWD=secret;Database=master'
	assert mssql.Config{
		driver:   'FreeTDS'
		server:   '127.0.0.1'
		port:     1433
		user:     'sa'
		password: 'secret'
		options:  {
			'ClientCharset': 'UTF-8'
			'TDS_Version':   '7.4'
		}
	}.get_conn_str() == 'Driver=FreeTDS;Server=127.0.0.1;Port=1433;UID=sa;PWD=secret;ClientCharset=UTF-8;TDS_Version=7.4'
	assert mssql.Config{
		dsn:      'Reporting DB'
		user:     'sa'
		password: 'secret'
		dbname:   'master'
		options:  {
			'Encrypt':                'yes'
			'TrustServerCertificate': 'yes'
		}
	}.get_conn_str() == 'DSN={Reporting DB};UID=sa;PWD=secret;Database=master;Encrypt=yes;TrustServerCertificate=yes'
	assert mssql.Config{
		conn_str: 'DSN=Accounting;Trusted_Connection=Yes'
		driver:   'ignored'
	}.get_conn_str() == 'DSN=Accounting;Trusted_Connection=Yes'
}

fn test_row_helpers() {
	row := mssql.Row{
		vals: ['1', 'alice']
	}
	assert row.val(0) == '1'
	assert row.val(1) == 'alice'
	assert row.values() == ['1', 'alice']
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

	conn.query("if object_id('vlang_mssql_test', 'U') is not null drop table vlang_mssql_test") or {}
	defer {
		conn.query("if object_id('vlang_mssql_test', 'U') is not null drop table vlang_mssql_test") or {}
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
			// SQL Server can accept a connection before it is ready to execute statements.
			if _ := conn.query('select 1') {
				return conn
			} else {
				last_err = err.msg()
				conn.close()
			}
		} else {
			last_err = err.msg()
		}
		time.sleep(2 * time.second)
	}
	return error(last_err)
}
