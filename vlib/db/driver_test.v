// vtest build: !musl? && !sanitize-memory-clang && !self_msan_compiler
module main

import db

fn test_sqlite_driver_open_exec() {
	mut driver := db.open(db.DriverConfig{
		kind:   .sqlite
		dbname: ':memory:'
	})!
	defer {
		driver.close() or {}
	}

	driver.exec('create table users (id integer primary key, name text)')!
	driver.exec_param_many('insert into users (name) values (?)', ['alice'])!

	row := driver.exec_one('select id, name from users')!
	assert row.val(0) == '1'
	assert row.val(1) == 'alice'
	assert row.get_string('name') == 'alice'
	assert row.values() == ['1', 'alice']

	assert driver.validate()!
	driver.reset()!
}

fn test_optional_backends_report_compile_flag_when_disabled() {
	$if !db_pg ? {
		if _ := db.open(db.DriverConfig{
			kind: .pg
		})
		{
			assert false
		} else {
			assert err.msg().contains('-d db_pg')
		}
	}
	$if !db_mysql ? {
		if _ := db.open(db.DriverConfig{
			kind: .mysql
		})
		{
			assert false
		} else {
			assert err.msg().contains('-d db_mysql')
		}
	}
	$if !db_mssql ? {
		if _ := db.open(db.DriverConfig{
			kind: .mssql
		})
		{
			assert false
		} else {
			assert err.msg().contains('-d db_mssql')
		}
	}
}
