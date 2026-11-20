// vtest build: started_mssql?
module main

import db.mssql
import os

struct MsgUser {
	id   int @[primary; sql: serial]
	name string
	age  int
}

// orm_shape_check is never called; it makes the compiler type-check the full
// ORM surface against db.mssql.Connection even without a live SQL Server.
fn orm_shape_check(mut conn mssql.Connection) {
	sql conn {
		create table MsgUser
	}!
	mut u := MsgUser{
		name: 'alice'
		age:  30
	}
	sql conn {
		insert u into MsgUser
	}!
	rows := sql conn {
		select from MsgUser where name == 'alice' limit 1
	}!
	_ = rows
	sql conn {
		update MsgUser set age = 31 where name == 'alice'
	}!
	sql conn {
		delete from MsgUser where name == 'alice'
	}!
	sql conn {
		drop table MsgUser
	}!
}

fn test_mssql_orm_crud() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		return
	}
	conn_str := os.getenv_opt('VMSSQL_CONN_STR') or {
		'Driver={FreeTDS};Server=127.0.0.1;Port=1433;UID=sa;PWD=Vlang12345678!;Database=master;TDS_Version=7.4;ClientCharset=UTF-8'
	}
	mut conn := mssql.open(conn_str)!
	defer {
		conn.close()
	}

	sql conn {
		drop table MsgUser
	} or {}

	sql conn {
		create table MsgUser
	}!
	defer {
		sql conn {
			drop table MsgUser
		} or {}
	}

	mut u := MsgUser{
		name: 'alice'
		age:  30
	}
	inserted_id := sql conn {
		insert u into MsgUser
	}!
	assert inserted_id > 0

	rows := sql conn {
		select from MsgUser where name == 'alice' limit 1
	}!
	assert rows.len == 1
	assert rows[0].name == 'alice'
	assert rows[0].age == 30

	sql conn {
		update MsgUser set age = 31 where name == 'alice'
	}!
	updated := sql conn {
		select from MsgUser where name == 'alice'
	}!
	assert updated[0].age == 31
}

fn test_mssql_orm_pagination_needs_order() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		return
	}
	conn_str := os.getenv_opt('VMSSQL_CONN_STR') or {
		'Driver={FreeTDS};Server=127.0.0.1;Port=1433;UID=sa;PWD=Vlang12345678!;Database=master;TDS_Version=7.4;ClientCharset=UTF-8'
	}
	mut conn := mssql.open(conn_str)!
	defer {
		conn.close()
	}
	sql conn {
		drop table MsgUser
	} or {}
	sql conn {
		create table MsgUser
	}!
	defer {
		sql conn {
			drop table MsgUser
		} or {}
	}
	for i in 0 .. 5 {
		mut u := MsgUser{
			name: 'user${i}'
			age:  i
		}
		sql conn {
			insert u into MsgUser
		}!
	}
	rows := sql conn {
		select from MsgUser order by id limit 2 offset 1
	}!
	assert rows.len == 2
	assert rows[0].name == 'user1'
	assert rows[1].name == 'user2'
}
