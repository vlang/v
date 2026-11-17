// vtest build: started_postgres?
module main

import db.pg

fn connect() !&pg.DB {
	return pg.connect(pg.Config{ user: 'postgres', password: '12345678', dbname: 'postgres' })
}

fn test_a_refused_statement_reports_its_sqlstate_and_constraint() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working postgres server running on localhost.')
		return
	}
	mut db := connect()!
	defer {
		db.close() or {}
	}
	db.exec('drop table if exists pg_error_fields')!
	db.exec('create table pg_error_fields (id int constraint pg_error_fields_pk primary key, n int not null)')!
	defer {
		db.exec('drop table if exists pg_error_fields') or {}
	}
	db.exec('insert into pg_error_fields values (1, 1)')!

	mut seen := 0
	db.exec_param_many('insert into pg_error_fields values ($1, 1)', ['1']) or {
		seen++
		assert err.msg().starts_with('pg exec_param_many error:\nERROR:  duplicate key value')
		if err is pg.PgError {
			assert err.sqlstate == '23505'
			assert err.severity == 'ERROR'
			assert err.constraint == 'pg_error_fields_pk'
			assert err.table == 'pg_error_fields'
			assert err.schema == 'public'
			assert err.primary.starts_with('duplicate key value')
			assert err.detail.contains('(id)=(1)')
		} else {
			assert false, 'not a pg.PgError: ${err}'
		}
	}
	db.exec('insert into pg_error_fields values (2, null)') or {
		seen++
		if err is pg.PgError {
			assert err.sqlstate == '23502'
			assert err.column == 'n'
			assert err.constraint == ''
		} else {
			assert false, 'not a pg.PgError: ${err}'
		}
	}
	db.exec_result("do $$ begin raise exception 'refused' using errcode = 'AB001', hint = 'ask'; end $$") or {
		seen++
		if err is pg.PgError {
			assert err.sqlstate == 'AB001'
			assert err.primary == 'refused'
			assert err.hint == 'ask'
		} else {
			assert false, 'not a pg.PgError: ${err}'
		}
	}
	assert seen == 3
	// The connection is still usable, and in a transaction too.
	mut tx := db.begin()!
	tx.exec('select 1/0') or {
		seen++
		if err is pg.PgError {
			assert err.sqlstate == '22012'
		} else {
			assert false, 'not a pg.PgError: ${err}'
		}
	}
	tx.rollback()!
	assert seen == 4
	assert db.q_int('select count(*) from pg_error_fields')! == 1
}
