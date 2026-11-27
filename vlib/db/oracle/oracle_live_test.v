// vtest build: started_oracle?
module oracle

import orm

// These tests only run when a live Oracle Database is advertised with
// `started_oracle`, but they still type-check and compile locally.

struct OracleUser {
	id   int @[primary; sql: serial]
	name string
}

fn live_config() Config {
	return Config{
		host:     'localhost'
		port:     1521
		service:  'FREE'
		user:     'system'
		password: 'oracle'
	}
}

fn test_live_exec_and_orm() {
	mut conn := open(live_config())!
	defer {
		conn.close()
	}

	res := conn.exec('SELECT 1 AS N FROM DUAL', [])!
	assert res.rows.len == 1
	assert res.rows[0].vals[0] == '1'

	conn.create(orm.new_table('v_oracle_users', []), [
		orm.TableField{ name: 'id', typ: orm.serial }
		orm.TableField{ name: 'name', typ: typeof[string]().idx },
	]) or {}

	conn.exec('INSERT INTO "v_oracle_users" ("name") VALUES (:1)', [
		Param{ value: 'vlang' },
	])!

	rows := conn.exec('SELECT "name" FROM "v_oracle_users"', [])!
	assert rows.rows.len >= 1
	assert rows.rows[rows.rows.len - 1].vals[0] == 'vlang'

	conn.exec('DELETE FROM "v_oracle_users"', [])!
	conn.drop(orm.new_table('v_oracle_users', [])) or {}
}
