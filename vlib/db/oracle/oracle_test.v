module oracle

import orm

fn test_config_dsn_string() {
	cfg := Config{
		host:    'db.example.com'
		port:    1522
		service: 'ORCLPDB1'
	}
	assert cfg.dsn_string() == '//db.example.com:1522/ORCLPDB1'

	default_port := Config{
		host: 'db.example.com'
		sid:  'ORCL'
	}
	assert default_port.dsn_string() == '//db.example.com:1521/ORCL'

	alias := Config{
		dsn: 'MY_TNS_ALIAS'
	}
	assert alias.dsn_string() == 'MY_TNS_ALIAS'

	empty := Config{}
	assert empty.dsn_string() == ''
}

fn test_select_limit_rewrite() {
	cfg := orm.SelectConfig{
		table:      orm.Table{ name: 'users' }
		fields:     ['id']
		types:      [typeof[int]().idx]
		has_limit:  true
		has_offset: true
	}
	q := oracle_select_query(cfg, orm.QueryData{})
	assert q.contains('OFFSET :2 ROWS FETCH FIRST :1 ROWS ONLY'), q

	no_offset := orm.SelectConfig{
		table:     orm.Table{ name: 'users' }
		fields:    ['id']
		types:     [typeof[int]().idx]
		has_limit: true
	}
	q2 := oracle_select_query(no_offset, orm.QueryData{})
	assert q2.contains('FETCH FIRST :1 ROWS ONLY'), q2
	assert !q2.contains('LIMIT'), q2
}

fn test_savepoint_name_validation() {
	assert valid_savepoint('sp_1')
	assert !valid_savepoint('')
	assert !valid_savepoint('bad name')
	assert !valid_savepoint('DROP TABLE users')
}

fn test_closed_connection_reports_error() {
	mut conn := Connection{}
	_ = conn.exec('SELECT 1 FROM DUAL', []) or {
		assert err.msg().contains('not open')
		return
	}
	assert false, 'closed connection should fail'
}

fn test_orm_interface_conformance() {
	mut conn := Connection{}
	// These casts are compile-time conformance checks: Connection must satisfy
	// both ORM connection interfaces without any vlib/orm edits.
	_ = orm.Connection(conn)
	_ = orm.TransactionalConnection(conn)
}
