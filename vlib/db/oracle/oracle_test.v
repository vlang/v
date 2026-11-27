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

fn test_limit_offset_bind_order() {
	// The recorder stands in for the OCI boundary: it captures the SQL and the
	// values that would be handed to OCIBindByPos, without a live client.
	mut conn := Connection{}
	conn.record_calls = true
	cfg := orm.SelectConfig{
		table:      orm.Table{ name: 'users' }
		fields:     ['id']
		types:      [typeof[int]().idx]
		has_limit:  true
		has_offset: true
	}
	// data.data carries [limit, offset] (10 and 20 respectively).
	data := orm.QueryData{
		data: [orm.Primitive(10), orm.Primitive(20)]
	}
	conn.select(cfg, data, orm.QueryData{})!
	assert conn.calls.len == 1
	assert conn.calls[0].query.contains('OFFSET :2 ROWS FETCH FIRST :1 ROWS ONLY')
	values := conn.calls[0].params.map(it.value)
	assert values == ['20', '10'], 'expected offset before limit, got ${values}'
}

fn test_sequence_ownership_per_table() {
	mut conn := Connection{}
	conn.record_calls = true
	serial_id := orm.TableField{ name: 'id', typ: orm.serial }
	conn.create(orm.new_table('alpha', []), [serial_id])!
	conn.create(orm.new_table('beta', []), [serial_id])!
	assert conn.sequences['alpha'] == 'alpha_id_SEQ'
	assert conn.sequences['beta'] == 'beta_id_SEQ'

	// Insert into alpha, then last_id() must read alpha's sequence even though
	// beta was the table created most recently.
	conn.calls.clear()
	conn.insert(orm.new_table('alpha', []), orm.QueryData{})!
	assert conn.last_insert_table == 'alpha'
	_ = conn.last_id()
	assert conn.calls.len == 2
	assert conn.calls[1].query == 'SELECT "alpha_id_SEQ".CURRVAL FROM DUAL', conn.calls[1].query

	// Dropping alpha must drop only alpha's sequence; beta's stays recorded.
	conn.calls.clear()
	conn.drop(orm.new_table('alpha', []))!
	assert conn.calls.any(it.query == 'DROP SEQUENCE "alpha_id_SEQ"')
	assert !conn.calls.any(it.query.contains('beta_id_SEQ'))
	assert 'alpha' !in conn.sequences
	assert conn.sequences['beta'] == 'beta_id_SEQ'
}
