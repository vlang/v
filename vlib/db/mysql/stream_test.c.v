// vtest build: started_mysqld?
module mysql

fn connect_for_stream_test() !DB {
	return connect(Config{
		host:     '127.0.0.1'
		port:     u32($d('mysql_test_port', 3306))
		username: 'root'
		password: $d('mysql_test_password', '12345678')
		dbname:   'mysql'
	})
}

fn create_stream_test_table(db &DB) ! {
	db.exec_none('DROP TABLE IF EXISTS mysql_stream_test')
	db.exec('CREATE TABLE mysql_stream_test (
		id INT PRIMARY KEY,
		name VARCHAR(50),
		amount DECIMAL(10, 2),
		payload BLOB
	)')!
	db.exec("INSERT INTO mysql_stream_test VALUES
		(1, 'first', 10.25, X'00FF00'),
		(2, 'second', 20.50, X''),
		(3, 'third', 30.75, X'0102'),
		(4, NULL, NULL, NULL),
		(5, '', 0.00, X'')")!
}

fn assert_stream_connection_reusable(db &DB) ! {
	result := db.query('SELECT 42')!
	assert result.rows()[0].val(0) == '42'
	unsafe { result.free() }
}

fn test_query_stream_reads_unbuffered_batches() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	mut db := connect_for_stream_test()!
	defer {
		db.close() or {}
	}
	create_stream_test_table(&db)!
	defer {
		db.exec_none('DROP TABLE IF EXISTS mysql_stream_test')
	}

	mut stream :=
		db.query_stream('SELECT id, name, amount FROM mysql_stream_test WHERE id <= 3 ORDER BY id')!
	defer {
		stream.close()
	}
	assert stream.fields().map(it.name) == ['id', 'name', 'amount']
	first := stream.next_batch(2)!
	assert first.len == 2
	assert first[0].values() == ['1', 'first', '10.25']
	assert first[1].values() == ['2', 'second', '20.50']
	second := stream.next_batch(2)!
	assert second.len == 1
	assert second[0].values() == ['3', 'third', '30.75']
	assert stream.next_batch(2)!.len == 0

	result := db.query('SELECT COUNT(*) FROM mysql_stream_test')!
	assert result.rows()[0].val(0) == '5'
	unsafe { result.free() }

	mut nullable :=
		db.query_stream('SELECT name FROM mysql_stream_test WHERE id IN (4, 5) ORDER BY id')!
	nullable_rows := nullable.next_batch(2)!
	assert nullable_rows[0].val_opt(0) == none
	assert nullable_rows[0].val(0) == ''
	assert nullable_rows[1].val_opt(0)? == ''
	assert nullable_rows[1].val(0) == ''
	nullable.close()

	mut partial := db.query_stream('SELECT id FROM mysql_stream_test ORDER BY id')!
	assert partial.next_batch(1)!.len == 1
	partial.close()
	assert_stream_connection_reusable(&db)!
}

fn test_prepare_stream_reads_unbuffered_batches() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	mut db := connect_for_stream_test()!
	defer {
		db.close() or {}
	}
	create_stream_test_table(&db)!
	defer {
		db.exec_none('DROP TABLE IF EXISTS mysql_stream_test')
	}

	mut stmt := db.prepare_stream('SELECT id, name, amount FROM mysql_stream_test
		WHERE id > ? AND id <= 3 ORDER BY id')!
	defer {
		stmt.close()
	}
	stmt.execute(['1'])!
	assert stmt.fields().map(it.name) == ['id', 'name', 'amount']
	first := stmt.next_batch(1)!
	assert first.len == 1
	assert first[0].values() == ['2', 'second', '20.50']
	second := stmt.next_batch(2)!
	assert second.len == 1
	assert second[0].values() == ['3', 'third', '30.75']
	assert stmt.next_batch(2)!.len == 0

	result := db.query('SELECT COUNT(*) FROM mysql_stream_test')!
	assert result.rows()[0].val(0) == '5'
	unsafe { result.free() }

	mut nullable := db.prepare_stream('SELECT name FROM mysql_stream_test
		WHERE id >= ? ORDER BY id')!
	nullable.execute(['4'])!
	nullable_rows := nullable.next_batch(2)!
	assert nullable_rows[0].val_opt(0) == none
	assert nullable_rows[0].val(0) == ''
	assert nullable_rows[1].val_opt(0)? == ''
	assert nullable_rows[1].val(0) == ''
	nullable.close()

	mut partial := db.prepare_stream('SELECT id FROM mysql_stream_test ORDER BY id')!
	partial.execute([])!
	assert partial.next_batch(1)!.len == 1
	partial.close()
	assert_stream_connection_reusable(&db)!
}

fn test_copied_stream_handles_share_lifecycle_state() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	mut db := connect_for_stream_test()!
	defer {
		db.close() or {}
	}
	create_stream_test_table(&db)!
	defer {
		db.exec_none('DROP TABLE IF EXISTS mysql_stream_test')
	}

	mut stream := db.query_stream('SELECT id FROM mysql_stream_test ORDER BY id')!
	mut stream_copy := stream
	assert stream.next_batch(1)![0].val(0) == '1'
	assert stream_copy.next_batch(1)![0].val(0) == '2'
	stream.close()
	stream_copy.close()
	assert stream_copy.next_batch(1)!.len == 0
	assert_stream_connection_reusable(&db)!

	mut stmt := db.prepare_stream('SELECT id FROM mysql_stream_test ORDER BY id')!
	mut stmt_copy := stmt
	stmt_copy.execute([])!
	assert stmt.fields().map(it.name) == ['id']
	assert stmt.next_batch(1)![0].val(0) == '1'
	stmt.close()
	stmt_copy.close()
	assert stmt_copy.next_batch(1)!.len == 0
	assert_stream_connection_reusable(&db)!
}

fn test_query_stream_blob_and_empty_result() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	mut db := connect_for_stream_test()!
	defer {
		db.close() or {}
	}
	create_stream_test_table(&db)!
	defer {
		db.exec_none('DROP TABLE IF EXISTS mysql_stream_test')
	}

	mut blob_stream := db.query_stream('SELECT payload FROM mysql_stream_test WHERE id = 1')!
	blob_rows := blob_stream.next_batch(1)!
	assert blob_rows.len == 1
	blob := blob_rows[0].val_opt(0)?
	assert blob.len == 3
	assert blob.bytes() == [u8(0x00), 0xff, 0x00]
	assert blob_stream.next_batch(1)!.len == 0

	mut empty := db.query_stream('SELECT id, name FROM mysql_stream_test WHERE id < 0')!
	assert empty.fields().map(it.name) == ['id', 'name']
	assert empty.next_batch(10)!.len == 0
	assert_stream_connection_reusable(&db)!
}

fn test_use_result_discards_pending_result() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	mut db := connect_for_stream_test()!
	defer {
		db.close() or {}
	}
	query := 'SELECT 1 AS id'
	mut query_guard := db.acquire_connection_guard()!
	assert C.mysql_real_query(query_guard.conn, query.str, query.len) == 0
	query_guard.release()

	db.use_result()
	assert_stream_connection_reusable(&db)!
}

fn test_query_stream_drains_multi_statement_results() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	mut db := connect(Config{
		host:     '127.0.0.1'
		port:     u32($d('mysql_test_port', 3306))
		username: 'root'
		password: $d('mysql_test_password', '12345678')
		dbname:   'mysql'
		flag:     .client_multi_statements
	})!
	defer {
		db.close() or {}
	}

	mut exhausted := db.query_stream('SELECT 1; SELECT 2')!
	assert exhausted.next_batch(10)![0].val(0) == '1'
	assert_stream_connection_reusable(&db)!

	mut partial := db.query_stream('SELECT 1 UNION ALL SELECT 2; SELECT 3')!
	assert partial.next_batch(1)![0].val(0) == '1'
	partial.close()
	assert_stream_connection_reusable(&db)!
}

fn test_prepare_stream_blob_param_empty_result_and_execute_failure() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	mut db := connect_for_stream_test()!
	defer {
		db.close() or {}
	}
	create_stream_test_table(&db)!
	defer {
		db.exec_none('DROP TABLE IF EXISTS mysql_stream_test')
	}

	binary_param := [u8(0x00), 0xff, 0x00].bytestr()
	mut insert := db.prepare_stream('INSERT INTO mysql_stream_test (id, name, payload)
		VALUES (?, ?, ?)')!
	insert.execute(['6', 'binary', binary_param])!
	insert.close()
	mut selected := db.query_stream('SELECT payload FROM mysql_stream_test WHERE id = 6')!
	selected_rows := selected.next_batch(1)!
	assert selected_rows[0].val_opt(0)?.bytes() == [u8(0x00), 0xff, 0x00]
	selected.close()

	mut empty := db.prepare_stream('SELECT id, name FROM mysql_stream_test WHERE id = ?')!
	empty.execute(['999'])!
	assert empty.fields().map(it.name) == ['id', 'name']
	assert empty.next_batch(10)!.len == 0
	assert_stream_connection_reusable(&db)!

	mut invalid := db.prepare_stream('SELECT ?')!
	mut execute_error := ''
	invalid.execute([]) or { execute_error = err.msg() }
	assert execute_error == 'db.mysql: stream statement parameter count mismatch: expected 1, got 0'
	invalid.close()
	assert_stream_connection_reusable(&db)!

	mut extra := db.prepare_stream('SELECT ?')!
	mut extra_error := ''
	extra.execute(['1', '2']) or { extra_error = err.msg() }
	assert extra_error == 'db.mysql: stream statement parameter count mismatch: expected 1, got 2'
	extra.close()
	assert_stream_connection_reusable(&db)!

	mut duplicate := db.prepare_stream('INSERT INTO mysql_stream_test (id) VALUES (?)')!
	mut server_execute_error := ''
	duplicate.execute(['1']) or { server_execute_error = err.msg() }
	assert server_execute_error != ''
	duplicate.close()
	assert_stream_connection_reusable(&db)!
}

fn test_query_stream_fetch_failure_releases_connection() {
	$if !network ? {
		eprintln('> Skipping test ${@FN}, since `-d network` is not passed.')
		eprintln('> This test requires a working mysql server running on localhost.')
		return
	}
	mut db := connect_for_stream_test()!
	defer {
		db.close() or {}
	}
	mut killer := connect_for_stream_test()!
	defer {
		killer.close() or {}
	}
	create_stream_test_table(&db)!
	defer {
		db.exec_none('DROP TABLE IF EXISTS mysql_stream_test')
	}

	id_result := db.query('SELECT CONNECTION_ID()')!
	connection_id := id_result.rows()[0].val(0)
	unsafe { id_result.free() }
	mut stream := db.query_stream("SELECT a.id, REPEAT('x', 65535)
		FROM mysql_stream_test a
		CROSS JOIN mysql_stream_test b
		CROSS JOIN mysql_stream_test c
		CROSS JOIN mysql_stream_test d")!
	assert stream.next_batch(1)!.len == 1
	assert killer.exec_none('KILL QUERY ${connection_id}') == 0
	mut fetch_failed := false
	for _ in 0 .. 625 {
		batch := stream.next_batch(1) or {
			fetch_failed = true
			break
		}
		if batch.len == 0 {
			break
		}
	}
	assert fetch_failed
	stream.close()
	assert_stream_connection_reusable(&db)!
}
