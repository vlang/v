// vtest build: started_mssql?
module mssql

// These tests need only the ODBC headers to compile; they run without a live
// SQL Server. Network tests live in mssql_test.v and mssql_orm_test.v.

fn test_is_insert_stmt_detects_plain_inserts() {
	assert is_insert_stmt('INSERT INTO "users" ("name") VALUES (?)')
	assert is_insert_stmt('insert into "users" ("name") values (?)')
	assert is_insert_stmt('  INSERT INTO "users" ("name") VALUES (?)  ')
	assert is_insert_stmt('INSERT INTO "t" ("a") VALUES (?), (?)')
}

fn test_is_insert_stmt_rejects_other_batches() {
	assert !is_insert_stmt('')
	assert !is_insert_stmt('SELECT SCOPE_IDENTITY()')
	assert !is_insert_stmt('select * from "users"')
	assert !is_insert_stmt('UPDATE "users" SET "name" = ?')
	assert !is_insert_stmt('DELETE FROM "users"')
	// OUTPUT batches already return rows, so exec passes them through
	// unchanged instead of appending an identity lookup.
	assert !is_insert_stmt('INSERT INTO "t" ("a") OUTPUT INSERTED."id" VALUES (?)')
	assert !is_insert_stmt('insert into "t" ("a") output deleted."id" values (?)')
}

fn test_insert_batch_query_keeps_identity_in_scope() {
	q := 'INSERT INTO "users" ("name") VALUES (?)'
	batch := insert_batch_query(q)
	assert batch == q + '; SELECT SCOPE_IDENTITY()'
	// The appended lookup takes no parameters, so the bind count is unchanged.
	assert batch.count('?') == q.count('?')
}

fn test_indicator_is_null() {
	// i64(-1) reads back as SQL_NULL_DATA on every target: a 64-bit driver
	// fills the whole cell, and a 32-bit driver's four 0xFF bytes fill the
	// low half of a -1 cell too.
	assert indicator_is_null(i64(-1))
	assert !indicator_is_null(i64(0))
	assert !indicator_is_null(i64(4))
}

fn test_indicator_is_null_32_matches_narrow_sqllen_write() {
	// A 32-bit driver writes four 0xFF bytes into a zeroed eight-byte cell
	// (little-endian); comparing the whole cell with -1 missed this NULL.
	assert indicator_is_null_32(i64(0x0000_0000_ffff_ffff))
	assert indicator_is_null_32(i64(-1))
	assert !indicator_is_null_32(i64(0))
	assert !indicator_is_null_32(i64(4))
	// High-half garbage must not leak into the verdict.
	assert !indicator_is_null_32(i64(0x0000_0001_0000_0000))
}

fn test_reserve_param_indicators_keeps_addresses_stable() {
	// ODBC retains every indicator address handed to SQLBindParameter until
	// SQLExecute, so exec preallocates all storage before the first bind.
	// This pins the invariant with more binds than the initial capacity of a
	// freshly grown array.
	mut h := HStmt{}
	n := 17
	h.reserve_param_indicators(n)
	assert h.param_indicators.len == n
	mut addrs := []voidptr{cap: n}
	for i in 0 .. n {
		addrs << voidptr(&h.param_indicators[i])
	}
	// Write through the retained addresses the way ODBC would, then confirm
	// every value landed in its own slot: storage must not have moved.
	for i in 0 .. n {
		unsafe {
			*(&i64(addrs[i])) = i64(100 + i)
		}
	}
	for i in 0 .. n {
		assert h.param_indicators[i] == i64(100 + i)
	}
}

fn test_check_savepoint_name_accepts_valid_names() {
	check_savepoint_name('sp1')!
	check_savepoint_name('_lead')!
	check_savepoint_name('A_Z09')!
	check_savepoint_name('x'.repeat(32))!
}

fn test_check_savepoint_name_rejects_bad_names() {
	for bad in ['', 'has space', 'has-dash', 'semi;colon', 'dot.name', '9lead', 'x'.repeat(33),
		'ünïcodé'] {
		mut failed := false
		check_savepoint_name(bad) or { failed = true }
		assert failed, 'expected savepoint name to be rejected: `${bad}`'
	}
}
