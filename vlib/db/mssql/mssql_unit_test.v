module mssql

// Offline regressions for the mssql driver boundaries. These tests need no
// live SQL Server: they exercise the pure batch/indicator logic against a
// mocked ODBC boundary (simulated driver writes into i64 cells).

fn test_is_insert_statement() {
	assert is_insert_statement('insert into t (a) values (1)')
	assert is_insert_statement('  INSERT INTO t (a) VALUES (1)')
	assert is_insert_statement('\n\tinsert\ninto t (a) values (1)')
	assert is_insert_statement('insert(a, b) values (1, 2)')
	assert is_insert_statement('insert;')
	assert !is_insert_statement('select 1')
	assert !is_insert_statement('update t set a = 1')
	assert !is_insert_statement('delete from t')
	assert !is_insert_statement('')
	assert !is_insert_statement('ins')
	assert !is_insert_statement('inserted into t')
}

fn test_identity_batch_query_runs_in_same_scope() {
	q := 'insert into t (a) values (1)'
	batch := identity_batch_query(q)
	assert batch == q + '; SELECT SCOPE_IDENTITY()'
	assert batch.starts_with(q)
	assert batch.contains('SCOPE_IDENTITY()')
	// A separate `SELECT SCOPE_IDENTITY()` query would run in a new batch
	// (scope) and return NULL; the batch form keeps one scope.
	assert batch != 'SELECT SCOPE_IDENTITY()'
}

fn test_last_id_returns_retained_identity() {
	mut conn := Connection{
		last_insert_id: 7
	}
	assert conn.last_id() == 7
	conn.last_insert_id = 0
	assert conn.last_id() == 0
	fresh := Connection{}
	assert fresh.last_id() == 0
}

fn test_reserve_params_keeps_all_binds_inside() {
	mut h := HStmt{}
	h.reserve_params(17)
	assert h.param_indicators.len == 17
	mut retained := []voidptr{cap: 17}
	for i in 0 .. 17 {
		unsafe {
			retained << voidptr(&h.param_indicators[i])
		}
	}
	// Simulate the indicator writes bind_parameter performs, without the ODBC call.
	for i in 0 .. 17 {
		h.param_indicators[i] = i64(i)
	}
	for i in 0 .. 17 {
		unsafe {
			assert retained[i] == voidptr(&h.param_indicators[i])
		}
		assert h.param_indicators[i] == i64(i)
	}
}

fn test_incremental_growth_invalidates_retained_pointers() {
	// Models the old bind_parameter growth (append one cell per bind) to show
	// why preallocation is required: ODBC retains each bound address until
	// SQLExecute, but growth moves the storage.
	mut inds := []i64{}
	mut retained := []voidptr{cap: 17}
	for i in 0 .. 17 {
		if inds.len < i + 1 {
			inds << []i64{len: (i + 1) - inds.len}
		}
		unsafe {
			retained << voidptr(&inds[i])
		}
		inds[i] = i64(i)
	}
	mut mismatched := 0
	for i in 0 .. 17 {
		unsafe {
			if retained[i] != voidptr(&inds[i]) {
				mismatched++
			}
		}
	}
	assert mismatched > 0
}

fn test_is_null_indicator_matches_sqllen_width() {
	// 8-byte SQLLEN NULL write (64-bit driver).
	assert is_null_indicator(i64(-1))
	// 4-byte SQLLEN NULL write into a zeroed i64 cell (32-bit driver ABI):
	// only the low 4 bytes become 0xFF, so the cell reads back as 4294967295.
	mut cell := i64(0)
	unsafe {
		p := &u32(voidptr(&cell))
		*p = u32(0xffffffff)
	}
	assert cell == i64(4294967295)
	assert is_null_indicator(cell)
	// Ordinary lengths are not NULL on either width.
	assert !is_null_indicator(i64(0))
	assert !is_null_indicator(i64(5))
	mut cell2 := i64(0)
	unsafe {
		p2 := &u32(voidptr(&cell2))
		*p2 = u32(5)
	}
	assert !is_null_indicator(cell2)
}
