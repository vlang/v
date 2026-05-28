fn get_opt(a int) ?string {
	if a < 0 {
		return none
	}
	return 'success'
}

fn get_opt_int(a int) ?int {
	if a < 0 {
		return none
	}
	return 12
}

struct TestResult {
	cols map[string]int
	rows []TestRow
}

struct TestRow {
	vals []?string
}

struct TestDb {}

struct TestPgConnection {
	db TestDb
}

fn (db TestDb) exec_result(_ string) !TestResult {
	return TestResult{
		cols: {
			'present': 0
			'empty':   1
			'none':    2
		}
		rows: [TestRow{
			vals: [?string('hello'), ?string(''), ?string(none)]
		}]
	}
}

fn (c &TestPgConnection) query(q string) ![]map[string]string {
	res := c.db.exec_result(q)!
	mut rows := []map[string]string{}

	for row in res.rows {
		mut values := map[string]string{}
		for col, idx in res.cols {
			values[col] = row.vals[idx] or { '' } as string
		}
		rows << values
	}
	return rows
}

fn test_option_unwrap_as_cast() {
	x := get_opt(1)
	d := get_opt_int(12)
	dump(d? as int == 12)
	dump('${x? as string}' == 'success')

	assert d? as int == 12
	assert x? as string == 'success'
}

fn test_option_array_index_or_block_followed_by_as_cast() {
	conn := &TestPgConnection{
		db: TestDb{}
	}
	rows := conn.query('select') or { panic(err) }

	assert rows.len == 1
	assert rows[0]['present'] == 'hello'
	assert rows[0]['empty'] == ''
	assert rows[0]['none'] == ''
}
