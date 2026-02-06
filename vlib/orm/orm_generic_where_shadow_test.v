// vtest build: present_sqlite3? && !sanitize-memory-clang
// Tests for generic ORM with WHERE clause variable shadowing field names.
// This tests the fix for the bug where `select from T where status == status`
// (where `status` is both a field name and a local variable) was incorrectly
// optimized to `true` by the transformer, resulting in empty WHERE clauses.
import db.sqlite

pub enum TestStatus as u8 {
	pending
	active
	completed
}

pub struct GenericRecord[T] {
pub:
	id      int @[primary; sql: serial]
	status  TestStatus
	payload T @[sql: '-']
}

struct TestPayload {
	value int
}

// Test that WHERE clause works when variable name shadows field name
fn test_generic_where_variable_shadows_field() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer { db.close() or {} }

	sql db {
		create table GenericRecord[TestPayload]
	} or { panic(err) }

	// Insert records with different statuses
	r1 := GenericRecord[TestPayload]{
		status:  .pending
		payload: TestPayload{
			value: 1
		}
	}
	r2 := GenericRecord[TestPayload]{
		status:  .active
		payload: TestPayload{
			value: 2
		}
	}
	r3 := GenericRecord[TestPayload]{
		status:  .active
		payload: TestPayload{
			value: 3
		}
	}
	r4 := GenericRecord[TestPayload]{
		status:  .completed
		payload: TestPayload{
			value: 4
		}
	}

	sql db {
		insert r1 into GenericRecord[TestPayload]
		insert r2 into GenericRecord[TestPayload]
		insert r3 into GenericRecord[TestPayload]
		insert r4 into GenericRecord[TestPayload]
	} or { panic(err) }

	// Test: variable `status` shadows field `status`
	// Before the fix, this would return all rows or error because
	// `status == status` was optimized to `true`
	status := TestStatus.active
	results := sql db {
		select from GenericRecord[TestPayload] where status == status
	} or { panic(err) }

	assert results.len == 2, 'Expected 2 active records, got ${results.len}'

	// Test with different status values
	status2 := TestStatus.pending
	pending_results := sql db {
		select from GenericRecord[TestPayload] where status == status2
	} or { panic(err) }
	assert pending_results.len == 1, 'Expected 1 pending record, got ${pending_results.len}'

	status3 := TestStatus.completed
	completed_results := sql db {
		select from GenericRecord[TestPayload] where status == status3
	} or { panic(err) }
	assert completed_results.len == 1, 'Expected 1 completed record, got ${completed_results.len}'
}

// Test generic select helper function with shadowed variable
fn generic_select_by_status[T](db &sqlite.DB, status TestStatus) ![]GenericRecord[T] {
	return sql db {
		select from GenericRecord[T] where status == status
	}!
}

fn test_generic_function_where_shadow() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer { db.close() or {} }

	sql db {
		create table GenericRecord[TestPayload]
	} or { panic(err) }

	r1 := GenericRecord[TestPayload]{
		status:  .pending
		payload: TestPayload{
			value: 10
		}
	}
	r2 := GenericRecord[TestPayload]{
		status:  .active
		payload: TestPayload{
			value: 20
		}
	}

	sql db {
		insert r1 into GenericRecord[TestPayload]
		insert r2 into GenericRecord[TestPayload]
	} or { panic(err) }

	// Call generic function where parameter shadows field name
	active_records := generic_select_by_status[TestPayload](&db, .active) or { panic(err) }
	assert active_records.len == 1, 'Expected 1 active record from generic function'
	assert active_records[0].status == .active

	pending_records := generic_select_by_status[TestPayload](&db, .pending) or { panic(err) }
	assert pending_records.len == 1, 'Expected 1 pending record from generic function'
	assert pending_records[0].status == .pending
}

// Test count with shadowed variable
fn test_generic_count_where_shadow() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer { db.close() or {} }

	sql db {
		create table GenericRecord[TestPayload]
	} or { panic(err) }

	for i in 0 .. 5 {
		r := GenericRecord[TestPayload]{
			status:  if i < 3 { .active } else { .completed }
			payload: TestPayload{
				value: i
			}
		}
		sql db {
			insert r into GenericRecord[TestPayload]
		} or { panic(err) }
	}

	status := TestStatus.active
	count := sql db {
		select count from GenericRecord[TestPayload] where status == status
	} or { panic(err) }

	assert count == 3, 'Expected count 3, got ${count}'
}

// Test that table name doesn't include generic brackets
fn test_generic_table_name() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer { db.close() or {} }

	// This should create table named "GenericRecord", not "GenericRecord[TestPayload]"
	sql db {
		create table GenericRecord[TestPayload]
	} or { panic(err) }

	// Insert and select should work without SQL syntax errors
	r := GenericRecord[TestPayload]{
		status:  .pending
		payload: TestPayload{
			value: 42
		}
	}
	sql db {
		insert r into GenericRecord[TestPayload]
	} or { panic(err) }

	results := sql db {
		select from GenericRecord[TestPayload]
	} or { panic(err) }

	assert results.len == 1
}
