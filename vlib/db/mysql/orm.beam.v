// BEAM Backend: MySQL ORM methods require the mysql-otp Erlang driver.
// BEAM: requires mysql-otp Erlang driver. See vbeam docs for setup.
//
// These stubs implement the orm.Connection interface so that V code using
// `sql db { ... }` syntax compiles on BEAM. All operations return errors
// directing users to the Erlang driver for real database operations.
module mysql

import orm

// select is used internally by V's ORM for processing `SELECT` queries.
// BEAM: requires mysql-otp driver. Use mysql:query/2 with raw SQL via Erlang interop.
pub fn (db DB) select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	return error('BEAM: MySQL ORM requires mysql-otp Erlang driver. Use mysql:query/2 via Erlang interop.')
}

// insert is used internally by V's ORM for processing `INSERT` queries.
// BEAM: requires mysql-otp driver.
pub fn (db DB) insert(table orm.Table, data orm.QueryData) ! {
	return error('BEAM: MySQL ORM requires mysql-otp Erlang driver.')
}

// update is used internally by V's ORM for processing `UPDATE` queries.
// BEAM: requires mysql-otp driver.
pub fn (db DB) update(table orm.Table, data orm.QueryData, where orm.QueryData) ! {
	return error('BEAM: MySQL ORM requires mysql-otp Erlang driver.')
}

// delete is used internally by V's ORM for processing `DELETE` queries.
// BEAM: requires mysql-otp driver.
pub fn (db DB) delete(table orm.Table, where orm.QueryData) ! {
	return error('BEAM: MySQL ORM requires mysql-otp Erlang driver.')
}

// last_id is used internally by V's ORM for post-processing `INSERT` queries.
// BEAM: requires mysql-otp driver. Returns 0 (no connection).
pub fn (db DB) last_id() int {
	return 0
}

// create is used internally by V's ORM for processing table creation queries (DDL).
// BEAM: requires mysql-otp driver.
pub fn (db DB) create(table orm.Table, fields []orm.TableField) ! {
	return error('BEAM: MySQL ORM requires mysql-otp Erlang driver.')
}

// drop is used internally by V's ORM for processing table destroying queries (DDL).
// BEAM: requires mysql-otp driver.
pub fn (db DB) drop(table orm.Table) ! {
	return error('BEAM: MySQL ORM requires mysql-otp Erlang driver.')
}

// mysql_stmt_worker executes a query without returning the result.
// BEAM: requires mysql-otp driver.
fn mysql_stmt_worker(db DB, query string, data orm.QueryData, where orm.QueryData) ! {
	return error('BEAM: MySQL requires mysql-otp Erlang driver.')
}
