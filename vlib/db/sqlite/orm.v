module sqlite

import orm
import time

// select is used internally by V's ORM for processing `SELECT ` queries
pub fn (db DB) select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	// 1. Create query and bind necessary data
	query := orm.orm_select_gen(config, '`', true, '?', 1, where)
	$if trace_sqlite ? {
		eprintln('> select query: "${query}"')
	}
	stmt := db.new_init_stmt(query)!
	defer {
		stmt.finalize()
	}
	mut c := 1
	sqlite_stmt_binder(stmt, where, query, mut c)!
	sqlite_stmt_binder(stmt, data, query, mut c)!

	mut ret := [][]orm.Primitive{}

	if config.is_count {
		// 2. Get count of returned values & add it to ret array
		step := stmt.step()
		if step !in [sqlite_row, sqlite_ok, sqlite_done] {
			return db.error_message(step, query)
		}
		count := stmt.sqlite_select_column(0, 8)!
		ret << [count]
		return ret
	}
	for {
		// 2. Parse returned values
		step := stmt.step()
		if step == sqlite_done {
			break
		}
		if step != sqlite_ok && step != sqlite_row {
			break
		}
		mut row := []orm.Primitive{}
		for i, typ in config.types {
			primitive := stmt.sqlite_select_column(i, typ)!
			row << primitive
		}
		ret << row
	}
	return ret
}

// sql stmt

// insert is used internally by V's ORM for processing `INSERT ` queries
pub fn (db DB) insert(table orm.Table, data orm.QueryData) ! {
	query, converted_data := orm.orm_stmt_gen(.sqlite, table, '`', .insert, true, '?',
		1, data, orm.QueryData{})
	sqlite_stmt_worker(db, query, converted_data, orm.QueryData{})!
}

// update is used internally by V's ORM for processing `UPDATE ` queries
pub fn (db DB) update(table orm.Table, data orm.QueryData, where orm.QueryData) ! {
	query, _ := orm.orm_stmt_gen(.sqlite, table, '`', .update, true, '?', 1, data, where)
	sqlite_stmt_worker(db, query, data, where)!
}

// delete is used internally by V's ORM for processing `DELETE ` queries
pub fn (db DB) delete(table orm.Table, where orm.QueryData) ! {
	query, _ := orm.orm_stmt_gen(.sqlite, table, '`', .delete, true, '?', 1, orm.QueryData{},
		where)
	sqlite_stmt_worker(db, query, orm.QueryData{}, where)!
}

// last_id is used internally by V's ORM for post-processing `INSERT ` queries
pub fn (db DB) last_id() int {
	query := 'SELECT last_insert_rowid();'

	return db.q_int(query) or { 0 }
}

// DDL (table creation/destroying etc)

// create is used internally by V's ORM for processing table creation queries (DDL)
pub fn (db DB) create(table orm.Table, fields []orm.TableField) ! {
	query := orm.orm_table_gen(.sqlite, table, '`', true, 0, fields, sqlite_type_from_v,
		false) or { return err }
	sqlite_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{})!
}

// drop is used internally by V's ORM for processing table destroying queries (DDL)
pub fn (db DB) drop(table orm.Table) ! {
	query := 'DROP TABLE `${table.name}`;'
	sqlite_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{})!
}

// helper

// Executes query and bind prepared statement data directly
fn sqlite_stmt_worker(db DB, query string, data orm.QueryData, where orm.QueryData) ! {
	$if trace_sqlite ? {
		eprintln('> sqlite_stmt_worker query: "${query}"')
	}
	stmt := db.new_init_stmt(query)!
	defer {
		stmt.finalize()
	}
	mut c := 1
	sqlite_stmt_binder(stmt, data, query, mut c)!
	sqlite_stmt_binder(stmt, where, query, mut c)!
	stmt.orm_step(query)!
}

// Binds all values of d in the prepared statement
fn sqlite_stmt_binder(stmt Stmt, d orm.QueryData, query string, mut c &int) ! {
	for data in d.data {
		err := bind(stmt, mut c, data)

		if err != 0 {
			return stmt.db.error_message(err, query)
		}
		c++
	}
}

// Universal bind function
fn bind(stmt Stmt, mut c &int, data orm.Primitive) int {
	mut err := 0
	match data {
		i8, i16, int, u8, u16, u32, bool {
			err = stmt.bind_int(c, int(data))
		}
		i64, u64 {
			err = stmt.bind_i64(c, i64(data))
		}
		f32, f64 {
			err = stmt.bind_f64(c, unsafe { *(&f64(&data)) })
		}
		string {
			err = stmt.bind_text(c, data)
		}
		time.Time {
			err = stmt.bind_int(c, int(data.unix()))
		}
		orm.InfixType {
			err = bind(stmt, mut c, data.right)
		}
		orm.Null {
			err = stmt.bind_null(c)
		}
		[]orm.Primitive {
			for element in data {
				tmp_err := bind(stmt, mut c, element)
				c++
				if tmp_err != 0 {
					err = tmp_err
					break
				}
			}
		}
	}
	return err
}

// Selects column in result and converts it to an orm.Primitive
fn (stmt Stmt) sqlite_select_column(idx int, typ int) !orm.Primitive {
	if typ in orm.nums || typ == -1 {
		return stmt.get_int(idx) or { return orm.Null{} }
	} else if typ in orm.num64 {
		return stmt.get_i64(idx) or { return orm.Null{} }
	} else if typ in orm.float {
		return stmt.get_f64(idx) or { return orm.Null{} }
	} else if typ == orm.type_string {
		if v := stmt.get_text(idx) {
			return v.clone()
		} else {
			return orm.Null{}
		}
	} else if typ == orm.enum_ {
		return stmt.get_i64(idx) or { return orm.Null{} }
	} else if typ == orm.time_ {
		if v := stmt.get_int(idx) {
			return time.unix(v)
		} else {
			return orm.Null{}
		}
	} else {
		return error('Unknown type ${typ}')
	}
}

// Convert type int to sql type string
fn sqlite_type_from_v(typ int) !string {
	return if typ in orm.nums || typ in orm.num64 || typ in [orm.serial, orm.time_, orm.enum_] {
		'INTEGER'
	} else if typ in orm.float {
		'REAL'
	} else if typ == orm.type_string {
		'TEXT'
	} else {
		error('Unknown type ${typ}')
	}
}
