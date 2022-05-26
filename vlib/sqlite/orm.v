module sqlite

import orm
import time

// sql expr

pub fn (db DB) @select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ?[][]orm.Primitive {
	// 1. Create query and bind necessary data
	query := orm.orm_select_gen(config, '`', true, '?', 1, where)
	stmt := db.new_init_stmt(query)?
	mut c := 1
	sqlite_stmt_binder(stmt, where, query, mut c)?
	sqlite_stmt_binder(stmt, data, query, mut c)?

	defer {
		stmt.finalize()
	}

	mut ret := [][]orm.Primitive{}

	if config.is_count {
		// 2. Get count of returned values & add it to ret array
		step := stmt.step()
		if step !in [sqlite_row, sqlite_ok, sqlite_done] {
			return db.error_message(step, query)
		}
		count := stmt.sqlite_select_column(0, 8)?
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
			primitive := stmt.sqlite_select_column(i, typ)?
			row << primitive
		}
		ret << row
	}
	return ret
}

// sql stmt

pub fn (db DB) insert(table string, data orm.QueryData) ? {
	query := orm.orm_stmt_gen(table, '`', .insert, true, '?', 1, data, orm.QueryData{})
	sqlite_stmt_worker(db, query, data, orm.QueryData{})?
}

pub fn (db DB) update(table string, data orm.QueryData, where orm.QueryData) ? {
	query := orm.orm_stmt_gen(table, '`', .update, true, '?', 1, data, where)
	sqlite_stmt_worker(db, query, data, where)?
}

pub fn (db DB) delete(table string, where orm.QueryData) ? {
	query := orm.orm_stmt_gen(table, '`', .delete, true, '?', 1, orm.QueryData{}, where)
	sqlite_stmt_worker(db, query, orm.QueryData{}, where)?
}

pub fn (db DB) last_id() orm.Primitive {
	query := 'SELECT last_insert_rowid();'
	id := db.q_int(query)
	return orm.Primitive(id)
}

// table
pub fn (db DB) create(table string, fields []orm.TableField) ? {
	query := orm.orm_table_gen(table, '`', true, 0, fields, sqlite_type_from_v, false) or {
		return err
	}
	sqlite_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{})?
}

pub fn (db DB) drop(table string) ? {
	query := 'DROP TABLE `$table`;'
	sqlite_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{})?
}

// helper

// Executes query and bind prepared statement data directly
fn sqlite_stmt_worker(db DB, query string, data orm.QueryData, where orm.QueryData) ? {
	stmt := db.new_init_stmt(query)?
	mut c := 1
	sqlite_stmt_binder(stmt, data, query, mut c)?
	sqlite_stmt_binder(stmt, where, query, mut c)?
	stmt.orm_step(query)?
	stmt.finalize()
}

// Binds all values of d in the prepared statement
fn sqlite_stmt_binder(stmt Stmt, d orm.QueryData, query string, mut c &int) ? {
	for data in d.data {
		err := bind(stmt, c, data)

		if err != 0 {
			return stmt.db.error_message(err, query)
		}
		c++
	}
}

// Universal bind function
fn bind(stmt Stmt, c &int, data orm.Primitive) int {
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
			err = stmt.bind_int(c, int(data.unix))
		}
		orm.InfixType {
			err = bind(stmt, c, data.right)
		}
	}
	return err
}

// Selects column in result and converts it to an orm.Primitive
fn (stmt Stmt) sqlite_select_column(idx int, typ int) ?orm.Primitive {
	mut primitive := orm.Primitive(0)

	if typ in orm.nums || typ == -1 {
		primitive = stmt.get_int(idx)
	} else if typ in orm.num64 {
		primitive = stmt.get_i64(idx)
	} else if typ in orm.float {
		primitive = stmt.get_f64(idx)
	} else if typ == orm.string {
		primitive = stmt.get_text(idx).clone()
	} else if typ == orm.time {
		d := stmt.get_int(idx)
		primitive = time.unix(d)
	} else {
		return error('Unknown type $typ')
	}

	return primitive
}

// Convert type int to sql type string
fn sqlite_type_from_v(typ int) ?string {
	return if typ in orm.nums || typ < 0 || typ in orm.num64 || typ == orm.time {
		'INTEGER'
	} else if typ in orm.float {
		'REAL'
	} else if typ == orm.string {
		'TEXT'
	} else {
		error('Unknown type $typ')
	}
}
