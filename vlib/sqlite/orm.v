module sqlite

import orm
import time

// sql expr

pub fn (db DB) @select(config orm.OrmSelectConfig, data orm.OrmQueryData, where orm.OrmQueryData) ?[][]orm.Primitive {
	query := orm.orm_select_gen(config, '`', true, '?', 1, where)
	stmt := db.new_init_stmt(query)
	sqlite_stmt_binder(stmt, data, query) ?
	sqlite_stmt_binder(stmt, where, query) ?

	mut ret := [][]orm.Primitive{}

	if config.is_count {
		stmt.orm_step(query) ?
		ret << [orm.Primitive(stmt.get_count())]
		return ret
	}

	for {
		eprintln(ret)
		step := stmt.step()
		if step == sqlite_done {
			break
		}
		if step != sqlite_ok && step != sqlite_row {
			break
		}
		mut row := []orm.Primitive{}
		for i, typ in config.types {
			primitive := stmt.sqlite_select_column(i, typ) ?
			row << primitive
		}
		
		ret << row
		eprintln(ret)
	}

	eprintln(ret)

	return ret
}

// sql stmt

pub fn (db DB) insert(table string, data orm.OrmQueryData) ? {
	query := orm.orm_stmt_gen(table, '`', .insert, true, '?', 1, data, orm.OrmQueryData{})
	sqlite_stmt_worker(db, query, data, orm.OrmQueryData{}) ?
}

pub fn (db DB) update(table string, data orm.OrmQueryData, where orm.OrmQueryData) ? {
	query := orm.orm_stmt_gen(table, '`', .update, true, '?', 1, data, where)
	sqlite_stmt_worker(db, query, data, where) ?
}

pub fn (db DB) delete(table string, data orm.OrmQueryData, where orm.OrmQueryData) ? {
	query := orm.orm_stmt_gen(table, '`', .delete, true, '?', 1, data, where)
	sqlite_stmt_worker(db, query, data, where) ?
}

// table
pub fn (db DB) create(table string, fields []orm.OrmTableField) ? {
	query := orm.orm_table_gen(table, '`', true, 0, fields, sqlite_type_from_v) or { return err }
	err := db.exec_none(query)
	if err != sqlite_ok && err != sqlite_done {
		return db.error_message(err, query)
	}
}

pub fn (db DB) drop(table string) ? {
	query := 'DROP TABLE `$table`;'
	err := db.exec_none(query)
	if err != sqlite_ok && err != sqlite_done {
		return db.error_message(err, query)
	}
}

// helper

fn sqlite_stmt_worker(db DB, query string, data orm.OrmQueryData, where orm.OrmQueryData) ? {
	stmt := db.new_init_stmt(query)
	sqlite_stmt_binder(stmt, data, query) ?
	sqlite_stmt_binder(stmt, where, query) ?
	stmt.orm_step(query) ?
	stmt.finalize()
}

fn sqlite_stmt_binder(stmt Stmt, d orm.OrmQueryData, query string) ? {
	mut c := 1
	for data in d.data {
		eprintln(c)
		eprintln(data)
		mut err := 0
		match data {
			i8, i16, int, byte, u16, u32, bool {
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
		}
		eprintln('error: $err')
		if err != 0 {
			return stmt.db.error_message(err, query)
		}
		c++
	}
}

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
		primitive = time.unix(stmt.get_int(idx))
	} else {
		return error('Unknown type $typ')
	}

	return primitive
}

fn sqlite_type_from_v(typ int) ?string {
	return if typ in orm.nums || typ < 0 || typ in orm.num64 {
		'INTEGER'
	} else if typ in orm.float {
		'REAL'
	} else if typ == orm.string {
		'TEXT'
	} else {
		error('Unknown type $typ')
	}
}
