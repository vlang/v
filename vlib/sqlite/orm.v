module sqlite

import orm

// sql expr

pub fn (db DB) @select(config orm.OrmSelectConfig, data orm.OrmQueryData, where orm.OrmQueryData) ?[][]string {
	query := orm.orm_select_gen(config, '`', true, '?', 1, where)
	stmt := db.new_init_stmt(query)
	sqlite_stmt_binder(stmt, data, query) ?
	sqlite_stmt_binder(stmt, where, query) ?

	mut ret := [][]string{}

	if config.is_count {
		stmt.orm_step(query) ?
		ret << [stmt.get_count().str()]
		return ret
	}

	for {
		step := stmt.step()
		if step == sqlite_done {
			break
		}
		if step != sqlite_ok && step != sqlite_row {
			break
		}
		mut row := []string{}
		for i, typ in config.types {
			row << stmt.sqlite_select_column(i, typ) ?
		}

		ret << row
	}

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
	for i, data in d.data {
		mut err := 0
		typ := d.types[i]
		if typ in orm.nums {
			err = stmt.bind_int(c, &int(data))
		} else if typ in orm.num64 {
			err = stmt.bind_i64(c, &i64(data))
		} else if typ in orm.float {
			err = stmt.bind_f64(c, &f64(data))
		} else if typ == orm.string {
			err = stmt.bind_text(c, unsafe { (&char(data)).vstring() })
		}
		if err != 0 {
			return stmt.db.error_message(err, query)
		}
		c++
	}
}

fn (stmt Stmt) sqlite_select_column(idx int, typ int) ?string {
	return if typ in orm.nums || typ == -1 {
		stmt.get_int(idx).str()
	} else if typ in orm.num64 {
		stmt.get_i64(idx).str()
	} else if typ in orm.float {
		stmt.get_f64(idx).str()
	} else if typ == orm.string {
		stmt.get_f64(idx)
	} else {
		error('Unknown type $typ')
	}
}

fn sqlite_type_from_v(typ int) ?string {
	return if typ in orm.nums || typ == -1 || typ in orm.num64 {
		'INTEGER'
	} else if typ in orm.float {
		'REAL'
	} else if typ == orm.string {
		'TEXT'
	} else {
		error('Unknown type $typ')
	}
}
