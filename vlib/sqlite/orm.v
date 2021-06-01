module sqlite

import orm

// sql expr

pub fn (db DB) @select(table string, config orm.OrmSelectConfig, data orm.OrmQueryData, where orm.OrmQueryData) ?[][]string {
	query := orm.orm_select_gen(config, "'", true, '?', where)
	sqlite_stmt_worker(db, query, data, where) ?
	return none
}

// sql stmt

pub fn (db DB) insert(table string, data orm.OrmQueryData, where orm.OrmQueryData) ? {
	query := orm.orm_stmt_gen(table, "'", .insert, true, '?', data, where)
	sqlite_stmt_worker(db, query, data, where) ?
}

pub fn (db DB) update(table string, data orm.OrmQueryData, where orm.OrmQueryData) ? {
	query := orm.orm_stmt_gen(table, "'", .update, true, '?', data, where)
	sqlite_stmt_worker(db, query, data, where) ?
}

pub fn (db DB) delete(table string, data orm.OrmQueryData, where orm.OrmQueryData) ? {
	query := orm.orm_stmt_gen(table, "'", .delete, true, '?', data, where)
	sqlite_stmt_worker(db, query, data, where) ?
}

// table
pub fn (db DB) create(talbe string, fields []orm.OrmTableField) ? {
}

// helper

fn sqlite_stmt_worker(db DB, query string, data orm.OrmQueryData, where orm.OrmQueryData) ? {
	stmt := db.new_init_stmt(query)
	sqlite_stmt_binder(stmt, data) ?
	sqlite_stmt_binder(stmt, where) ?
	stmt.orm_step() ?
	stmt.finalize()
}

fn sqlite_stmt_binder(stmt Stmt, data orm.OrmQueryData) ? {
	mut c := 0
	for i, typ in data.types {
		mut err := 0
		if typ in orm.nums {
			err = stmt.bind_int(c, int(data.data[i]))
		} else if typ in orm.num64 {
			err = stmt.bind_i64(c, i64(data.data[i]))
		} else if typ in orm.float {
			err = stmt.bind_f64(c, f64(data.data[i]))
		} else if typ == orm.string {
			err = stmt.bind_text(c, unsafe { byteptr(data.data[i]).vstring() })
		}
		if err != 0 {
			return stmt.db.error_message(err)
		}
		c++
	}
}

fn sqlite_type_from_v(typ int) string {
	return if typ in orm.nums {
		'INT'
	} else if typ in orm.num64 {
		'INT64'
	} else if typ in orm.float {
		'DOUBLE'
	} else if typ == orm.string {
		'TEXT'
	} else {
		''
	}
}
