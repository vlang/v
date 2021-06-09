module mysql

import orm
import time

// sql expr

pub fn (db Connection) @select(config orm.OrmSelectConfig, data orm.OrmQueryData, where orm.OrmQueryData) ?[][]orm.Primitive {
	query := orm.orm_select_gen(config, '`', false, '?', 0, where)
	mut ret := [][]orm.Primitive{}
	mut stmt := db.init_stmt(query)
	stmt.prepare() ?
	mysql_stmt_binder(mut stmt, where) ?
	mysql_stmt_binder(mut stmt, data) ?
	if data.data.len > 0 || where.data.len > 0 {
		stmt.bind_params() ?
	}
	mut rb := stmt.gen_result_buffer()
	rb.bind_result_buffer() ?
	stmt.execute() ?

	for {
		res := rb.fetch_row()
		if res == mysql_no_data {
			break
		}
		if res != 0 {
			return stmt.error(res)
		}
		mut row := []orm.Primitive{}
		for rb.next() {
			primitive := rb.mysql_select_column(config.types[rb.idx]) ?
			row << primitive
		}
		ret << row
	}

	stmt.close() ?

	return ret
}

// sql stmt

pub fn (db Connection) insert(table string, data orm.OrmQueryData) ? {
	query := orm.orm_stmt_gen(table, '`', .insert, false, '?', 1, data, orm.OrmQueryData{})
	mysql_stmt_worker(db, query, data, orm.OrmQueryData{}) ?
}

pub fn (db Connection) update(table string, data orm.OrmQueryData, where orm.OrmQueryData) ? {
	query := orm.orm_stmt_gen(table, '`', .update, false, '?', 1, data, where)
	mysql_stmt_worker(db, query, data, where) ?
}

pub fn (db Connection) delete(table string, where orm.OrmQueryData) ? {
	query := orm.orm_stmt_gen(table, '`', .delete, false, '?', 1, orm.OrmQueryData{},
		where)
	mysql_stmt_worker(db, query, orm.OrmQueryData{}, where) ?
}

pub fn (db Connection) last_id() orm.Primitive {
	// query := 'SELECT last_insert_rowid();'
	// id := db.q_int(query)
	return orm.Primitive(0)
}

// table
pub fn (db Connection) create(table string, fields []orm.OrmTableField) ? {
	query := orm.orm_table_gen(table, '`', false, 0, fields, mysql_type_from_v, false) or {
		return err
	}
	mysql_stmt_worker(db, query, orm.OrmQueryData{}, orm.OrmQueryData{}) ?
}

pub fn (db Connection) drop(table string) ? {
	query := 'DROP TABLE `$table`;'
	mysql_stmt_worker(db, query, orm.OrmQueryData{}, orm.OrmQueryData{}) ?
}

fn mysql_stmt_worker(db Connection, query string, data orm.OrmQueryData, where orm.OrmQueryData) ? {
	mut stmt := db.init_stmt(query)
	stmt.prepare() ?
	mysql_stmt_binder(mut stmt, data) ?
	mysql_stmt_binder(mut stmt, where) ?
	if data.data.len > 0 || where.data.len > 0 {
		stmt.bind_params() ?
	}
	stmt.execute() ?
	stmt.close() ?
}

fn mysql_stmt_binder(mut stmt Stmt, d orm.OrmQueryData) ? {
	for data in d.data {
		match data {
			bool {
				stmt.bind_bool(&data)
			}
			i8 {
				stmt.bind_i8(&data)
			}
			i16 {
				stmt.bind_i16(&data)
			}
			int {
				stmt.bind_int(&data)
			}
			i64 {
				stmt.bind_i64(&data)
			}
			byte {
				stmt.bind_byte(&data)
			}
			u16 {
				stmt.bind_u16(&data)
			}
			u32 {
				stmt.bind_u32(&data)
			}
			u64 {
				stmt.bind_u64(&data)
			}
			f32 {
				stmt.bind_f32(unsafe { &f32(&data) })
			}
			f64 {
				stmt.bind_f64(unsafe { &f64(&data) })
			}
			string {
				stmt.bind_text(data)
			}
			time.Time {
				stmt.bind_int(&int(data.unix))
			}
		}
	}
}


fn (mut rb StmtResultBuffer) mysql_select_column(typ int) ?orm.Primitive {
	mut primitive := orm.Primitive(0)
	match typ {
		5 {
			primitive = rb.get_i8() ?
		}
		6 {
			primitive = rb.get_i16() ?
		}
		7, -1 {
			primitive = rb.get_int() ?
		}
		8 {
			primitive = rb.get_i64() ?
		}
		9 {
			primitive = rb.get_byte() ?
		}
		10 {
			primitive = rb.get_u16() ?
		}
		11 {
			primitive = rb.get_u32() ?
		}
		12 {
			primitive = rb.get_u64() ?
		}
		13 {
			primitive = rb.get_f32() ?
		}
		14 {
			primitive = rb.get_f64() ?
		}
		15 {
			primitive = rb.get_bool() ?
		}
		orm.string {
			primitive = rb.get_text() ?
		}
		orm.time {
			timestamp := rb.get_int() ?
			primitive = time.unix(timestamp)
		}
		else {
			return error('Unknown type $typ')
		}
	}

	return primitive
}

fn mysql_type_from_v(typ int) ?string {
	str := match typ {
		5, 9, 16 {
			'TINYINT'
		}
		6, 10 {
			'SMALLINT'
		}
		7, 11 {
			'INT'
		}
		8, 12 {
			'BIGINT'
		}
		13 {
			'FLOAT'
		}
		14 {
			'DOUBLE'
		}
		orm.string {
			'TEXT'
		}
		-1 {
			'SERIAL'
		}
		else {
			''
		}
	}
	if str == '' {
		return error('Unknown type $typ')
	}
	return str
}
