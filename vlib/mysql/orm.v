module mysql

import orm
import time

// sql expr

pub fn (db Connection) @select(config orm.OrmSelectConfig, data orm.OrmQueryData, where orm.OrmQueryData) ?[][]orm.Primitive {
	query := orm.orm_select_gen(config, '`', false, '?', 0, where)
	mut ret := [][]orm.Primitive{}
	eprintln(query)
	mut stmt := db.init_stmt(query)
	stmt.prepare() ?
	mysql_stmt_binder(mut stmt, where) ?
	mysql_stmt_binder(mut stmt, data) ?
	if data.data.len > 0 || where.data.len > 0 {
		stmt.bind_params() ?
	}

	num_fields := stmt.get_field_count()

	metadata := stmt.gen_metadata()

	fields := stmt.fetch_fields(metadata)

	dataptr := []voidptr{len: int(num_fields)}
	data_len_ptr := []&u32{len: int(num_fields)}
	mut data_lens := []int{len: int(num_fields)}

	for i, typ in config.types {
		if typ == orm.string {
			data_lens[i] = orm.string_max_len
		}
	}

	stmt.bind_res(fields, dataptr, data_lens, data_len_ptr, num_fields)
	stmt.bind_result_buffer() ?

	mut status := stmt.execute() ?
	for {
		status = stmt.fetch_stmt() ?

		if status == 100 {
			eprintln('no data anymore')
			break
		}
		data_list := buffer_to_primitive(dataptr, config.types) ?
		ret << data_list
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

fn buffer_to_primitive(data_list []voidptr, types []int) ?[]orm.Primitive {
	mut res := []orm.Primitive{}
	for i in 0 .. data_list.len {
		data := data_list[i]
		mut primitive := orm.Primitive(0)
		match types[i] {
			5 {
				primitive = *(&i8(data))
			}
			6 {
				primitive = *(&i16(data))
			}
			7, -1 {
				primitive = *(&int(data))
			}
			8 {
				primitive = *(&i64(data))
			}
			9 {
				primitive = *(&byte(data))
			}
			10 {
				primitive = *(&u16(data))
			}
			11 {
				primitive = *(&u32(data))
			}
			12 {
				primitive = *(&u64(data))
			}
			13 {
				primitive = *(&f32(data))
			}
			14 {
				primitive = *(&f64(data))
			}
			15 {
				primitive = *(&bool(data))
			}
			orm.string {
				primitive = cstring_to_vstring(&char(data))
			}
			orm.time {
				timestamp := *(&int(data))
				primitive = time.unix(timestamp)
			}
			else {
				return error('Unknown type ${types[i]}')
			}
		}
		res << primitive
	}
	return res
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
