module mysql

import orm
import time

type Prims = f32 | f64 | i16 | i64 | i8 | int | string | u16 | u32 | u64 | u8

// sql expr

pub fn (db Connection) @select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ?[][]orm.Primitive {
	query := orm.orm_select_gen(config, '`', false, '?', 0, where)
	mut ret := [][]orm.Primitive{}
	mut stmt := db.init_stmt(query)
	stmt.prepare() ?

	mysql_stmt_binder(mut stmt, where) ?
	mysql_stmt_binder(mut stmt, data) ?
	if data.data.len > 0 || where.data.len > 0 {
		stmt.bind_params() ?
	}

	mut status := stmt.execute() ?
	num_fields := stmt.get_field_count()
	metadata := stmt.gen_metadata()
	fields := stmt.fetch_fields(metadata)

	mut dataptr := []Prims{}

	for i in 0 .. num_fields {
		f := unsafe { fields[i] }
		match FieldType(f.@type) {
			.type_tiny {
				dataptr << u8(0)
			}
			.type_short {
				dataptr << u16(0)
			}
			.type_long {
				dataptr << u32(0)
			}
			.type_longlong {
				dataptr << u64(0)
			}
			.type_float {
				dataptr << f32(0)
			}
			.type_double {
				dataptr << f64(0)
			}
			.type_string {
				dataptr << ''
			}
			else {
				dataptr << u8(0)
			}
		}
	}

	mut vptr := []&char{}

	for d in dataptr {
		vptr << d.get_data_ptr()
	}

	unsafe { dataptr.free() }

	lens := []u32{len: int(num_fields), init: 0}
	stmt.bind_res(fields, vptr, lens, num_fields)
	stmt.bind_result_buffer() ?
	stmt.store_result() ?

	mut row := 0

	for {
		status = stmt.fetch_stmt() ?

		if status == 1 || status == 100 {
			break
		}
		row++
		data_list := buffer_to_primitive(vptr, config.types) ?
		ret << data_list
	}

	stmt.close() ?

	return ret
}

// sql stmt

pub fn (db Connection) insert(table string, data orm.QueryData) ? {
	query := orm.orm_stmt_gen(table, '`', .insert, false, '?', 1, data, orm.QueryData{})
	mysql_stmt_worker(db, query, data, orm.QueryData{}) ?
}

pub fn (db Connection) update(table string, data orm.QueryData, where orm.QueryData) ? {
	query := orm.orm_stmt_gen(table, '`', .update, false, '?', 1, data, where)
	mysql_stmt_worker(db, query, data, where) ?
}

pub fn (db Connection) delete(table string, where orm.QueryData) ? {
	query := orm.orm_stmt_gen(table, '`', .delete, false, '?', 1, orm.QueryData{}, where)
	mysql_stmt_worker(db, query, orm.QueryData{}, where) ?
}

pub fn (db Connection) last_id() orm.Primitive {
	query := 'SELECT last_insert_id();'
	id := db.query(query) or {
		Result{
			result: 0
		}
	}
	return orm.Primitive(id.rows()[0].vals[0].int())
}

// table
pub fn (db Connection) create(table string, fields []orm.TableField) ? {
	query := orm.orm_table_gen(table, '`', false, 0, fields, mysql_type_from_v, false) or {
		return err
	}
	mysql_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{}) ?
}

pub fn (db Connection) drop(table string) ? {
	query := 'DROP TABLE `$table`;'
	mysql_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{}) ?
}

fn mysql_stmt_worker(db Connection, query string, data orm.QueryData, where orm.QueryData) ? {
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

fn mysql_stmt_binder(mut stmt Stmt, d orm.QueryData) ? {
	for data in d.data {
		stmt_binder_match(mut stmt, data)
	}
}

fn stmt_binder_match(mut stmt Stmt, data orm.Primitive) {
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
		u8 {
			stmt.bind_u8(&data)
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
		orm.InfixType {
			stmt_binder_match(mut stmt, data.right)
		}
	}
}

fn buffer_to_primitive(data_list []&char, types []int) ?[]orm.Primitive {
	mut res := []orm.Primitive{}

	for i, data in data_list {
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
				primitive = *(&u8(data))
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
				primitive = unsafe { cstring_to_vstring(&char(data)) }
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
		7, 11, orm.time {
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

fn (p Prims) get_data_ptr() &char {
	return match p {
		string {
			p.str
		}
		else {
			&char(&p)
		}
	}
}
