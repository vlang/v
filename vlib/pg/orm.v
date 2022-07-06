module pg

import orm
import time
import net.conv

// sql expr

pub fn (db DB) @select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ?[][]orm.Primitive {
	query := orm.orm_select_gen(config, '"', true, '$', 1, where)

	res := pg_stmt_worker(db, query, where, data)?

	mut ret := [][]orm.Primitive{}

	if config.is_count {
	}

	for row in res {
		mut row_data := []orm.Primitive{}
		for i, val in row.vals {
			field := str_to_primitive(val, config.types[i])?
			row_data << field
		}
		ret << row_data
	}

	return ret
}

// sql stmt

pub fn (db DB) insert(table string, data orm.QueryData) ? {
	query := orm.orm_stmt_gen(table, '"', .insert, true, '$', 1, data, orm.QueryData{})
	pg_stmt_worker(db, query, data, orm.QueryData{})?
}

pub fn (db DB) update(table string, data orm.QueryData, where orm.QueryData) ? {
	query := orm.orm_stmt_gen(table, '"', .update, true, '$', 1, data, where)
	pg_stmt_worker(db, query, data, where)?
}

pub fn (db DB) delete(table string, where orm.QueryData) ? {
	query := orm.orm_stmt_gen(table, '"', .delete, true, '$', 1, orm.QueryData{}, where)
	pg_stmt_worker(db, query, orm.QueryData{}, where)?
}

pub fn (db DB) last_id() orm.Primitive {
	query := 'SELECT LASTVAL();'
	id := db.q_int(query) or { 0 }
	return orm.Primitive(id)
}

// table

pub fn (db DB) create(table string, fields []orm.TableField) ? {
	query := orm.orm_table_gen(table, '"', true, 0, fields, pg_type_from_v, false) or { return err }
	pg_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{})?
}

pub fn (db DB) drop(table string) ? {
	query := 'DROP TABLE "$table";'
	pg_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{})?
}

// utils

fn pg_stmt_worker(db DB, query string, data orm.QueryData, where orm.QueryData) ?[]Row {
	mut param_types := []u32{}
	mut param_vals := []&char{}
	mut param_lens := []int{}
	mut param_formats := []int{}

	pg_stmt_binder(mut param_types, mut param_vals, mut param_lens, mut param_formats,
		data)
	pg_stmt_binder(mut param_types, mut param_vals, mut param_lens, mut param_formats,
		where)

	res := C.PQexecParams(db.conn, query.str, param_vals.len, param_types.data, param_vals.data,
		param_lens.data, param_formats.data, 0)
	return db.handle_error_or_result(res, 'orm_stmt_worker')
}

fn pg_stmt_binder(mut types []u32, mut vals []&char, mut lens []int, mut formats []int, d orm.QueryData) {
	for data in d.data {
		pg_stmt_match(mut types, mut vals, mut lens, mut formats, data)
	}
}

fn pg_stmt_match(mut types []u32, mut vals []&char, mut lens []int, mut formats []int, data orm.Primitive) {
	d := data
	match data {
		bool {
			types << u32(Oid.t_bool)
			vals << &char(&(d as bool))
			lens << int(sizeof(bool))
			formats << 1
		}
		u8 {
			types << u32(Oid.t_char)
			vals << &char(&(d as u8))
			lens << int(sizeof(u8))
			formats << 1
		}
		u16 {
			types << u32(Oid.t_int2)
			num := conv.htn16(&data)
			vals << &char(&num)
			lens << int(sizeof(u16))
			formats << 1
		}
		u32 {
			types << u32(Oid.t_int4)
			num := conv.htn32(&data)
			vals << &char(&num)
			lens << int(sizeof(u32))
			formats << 1
		}
		u64 {
			types << u32(Oid.t_int8)
			num := conv.htn64(&data)
			vals << &char(&num)
			lens << int(sizeof(u64))
			formats << 1
		}
		i8 {
			types << u32(Oid.t_char)
			vals << &char(&(d as i8))
			lens << int(sizeof(i8))
			formats << 1
		}
		i16 {
			types << u32(Oid.t_int2)
			num := conv.htn16(unsafe { &u16(&data) })
			vals << &char(&num)
			lens << int(sizeof(i16))
			formats << 1
		}
		int {
			types << u32(Oid.t_int4)
			num := conv.htn32(unsafe { &u32(&data) })
			vals << &char(&num)
			lens << int(sizeof(int))
			formats << 1
		}
		i64 {
			types << u32(Oid.t_int8)
			num := conv.htn64(unsafe { &u64(&data) })
			vals << &char(&num)
			lens << int(sizeof(i64))
			formats << 1
		}
		f32 {
			types << u32(Oid.t_float4)
			vals << &char(unsafe { &f32(&(d as f32)) })
			lens << int(sizeof(f32))
			formats << 1
		}
		f64 {
			types << u32(Oid.t_float8)
			vals << &char(unsafe { &f64(&(d as f64)) })
			lens << int(sizeof(f64))
			formats << 1
		}
		string {
			types << u32(Oid.t_text)
			vals << data.str
			lens << data.len
			formats << 0
		}
		time.Time {
			types << u32(Oid.t_int4)
			unix := int(data.unix)
			num := conv.htn32(unsafe { &u32(&unix) })
			vals << &char(&num)
			lens << int(sizeof(u32))
			formats << 1
		}
		orm.InfixType {
			pg_stmt_match(mut types, mut vals, mut lens, mut formats, data.right)
		}
	}
}

fn pg_type_from_v(typ int) ?string {
	str := match typ {
		orm.type_idx['i8'], orm.type_idx['i16'], orm.type_idx['u8'], orm.type_idx['u16'] {
			'SMALLINT'
		}
		orm.type_idx['bool'] {
			'BOOLEAN'
		}
		orm.type_idx['int'], orm.type_idx['u32'], orm.time {
			'INT'
		}
		orm.type_idx['i64'], orm.type_idx['u64'] {
			'BIGINT'
		}
		orm.float[0] {
			'REAL'
		}
		orm.float[1] {
			'DOUBLE PRECISION'
		}
		orm.string {
			'TEXT'
		}
		orm.serial {
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

fn str_to_primitive(str string, typ int) ?orm.Primitive {
	match typ {
		// bool
		orm.type_idx['bool'] {
			return orm.Primitive(str == 't')
		}
		// i8
		orm.type_idx['i8'] {
			return orm.Primitive(str.i8())
		}
		// i16
		orm.type_idx['i16'] {
			return orm.Primitive(str.i16())
		}
		// int
		orm.type_idx['int'] {
			return orm.Primitive(str.int())
		}
		// i64
		orm.type_idx['i64'] {
			return orm.Primitive(str.i64())
		}
		// u8
		orm.type_idx['u8'] {
			data := str.i8()
			return orm.Primitive(*unsafe { &u8(&data) })
		}
		// u16
		orm.type_idx['u16'] {
			data := str.i16()
			return orm.Primitive(*unsafe { &u16(&data) })
		}
		// u32
		orm.type_idx['u32'] {
			data := str.int()
			return orm.Primitive(*unsafe { &u32(&data) })
		}
		// u64
		orm.type_idx['u64'] {
			data := str.i64()
			return orm.Primitive(*unsafe { &u64(&data) })
		}
		// f32
		orm.type_idx['f32'] {
			return orm.Primitive(str.f32())
		}
		// f64
		orm.type_idx['f64'] {
			return orm.Primitive(str.f64())
		}
		orm.string {
			return orm.Primitive(str)
		}
		orm.time {
			if str.contains_any(' /:-') {
				date_time_str := time.parse(str)?
				return orm.Primitive(date_time_str)
			}

			timestamp := str.int()
			return orm.Primitive(time.unix(timestamp))
		}
		else {}
	}
	return error('Unknown field type $typ')
}
