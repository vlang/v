module pg

import orm
import time
import net.conv

// sql expr

// @select is used internally by V's ORM for processing `SELECT ` queries
pub fn (db DB) @select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	query := orm.orm_select_gen(config, '"', true, '$', 1, where)

	rows := pg_stmt_worker(db, query, where, data)!

	mut ret := [][]orm.Primitive{}

	if config.is_count {
	}

	for row in rows {
		mut row_data := []orm.Primitive{}
		for i, val in row.vals {
			row_data << val_to_primitive(val, config.types[i])!
		}
		ret << row_data
	}

	return ret
}

// sql stmt

// insert is used internally by V's ORM for processing `INSERT ` queries
pub fn (db DB) insert(table string, data orm.QueryData) ! {
	query, converted_data := orm.orm_stmt_gen(.default, table, '"', .insert, true, '$',
		1, data, orm.QueryData{})
	pg_stmt_worker(db, query, converted_data, orm.QueryData{})!
}

// update is used internally by V's ORM for processing `UPDATE ` queries
pub fn (db DB) update(table string, data orm.QueryData, where orm.QueryData) ! {
	query, _ := orm.orm_stmt_gen(.default, table, '"', .update, true, '$', 1, data, where)
	pg_stmt_worker(db, query, data, where)!
}

// delete is used internally by V's ORM for processing `DELETE ` queries
pub fn (db DB) delete(table string, where orm.QueryData) ! {
	query, _ := orm.orm_stmt_gen(.default, table, '"', .delete, true, '$', 1, orm.QueryData{},
		where)
	pg_stmt_worker(db, query, orm.QueryData{}, where)!
}

// last_id is used internally by V's ORM for post-processing `INSERT ` queries
pub fn (db DB) last_id() int {
	query := 'SELECT LASTVAL();'

	return db.q_int(query) or { 0 }
}

// DDL (table creation/destroying etc)

// create is used internally by V's ORM for processing table creation queries (DDL)
pub fn (db DB) create(table string, fields []orm.TableField) ! {
	query := orm.orm_table_gen(table, '"', true, 0, fields, pg_type_from_v, false) or { return err }
	pg_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{})!
}

// drop is used internally by V's ORM for processing table destroying queries (DDL)
pub fn (db DB) drop(table string) ! {
	query := 'DROP TABLE "${table}";'
	pg_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{})!
}

// utils

fn pg_stmt_worker(db DB, query string, data orm.QueryData, where orm.QueryData) ![]Row {
	mut param_types := []u32{}
	mut param_vals := []&char{}
	mut param_lens := []int{}
	mut param_formats := []int{}

	pg_stmt_binder(mut param_types, mut param_vals, mut param_lens, mut param_formats,
		data)
	pg_stmt_binder(mut param_types, mut param_vals, mut param_lens, mut param_formats,
		where)

	res := C.PQexecParams(db.conn, &char(query.str), param_vals.len, param_types.data,
		param_vals.data, param_lens.data, param_formats.data, 0) // here, the last 0 means require text results, 1 - binary results
	return db.handle_error_or_result(res, 'orm_stmt_worker')
}

fn pg_stmt_binder(mut types []u32, mut vals []&char, mut lens []int, mut formats []int, d orm.QueryData) {
	for data in d.data {
		pg_stmt_match(mut types, mut vals, mut lens, mut formats, data)
	}
}

fn pg_stmt_match(mut types []u32, mut vals []&char, mut lens []int, mut formats []int, data orm.Primitive) {
	match data {
		bool {
			types << u32(Oid.t_bool)
			vals << &char(&data)
			lens << int(sizeof(bool))
			formats << 1
		}
		u8 {
			types << u32(Oid.t_char)
			vals << &char(&data)
			lens << int(sizeof(u8))
			formats << 1
		}
		u16 {
			types << u32(Oid.t_int2)
			num := conv.hton16(data)
			vals << &char(&num)
			lens << int(sizeof(u16))
			formats << 1
		}
		u32 {
			types << u32(Oid.t_int4)
			num := conv.hton32(data)
			vals << &char(&num)
			lens << int(sizeof(u32))
			formats << 1
		}
		u64 {
			types << u32(Oid.t_int8)
			num := conv.hton64(data)
			vals << &char(&num)
			lens << int(sizeof(u64))
			formats << 1
		}
		i8 {
			types << u32(Oid.t_char)
			vals << &char(&data)
			lens << int(sizeof(i8))
			formats << 1
		}
		i16 {
			types << u32(Oid.t_int2)
			num := conv.hton16(u16(data))
			vals << &char(&num)
			lens << int(sizeof(i16))
			formats << 1
		}
		int {
			types << u32(Oid.t_int4)
			num := conv.hton32(u32(data))
			vals << &char(&num)
			lens << int(sizeof(int))
			formats << 1
		}
		i64 {
			types << u32(Oid.t_int8)
			num := conv.hton64(u64(data))
			vals << &char(&num)
			lens << int(sizeof(i64))
			formats << 1
		}
		f32 {
			types << u32(Oid.t_float4)
			vals << &char(&data)
			lens << int(sizeof(f32))
			formats << 1
		}
		f64 {
			types << u32(Oid.t_float8)
			vals << &char(&data)
			lens << int(sizeof(f64))
			formats << 1
		}
		string {
			// If paramTypes is NULL, or any particular element in the array is zero,
			// the server infers a data type for the parameter symbol in the same way
			// it would do for an untyped literal string.
			types << u32(0)
			vals << &char(data.str)
			lens << data.len
			formats << 0
		}
		time.Time {
			datetime := data.format_ss()
			types << u32(0)
			vals << &char(datetime.str)
			lens << datetime.len
			formats << 0
		}
		orm.InfixType {
			pg_stmt_match(mut types, mut vals, mut lens, mut formats, data.right)
		}
		orm.Null {
			types << u32(0) // we do not know col type, let server infer
			vals << &char(0) // NULL pointer indicates NULL
			lens << int(0) // ignored
			formats << 0 // ignored
		}
	}
}

fn pg_type_from_v(typ int) !string {
	str := match typ {
		orm.type_idx['i8'], orm.type_idx['i16'], orm.type_idx['u8'], orm.type_idx['u16'] {
			'SMALLINT'
		}
		orm.type_idx['bool'] {
			'BOOLEAN'
		}
		orm.type_idx['int'], orm.type_idx['u32'] {
			'INT'
		}
		orm.time {
			'TIMESTAMP'
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
		orm.type_string {
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
		return error('Unknown type ${typ}')
	}
	return str
}

fn val_to_primitive(val ?string, typ int) !orm.Primitive {
	if str := val {
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
			orm.type_string {
				return orm.Primitive(str)
			}
			orm.time {
				if str.contains_any(' /:-') {
					date_time_str := time.parse(str)!
					return orm.Primitive(date_time_str)
				}

				timestamp := str.int()
				return orm.Primitive(time.unix(timestamp))
			}
			else {}
		}
		return error('Unknown field type ${typ}')
	} else {
		return orm.Null{}
	}
}
