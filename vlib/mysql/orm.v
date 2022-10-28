module mysql

import orm
import time

type Prims = f32 | f64 | i16 | i64 | i8 | int | string | u16 | u32 | u64 | u8

// sql expr

pub fn (db Connection) @select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	query := orm.orm_select_gen(config, '`', false, '?', 0, where)
	mut ret := [][]orm.Primitive{}
	mut stmt := db.init_stmt(query)
	stmt.prepare()!

	mysql_stmt_binder(mut stmt, where)!
	mysql_stmt_binder(mut stmt, data)!

	if data.data.len > 0 || where.data.len > 0 {
		stmt.bind_params()!
	}

	mut status := stmt.execute()!
	num_fields := stmt.get_field_count()
	metadata := stmt.gen_metadata()
	fields := stmt.fetch_fields(metadata)
	mut dataptr := []&u8{}

	for i in 0 .. num_fields {
		f := unsafe { fields[i] }
		match unsafe { FieldType(f.@type) } {
			.type_tiny {
				dataptr << unsafe { malloc(1) }
			}
			.type_short {
				dataptr << unsafe { malloc(2) }
			}
			.type_long {
				dataptr << unsafe { malloc(4) }
			}
			.type_longlong {
				dataptr << unsafe { malloc(8) }
			}
			.type_float {
				dataptr << unsafe { malloc(4) }
			}
			.type_double {
				dataptr << unsafe { malloc(8) }
			}
			.type_time, .type_date, .type_datetime, .type_time2, .type_datetime2 {
				dataptr << unsafe { malloc(sizeof(C.MYSQL_TIME)) }
			}
			.type_string, .type_blob {
				dataptr << unsafe { malloc(512) }
			}
			.type_var_string {
				dataptr << unsafe { malloc(2) }
			}
			else {
				return error('\'${unsafe { FieldType(f.@type) }}\' is not yet implemented. Please create a new issue at https://github.com/vlang/v/issues/new')
			}
		}
	}

	lens := []u32{len: int(num_fields), init: 0}
	stmt.bind_res(fields, dataptr, lens, num_fields)

	mut row := 0
	mut types := config.types.clone()
	mut field_types := []FieldType{}
	if config.is_count {
		types = [orm.type_idx['u64']]
	}

	for i, mut mysql_bind in stmt.res {
		f := unsafe { fields[i] }
		field_types << unsafe { FieldType(f.@type) }
		match types[i] {
			orm.string {
				mysql_bind.buffer_type = C.MYSQL_TYPE_BLOB
				mysql_bind.buffer_length = FieldType.type_blob.get_len()
			}
			orm.time {
				match unsafe { FieldType(f.@type) } {
					.type_long {
						mysql_bind.buffer_type = C.MYSQL_TYPE_LONG
					}
					.type_time, .type_date, .type_datetime {
						mysql_bind.buffer_type = C.MYSQL_TYPE_BLOB
						mysql_bind.buffer_length = FieldType.type_blob.get_len()
					}
					.type_string, .type_blob {}
					else {
						return error('Unknown type ${f.@type}')
					}
				}
			}
			else {}
		}
	}

	stmt.bind_result_buffer()!
	stmt.store_result()!
	for {
		status = stmt.fetch_stmt()!

		if status == 1 || status == 100 {
			break
		}
		row++

		data_list := buffer_to_primitive(dataptr, types, field_types)!
		ret << data_list
	}

	stmt.close()!

	return ret
}

// sql stmt

pub fn (db Connection) insert(table string, data orm.QueryData) ! {
	mut converted_primitive_array := db.factory_orm_primitive_converted_from_sql(table,
		data)!

	converted_primitive_data := orm.QueryData{
		fields: data.fields
		data: converted_primitive_array
		types: []
		kinds: []
		is_and: []
	}

	query, converted_data := orm.orm_stmt_gen(table, '`', .insert, false, '?', 1, converted_primitive_data,
		orm.QueryData{})
	mysql_stmt_worker(db, query, converted_data, orm.QueryData{})!
}

pub fn (db Connection) update(table string, data orm.QueryData, where orm.QueryData) ! {
	query, _ := orm.orm_stmt_gen(table, '`', .update, false, '?', 1, data, where)
	mysql_stmt_worker(db, query, data, where)!
}

pub fn (db Connection) delete(table string, where orm.QueryData) ! {
	query, _ := orm.orm_stmt_gen(table, '`', .delete, false, '?', 1, orm.QueryData{},
		where)
	mysql_stmt_worker(db, query, orm.QueryData{}, where)!
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
pub fn (db Connection) create(table string, fields []orm.TableField) ! {
	query := orm.orm_table_gen(table, '`', true, 0, fields, mysql_type_from_v, false) or {
		return err
	}
	mysql_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{})!
}

pub fn (db Connection) drop(table string) ! {
	query := 'DROP TABLE `$table`;'
	mysql_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{})!
}

fn mysql_stmt_worker(db Connection, query string, data orm.QueryData, where orm.QueryData) ! {
	mut stmt := db.init_stmt(query)
	stmt.prepare()!
	mysql_stmt_binder(mut stmt, data)!
	mysql_stmt_binder(mut stmt, where)!
	if data.data.len > 0 || where.data.len > 0 {
		stmt.bind_params()!
	}
	stmt.execute()!
	stmt.close()!
}

fn mysql_stmt_binder(mut stmt Stmt, d orm.QueryData) ! {
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
			unix := int(data.unix)
			stmt_binder_match(mut stmt, unix)
		}
		orm.InfixType {
			stmt_binder_match(mut stmt, data.right)
		}
	}
}

fn buffer_to_primitive(data_list []&u8, types []int, field_types []FieldType) ![]orm.Primitive {
	mut res := []orm.Primitive{}

	for i, data in data_list {
		mut primitive := orm.Primitive(0)
		match types[i] {
			orm.type_idx['i8'] {
				primitive = *(unsafe { &i8(data) })
			}
			orm.type_idx['i16'] {
				primitive = *(unsafe { &i16(data) })
			}
			orm.type_idx['int'], orm.serial {
				primitive = *(unsafe { &int(data) })
			}
			orm.type_idx['i64'] {
				primitive = *(unsafe { &i64(data) })
			}
			orm.type_idx['u8'] {
				primitive = *(unsafe { &u8(data) })
			}
			orm.type_idx['u16'] {
				primitive = *(unsafe { &u16(data) })
			}
			orm.type_idx['u32'] {
				primitive = *(unsafe { &u32(data) })
			}
			orm.type_idx['u64'] {
				primitive = *(unsafe { &u64(data) })
			}
			orm.type_idx['f32'] {
				primitive = *(unsafe { &f32(data) })
			}
			orm.type_idx['f64'] {
				primitive = *(unsafe { &f64(data) })
			}
			orm.type_idx['bool'] {
				primitive = *(unsafe { &bool(data) })
			}
			orm.string {
				primitive = unsafe { cstring_to_vstring(&char(data)) }
			}
			orm.time {
				match field_types[i] {
					.type_long {
						timestamp := *(unsafe { &int(data) })
						primitive = time.unix(timestamp)
					}
					.type_datetime {
						string_time := unsafe { cstring_to_vstring(&char(data)) }
						primitive = time.parse(string_time)!
					}
					else {}
				}
			}
			else {
				return error('Unknown type ${types[i]}')
			}
		}
		res << primitive
	}

	return res
}

fn mysql_type_from_v(typ int) !string {
	str := match typ {
		orm.type_idx['i8'], orm.type_idx['u8'] {
			'TINYINT'
		}
		orm.type_idx['i16'], orm.type_idx['u16'] {
			'SMALLINT'
		}
		orm.type_idx['int'], orm.type_idx['u32'], orm.time {
			'INT'
		}
		orm.type_idx['i64'], orm.type_idx['u64'] {
			'BIGINT'
		}
		orm.type_idx['f32'] {
			'FLOAT'
		}
		orm.type_idx['f64'] {
			'DOUBLE'
		}
		orm.string {
			'TEXT'
		}
		orm.serial {
			'SERIAL'
		}
		orm.type_idx['bool'] {
			'BOOLEAN'
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

fn (db Connection) factory_orm_primitive_converted_from_sql(table string, data orm.QueryData) ![]orm.Primitive {
	mut map_val := db.get_table_data_type_map(table)!

	// adapt v type to sql time
	mut converted_data := []orm.Primitive{}
	for i, field in data.fields {
		match data.data[i].type_name() {
			'time.Time' {
				if map_val[field] == 'datetime' {
					converted_data << orm.Primitive((data.data[i] as time.Time).str())
				} else {
					converted_data << data.data[i]
				}
			}
			else {
				converted_data << data.data[i]
			}
		}
	}
	return converted_data
}

fn (db Connection) get_table_data_type_map(table string) !map[string]string {
	data_type_querys := "SELECT COLUMN_NAME, DATA_TYPE  FROM INFORMATION_SCHEMA.COLUMNS  WHERE TABLE_NAME = '$table'"
	mut map_val := map[string]string{}

	results := db.query(data_type_querys)!
	db.use_result()

	for row in results.rows() {
		map_val[row.vals[0]] = row.vals[1]
	}

	unsafe { results.free() }
	return map_val
}
