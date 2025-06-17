module mysql

import orm
import time

// select is used internally by V's ORM for processing `SELECT ` queries.
pub fn (db DB) select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	query := orm.orm_select_gen(config, '`', false, '?', 0, where)
	mut result := [][]orm.Primitive{}
	mut stmt := db.init_stmt(query)
	stmt.prepare()!

	mysql_stmt_bind_query_data(mut stmt, where)!
	mysql_stmt_bind_query_data(mut stmt, data)!

	if data.data.len > 0 || where.data.len > 0 {
		stmt.bind_params()!
	}

	stmt.execute()!
	metadata := stmt.gen_metadata()
	fields := stmt.fetch_fields(metadata)
	num_fields := stmt.get_field_count()
	mut data_pointers := []&u8{cap: int(num_fields)}

	// Allocate memory for each column.
	for i in 0 .. num_fields {
		field := unsafe { fields[i] }
		match unsafe { FieldType(field.type) } {
			.type_tiny {
				data_pointers << unsafe { malloc(1) }
			}
			.type_short {
				data_pointers << unsafe { malloc(2) }
			}
			.type_long, .type_float {
				data_pointers << unsafe { malloc(4) }
			}
			.type_longlong, .type_double {
				data_pointers << unsafe { malloc(8) }
			}
			.type_time, .type_date, .type_datetime, .type_time2, .type_datetime2, .type_timestamp {
				data_pointers << unsafe { malloc(sizeof(C.MYSQL_TIME)) }
			}
			.type_string, .type_var_string, .type_blob, .type_tiny_blob, .type_medium_blob,
			.type_long_blob {
				// Memory will be allocated later dynamically depending on the length of the value.
				data_pointers << &u8(unsafe { nil })
			}
			else {
				return error('\'${unsafe { FieldType(field.type) }}\' is not yet implemented. Please create a new issue at https://github.com/vlang/v/issues/new')
			}
		}
	}

	mut lengths := []u32{len: int(num_fields), init: 0}
	mut is_null := []bool{len: int(num_fields)}
	stmt.bind_res(fields, data_pointers, lengths, is_null, num_fields)

	mut types := config.types.clone()
	mut field_types := []FieldType{}
	if config.is_count {
		types = [orm.type_idx['u64']]
	}

	// Map stores column indexes and their binds in order to extract values
	// for these columns separately, with individual memory allocation for each value.
	mut string_binds_map := map[int]C.MYSQL_BIND{}

	for i, mut mysql_bind in stmt.res {
		field := unsafe { fields[i] }
		field_type := unsafe { FieldType(field.type) }
		field_types << field_type

		match field_type {
			.type_string, .type_var_string, .type_blob, .type_tiny_blob, .type_medium_blob,
			.type_long_blob {
				string_binds_map[i] = mysql_bind
			}
			.type_long {
				mysql_bind.buffer_type = C.MYSQL_TYPE_LONG
			}
			.type_time, .type_date, .type_datetime, .type_timestamp {
				// FIXME: Allocate memory for blobs dynamically.
				mysql_bind.buffer_type = C.MYSQL_TYPE_BLOB
				mysql_bind.buffer_length = FieldType.type_blob.get_len()
			}
			else {}
		}
	}

	stmt.bind_result_buffer()!
	stmt.store_result()!

	for {
		// Fetch every row from the `select` result.
		status := stmt.fetch_stmt()!
		is_error := status == 1
		are_no_rows_to_fetch := status == mysql_no_data

		if is_error || are_no_rows_to_fetch {
			break
		}

		// Fetch columns that should be allocated dynamically.
		for index, mut bind in string_binds_map {
			string_length := lengths[index] + 1
			data_pointers[index] = unsafe { malloc(string_length) }
			bind.buffer = data_pointers[index]
			bind.buffer_length = string_length
			bind.length = unsafe { nil }

			stmt.fetch_column(bind, index)!
		}

		result << data_pointers_to_primitives(is_null, data_pointers, types, field_types)!
	}

	stmt.close()!

	return result
}

// insert is used internally by V's ORM for processing `INSERT ` queries
pub fn (db DB) insert(table orm.Table, data orm.QueryData) ! {
	mut converted_primitive_array := db.convert_query_data_to_primitives(table.name, data)!

	converted_primitive_data := orm.QueryData{
		fields: data.fields
		data:   converted_primitive_array
		types:  []
		kinds:  []
		is_and: []
	}

	query, converted_data := orm.orm_stmt_gen(.default, table, '`', .insert, false, '?',
		1, converted_primitive_data, orm.QueryData{})
	mysql_stmt_worker(db, query, converted_data, orm.QueryData{})!
}

// update is used internally by V's ORM for processing `UPDATE ` queries
pub fn (db DB) update(table orm.Table, data orm.QueryData, where orm.QueryData) ! {
	query, _ := orm.orm_stmt_gen(.default, table, '`', .update, false, '?', 1, data, where)
	mysql_stmt_worker(db, query, data, where)!
}

// delete is used internally by V's ORM for processing `DELETE ` queries
pub fn (db DB) delete(table orm.Table, where orm.QueryData) ! {
	query, _ := orm.orm_stmt_gen(.default, table, '`', .delete, false, '?', 1, orm.QueryData{},
		where)
	mysql_stmt_worker(db, query, orm.QueryData{}, where)!
}

// last_id is used internally by V's ORM for post-processing `INSERT ` queries
pub fn (db DB) last_id() int {
	query := 'SELECT last_insert_id();'
	id := db.query(query) or { return 0 }

	return id.rows()[0].vals[0].int()
}

// create is used internally by V's ORM for processing table creation queries (DDL)
pub fn (db DB) create(table orm.Table, fields []orm.TableField) ! {
	query := orm.orm_table_gen(.mysql, table, '`', true, 0, fields, mysql_type_from_v,
		false) or { return err }
	mysql_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{})!
}

// drop is used internally by V's ORM for processing table destroying queries (DDL)
pub fn (db DB) drop(table orm.Table) ! {
	query := 'DROP TABLE `${table.name}`;'
	mysql_stmt_worker(db, query, orm.QueryData{}, orm.QueryData{})!
}

// mysql_stmt_worker executes the `query` with the provided `data` and `where` parameters
// without returning the result.
// This is commonly used for `INSERT`, `UPDATE`, `CREATE`, `DROP`, and `DELETE` queries.
fn mysql_stmt_worker(db DB, query string, data orm.QueryData, where orm.QueryData) ! {
	mut stmt := db.init_stmt(query)
	stmt.prepare()!

	mysql_stmt_bind_query_data(mut stmt, data)!
	mysql_stmt_bind_query_data(mut stmt, where)!

	if data.data.len > 0 || where.data.len > 0 {
		stmt.bind_params()!
	}

	stmt.execute()!
	stmt.close()!
}

// mysql_stmt_bind_query_data binds all the fields of `q` to the `stmt`.
fn mysql_stmt_bind_query_data(mut stmt Stmt, d orm.QueryData) ! {
	for data in d.data {
		stmt_bind_primitive(mut stmt, data)
	}
}

// stmt_bind_primitive binds the `data` to the `stmt`.
fn stmt_bind_primitive(mut stmt Stmt, data orm.Primitive) {
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
			unix := int(data.unix())
			stmt_bind_primitive(mut stmt, unix)
		}
		orm.InfixType {
			stmt_bind_primitive(mut stmt, data.right)
		}
		orm.Null {
			stmt.bind_null()
		}
		[]orm.Primitive {
			for element in data {
				stmt_bind_primitive(mut stmt, element)
			}
		}
	}
}

// data_pointers_to_primitives returns an array of `Primitive`
// cast from `data_pointers` using `types`.
fn data_pointers_to_primitives(is_null []bool, data_pointers []&u8, types []int, field_types []FieldType) ![]orm.Primitive {
	mut result := []orm.Primitive{}

	for i, data in data_pointers {
		mut primitive := orm.Primitive(0)
		if !is_null[i] {
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
				orm.type_string {
					primitive = unsafe { cstring_to_vstring(&char(data)) }
				}
				orm.time_ {
					match field_types[i] {
						.type_long {
							timestamp := *(unsafe { &int(data) })
							primitive = time.unix(timestamp)
						}
						.type_datetime, .type_timestamp {
							primitive = time.parse(unsafe { cstring_to_vstring(&char(data)) })!
						}
						else {}
					}
				}
				orm.enum_ {
					primitive = *(unsafe { &i64(data) })
				}
				else {
					return error('Unknown type ${types[i]}')
				}
			}
		} else {
			primitive = orm.Null{}
		}
		result << primitive
	}

	return result
}

// mysql_type_from_v converts the V type to the corresponding MySQL type.
fn mysql_type_from_v(typ int) !string {
	sql_type := match typ {
		orm.type_idx['i8'], orm.type_idx['u8'] {
			'TINYINT'
		}
		orm.type_idx['i16'], orm.type_idx['u16'] {
			'SMALLINT'
		}
		orm.type_idx['int'], orm.type_idx['u32'], orm.time_ {
			'INT'
		}
		orm.type_idx['i64'], orm.type_idx['u64'], orm.enum_ {
			'BIGINT'
		}
		orm.type_idx['f32'] {
			'FLOAT'
		}
		orm.type_idx['f64'] {
			'DOUBLE'
		}
		orm.type_string {
			'TEXT'
		}
		orm.serial {
			'SERIAL'
		}
		orm.type_idx['bool'] {
			'BOOLEAN'
		}
		else {
			return error('Unknown type ${typ}')
		}
	}

	return sql_type
}

// convert_query_data_to_primitives converts the `data` representing the `QueryData`
// into an array of `Primitive`.
fn (db DB) convert_query_data_to_primitives(table string, data orm.QueryData) ![]orm.Primitive {
	mut column_type_map := db.get_table_column_type_map(table)!
	mut converted_data := []orm.Primitive{}

	for i, field in data.fields {
		if data.data[i].type_name() == 'time.Time' {
			if column_type_map[field] in ['datetime', 'timestamp'] {
				converted_data << orm.Primitive((data.data[i] as time.Time).str())
			} else {
				converted_data << data.data[i]
			}
		} else {
			converted_data << data.data[i]
		}
	}

	return converted_data
}

// get_table_column_type_map returns a map where the key represents the column name,
// and the value represents its data type.
fn (db DB) get_table_column_type_map(table string) !map[string]string {
	data_type_query := "SELECT COLUMN_NAME, DATA_TYPE FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '${table}'"
	mut column_type_map := map[string]string{}
	results := db.query(data_type_query)!

	db.use_result()

	for row in results.rows() {
		column_type_map[row.vals[0]] = row.vals[1]
	}

	unsafe { results.free() }

	return column_type_map
}
