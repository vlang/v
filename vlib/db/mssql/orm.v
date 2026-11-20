module mssql

import orm
import strconv
import time

// select is used internally by V's ORM for processing `SELECT` queries.
pub fn (mut conn Connection) select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	where_with_tenant := orm.apply_tenant_filter(config.table, where)
	query := orm.orm_select_gen_with_dialect(.mssql, config, '"', false, '?', 1, where_with_tenant)

	// T-SQL emits OFFSET before FETCH NEXT, while the query builder appends the
	// limit value before the offset value, so swap the last two bound values.
	params := mssql_bind(where_with_tenant)
	params << mssql_select_data_params(config, data)

	res := conn.exec(query, params)!
	mut ret := [][]orm.Primitive{}
	for row in res.rows {
		mut row_data := []orm.Primitive{}
		for i, typ in config.types {
			if i >= row.vals.len {
				break
			}
			if row.is_null(i) {
				row_data << orm.Null{}
			} else {
				row_data << mssql_val_to_primitive(row.val(i), typ)!
			}
		}
		ret << row_data
	}
	return ret
}

// insert is used internally by V's ORM for processing `INSERT` queries.
pub fn (mut conn Connection) insert(table orm.Table, data orm.QueryData) ! {
	query, converted_data :=
		orm.orm_stmt_gen(.mssql, table, '"', .insert, false, '?', 1, data, orm.QueryData{})
	conn.exec(query, mssql_bind(converted_data))!
}

// update is used internally by V's ORM for processing `UPDATE` queries.
pub fn (mut conn Connection) update(table orm.Table, data orm.QueryData, where orm.QueryData) ! {
	where_with_tenant := orm.apply_tenant_filter(table, where)
	query, _ := orm.orm_stmt_gen(.mssql, table, '"', .update, false, '?', 1, data,
		where_with_tenant)
	mut params := mssql_bind(data)
	params << mssql_bind(where_with_tenant)
	conn.exec(query, params)!
}

// delete is used internally by V's ORM for processing `DELETE` queries.
pub fn (mut conn Connection) delete(table orm.Table, where orm.QueryData) ! {
	where_with_tenant := orm.apply_tenant_filter(table, where)
	query, _ := orm.orm_stmt_gen(.mssql, table, '"', .delete, false, '?', 1, orm.QueryData{},
		where_with_tenant)
	conn.exec(query, mssql_bind(where_with_tenant))!
}

// create is used internally by V's ORM for table creation.
pub fn (mut conn Connection) create(table orm.Table, fields []orm.TableField) ! {
	query := orm.orm_table_gen(.mssql, table, '"', true, 0, fields, mssql_type_from_v, true) or {
		return err
	}
	for stmt in query.split(';') {
		if stmt.trim_space() != '' {
			conn.query('${stmt};')!
		}
	}
}

// drop is used internally by V's ORM for table destruction.
pub fn (mut conn Connection) drop(table orm.Table) ! {
	conn.query('DROP TABLE "${table.name}";')!
}

// execute runs a raw SQL query and returns driver-agnostic rows with column names.
pub fn (mut conn Connection) execute(query string) ![]orm.Row {
	res := conn.query(query)!
	mut rows := []orm.Row{cap: res.rows.len}
	for r in res.rows {
		mut vals := []string{cap: r.vals.len}
		for i in 0 .. r.vals.len {
			vals << if r.is_null(i) { '' } else { r.val(i) }
		}
		rows << orm.Row{
			vals:  vals
			names: res.names
		}
	}
	return rows
}

// orm_begin starts a transaction on this connection.
pub fn (mut conn Connection) orm_begin() ! {
	conn.query('BEGIN TRANSACTION')!
}

// orm_commit commits the current transaction.
pub fn (mut conn Connection) orm_commit() ! {
	conn.query('COMMIT TRANSACTION')!
}

// orm_rollback rolls back the current transaction.
pub fn (mut conn Connection) orm_rollback() ! {
	conn.query('ROLLBACK TRANSACTION')!
}

// orm_savepoint creates a savepoint on this connection.
pub fn (mut conn Connection) orm_savepoint(name string) ! {
	check_savepoint_name(name)!
	conn.query('SAVE TRANSACTION ${name}')!
}

// orm_rollback_to rolls back to a savepoint on this connection.
pub fn (mut conn Connection) orm_rollback_to(name string) ! {
	check_savepoint_name(name)!
	conn.query('ROLLBACK TRANSACTION ${name}')!
}

// orm_release_savepoint is unsupported: SQL Server has no RELEASE SAVEPOINT.
pub fn (mut conn Connection) orm_release_savepoint(name string) ! {
	return error('db.mssql: SQL Server does not support releasing savepoints: ${name}')
}

fn check_savepoint_name(name string) ! {
	if name.len == 0 || name.len > 32 {
		return error('db.mssql: invalid savepoint name: ${name}')
	}
	for i, c in name {
		valid := if i == 0 {
			c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
		} else {
			c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
		}
		if !valid {
			return error('db.mssql: invalid savepoint name: ${name}')
		}
	}
}

fn mssql_select_data_params(config orm.SelectConfig, data orm.QueryData) []?string {
	mut params := mssql_bind(data)
	if config.has_limit && config.has_offset && params.len >= 2 {
		last := params.len - 1
		tmp := params[last]
		params[last] = params[last - 1]
		params[last - 1] = tmp
	}
	return params
}

fn mssql_bind(d orm.QueryData) []?string {
	mut out := []?string{}
	for p in d.data {
		out << mssql_primitive_params(p)
	}
	return out
}

fn mssql_primitive_params(p orm.Primitive) []?string {
	match p {
		orm.Null {
			return [?string(none)]
		}
		bool {
			return [if p { '1' } else { '0' }]
		}
		i8, i16, int, i64, u8, u16, u32, u64, f32, f64 {
			return ['${p}']
		}
		string {
			return [p]
		}
		time.Time {
			return [p.unix().str()]
		}
		orm.InfixType {
			return mssql_primitive_params(*p.right)
		}
		[]bool {
			return mssql_param_array(p)
		}
		[]f32 {
			return mssql_param_array(p)
		}
		[]f64 {
			return mssql_param_array(p)
		}
		[]i8 {
			return mssql_param_array(p)
		}
		[]i16 {
			return mssql_param_array(p)
		}
		[]int {
			return mssql_param_array(p)
		}
		[]i64 {
			return mssql_param_array(p)
		}
		[]u8 {
			return mssql_param_array(p)
		}
		[]u16 {
			return mssql_param_array(p)
		}
		[]u32 {
			return mssql_param_array(p)
		}
		[]u64 {
			return mssql_param_array(p)
		}
		[]string {
			return mssql_param_array(p)
		}
		[]time.Time {
			mut out := []?string{cap: p.len}
			for item in p {
				out << item.unix().str()
			}
			return out
		}
		[]orm.Primitive {
			mut out := []?string{}
			for item in p {
				out << mssql_primitive_params(item)
			}
			return out
		}
		[]orm.InfixType {
			mut out := []?string{}
			for item in p {
				out << mssql_primitive_params(*item.right)
			}
			return out
		}
	}
	return []?string{}
}

fn mssql_param_array[T](items []T) []?string {
	mut out := []?string{cap: items.len}
	for item in items {
		out << '${item}'
	}
	return out
}

fn mssql_val_to_primitive(val string, typ int) !orm.Primitive {
	match typ {
		orm.type_idx['bool'] {
			return orm.Primitive(val == '1' || val.to_lower() == 'true')
		}
		orm.type_idx['i8'] {
			return orm.Primitive(val.i8())
		}
		orm.type_idx['i16'] {
			return orm.Primitive(val.i16())
		}
		orm.type_idx['int'] {
			return orm.Primitive(val.int())
		}
		orm.type_idx['i64'] {
			return orm.Primitive(val.i64())
		}
		orm.type_idx['u8'] {
			return orm.Primitive(val.u8())
		}
		orm.type_idx['u16'] {
			return orm.Primitive(val.u16())
		}
		orm.type_idx['u32'] {
			return orm.Primitive(val.u32())
		}
		orm.type_idx['u64'] {
			return orm.Primitive(val.u64())
		}
		orm.type_idx['f32'] {
			return orm.Primitive(val.f32())
		}
		orm.type_idx['f64'] {
			return orm.Primitive(val.f64())
		}
		orm.type_string {
			return orm.Primitive(val)
		}
		orm.time_ {
			return orm.Primitive(time.unix(val.i64()))
		}
		orm.enum_ {
			if number := strconv.atoi64(val.trim_space()) {
				return orm.Primitive(number)
			}
			return orm.Primitive(val)
		}
		else {}
	}
	return error('Unknown field type ${typ}')
}

fn mssql_type_from_v(typ int) !string {
	str := match typ {
		orm.type_idx['bool'] {
			'BIT'
		}
		orm.type_idx['i8'], orm.type_idx['i16'], orm.type_idx['u8'] {
			'SMALLINT'
		}
		orm.type_idx['int'], orm.type_idx['u16'], orm.type_idx['u32'] {
			'INT'
		}
		orm.type_idx['i64'], orm.type_idx['u64'], orm.time_, orm.enum_ {
			'BIGINT'
		}
		orm.float[0] {
			'REAL'
		}
		orm.float[1] {
			'FLOAT'
		}
		orm.type_string {
			'NVARCHAR(450)'
		}
		orm.serial {
			'INT IDENTITY(1,1)'
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
