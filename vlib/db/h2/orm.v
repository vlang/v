module h2

import orm
import strconv
import time

// select is used internally by V's ORM for `SELECT` queries.
pub fn (db H2Connection) select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	where_with_tenant := orm.apply_tenant_filter(config.table, where)
	query := orm.orm_select_gen(config, '"', true, '?', 1, where_with_tenant)
	mut params := []string{}
	mut nulls := []bool{}
	bind_where_data(where_with_tenant, mut params, mut nulls)
	bind_data(data, mut params, mut nulls)
	res := db.exec_nullable(query, params, nulls)!

	mut ret := [][]orm.Primitive{}
	for row_idx, row in res.vals {
		mut row_data := []orm.Primitive{}
		for col, val in row {
			typ := if col < config.types.len { config.types[col] } else { -999 }
			row_data << primitive_from_string(val, res.nulls[row_idx][col], typ)
		}
		ret << row_data
	}
	return ret
}

// insert is used internally by V's ORM for `INSERT` queries.
pub fn (db H2Connection) insert(table orm.Table, data orm.QueryData) ! {
	query, converted_data := orm.orm_stmt_gen(.h2, table, '"', .insert, true, '?', 1, data,
		orm.QueryData{})
	mut params := []string{}
	mut nulls := []bool{}
	bind_data(converted_data, mut params, mut nulls)
	db.exec_nullable(query, params, nulls)!
}

// update is used internally by V's ORM for `UPDATE` queries.
pub fn (db H2Connection) update(table orm.Table, data orm.QueryData, where orm.QueryData) ! {
	where_with_tenant := orm.apply_tenant_filter(table, where)
	query, _ := orm.orm_stmt_gen(.h2, table, '"', .update, true, '?', 1, data, where_with_tenant)
	mut params := []string{}
	mut nulls := []bool{}
	bind_data(data, mut params, mut nulls)
	bind_where_data(where_with_tenant, mut params, mut nulls)
	db.exec_nullable(query, params, nulls)!
}

// delete is used internally by V's ORM for `DELETE` queries.
pub fn (db H2Connection) delete(table orm.Table, where orm.QueryData) ! {
	where_with_tenant := orm.apply_tenant_filter(table, where)
	query, _ := orm.orm_stmt_gen(.h2, table, '"', .delete, true, '?', 1, orm.QueryData{},
		where_with_tenant)
	mut params := []string{}
	mut nulls := []bool{}
	bind_where_data(where_with_tenant, mut params, mut nulls)
	db.exec_nullable(query, params, nulls)!
}

// create is used internally by V's ORM for table creation queries (DDL).
pub fn (db H2Connection) create(table orm.Table, fields []orm.TableField) ! {
	query := orm.orm_table_gen(.h2, table, '"', true, 0, fields, h2_type_from_v, false) or {
		return err
	}
	for stmt in query.split(';') {
		if stmt.trim_space() != '' {
			db.exec_nullable('${stmt.trim_space()};', [], [])!
		}
	}
}

// drop is used internally by V's ORM for table destroying queries (DDL).
pub fn (db H2Connection) drop(table orm.Table) ! {
	db.exec_nullable('DROP TABLE "${table.name}";', [], [])!
}

// execute runs a raw SQL query and returns driver-agnostic rows, with column
// names populated from the JDBC result metadata. SQL NULL becomes ''.
pub fn (db H2Connection) execute(query string) ![]orm.Row {
	res := db.exec(query, [], [])!
	mut rows := []orm.Row{}
	for row in res.vals {
		rows << orm.Row{
			vals:  row.clone()
			names: res.names.clone()
		}
	}
	return rows
}

// orm_begin starts a transaction.
pub fn (db H2Connection) orm_begin() ! {
	db.exec_nullable('BEGIN', [], [])!
}

// orm_commit commits a transaction.
pub fn (db H2Connection) orm_commit() ! {
	db.exec_nullable('COMMIT', [], [])!
}

// orm_rollback rolls back a transaction.
pub fn (db H2Connection) orm_rollback() ! {
	db.exec_nullable('ROLLBACK', [], [])!
}

// orm_savepoint creates a savepoint.
pub fn (db H2Connection) orm_savepoint(name string) ! {
	db.exec_nullable('SAVEPOINT "${name}"', [], [])!
}

// orm_rollback_to rolls back to a savepoint.
pub fn (db H2Connection) orm_rollback_to(name string) ! {
	db.exec_nullable('ROLLBACK TO SAVEPOINT "${name}"', [], [])!
}

// orm_release_savepoint releases a savepoint.
pub fn (db H2Connection) orm_release_savepoint(name string) ! {
	db.exec_nullable('RELEASE SAVEPOINT "${name}"', [], [])!
}

// ---- helpers ----

// bind_where_data skips the values of unary predicates (IS NULL / IS NOT NULL),
// which do not consume a JDBC placeholder.
fn bind_where_data(d orm.QueryData, mut params []string, mut nulls []bool) {
	mut idx := 0
	for kind in d.kinds {
		if kind in [.is_null, .is_not_null] {
			continue
		}
		if idx < d.data.len {
			bind_primitive(d.data[idx], mut params, mut nulls)
		}
		idx++
	}
}

fn bind_data(d orm.QueryData, mut params []string, mut nulls []bool) {
	for value in d.data {
		bind_primitive(value, mut params, mut nulls)
	}
}

fn bind_primitive(value orm.Primitive, mut params []string, mut nulls []bool) {
	match value {
		orm.Null {
			params << ''
			nulls << true
		}
		bool {
			params << if value { 'true' } else { 'false' }
			nulls << false
		}
		int, i8, i16, i64, u8, u16, u32, u64, f32, f64 {
			params << value.str()
			nulls << false
		}
		string {
			params << value
			nulls << false
		}
		time.Time {
			params << value.unix().str()
			nulls << false
		}
		orm.InfixType {
			bind_primitive(value.right, mut params, mut nulls)
		}
		[]orm.Primitive {
			for element in value {
				bind_primitive(element, mut params, mut nulls)
			}
		}
		[]bool {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
		[]int {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
		[]i8 {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
		[]i16 {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
		[]i64 {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
		[]u8 {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
		[]u16 {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
		[]u32 {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
		[]u64 {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
		[]f32 {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
		[]f64 {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
		[]string {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
		[]time.Time {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
		[]orm.InfixType {
			for element in value {
				bind_primitive(orm.Primitive(element), mut params, mut nulls)
			}
		}
	}
}

fn primitive_from_string(val string, is_null bool, typ int) orm.Primitive {
	if is_null {
		return orm.Null{}
	}
	if typ == orm.serial || typ in orm.nums {
		return conv_int(val)
	}
	if typ in orm.num64 {
		return conv_i64(val)
	}
	if typ == typeof[f32]().idx {
		return f32(val.f64())
	}
	if typ == typeof[f64]().idx {
		return val.f64()
	}
	if typ == orm.type_string {
		return val.clone()
	}
	if typ == orm.time_ {
		epoch := val.i64()
		if epoch == 0 && val != '0' {
			return orm.Null{}
		}
		return time.unix(epoch)
	}
	if typ == orm.enum_ {
		return conv_i64(val)
	}
	// Unknown type: infer from the text (covers COUNT(*) on a plain select).
	return infer_primitive(val)
}

fn conv_int(val string) orm.Primitive {
	return int(val.i64())
}

fn conv_i64(val string) orm.Primitive {
	return val.i64()
}

fn infer_primitive(val string) orm.Primitive {
	if n := strconv.parse_int(val, 10, 64) {
		return n
	} else {
		if f := strconv.atof64(val, strconv.AtoF64Param{}) {
			return f
		}
	}
	return val.clone()
}

// h2_type_from_v maps V type indices to H2 column types.
fn h2_type_from_v(typ int) !string {
	if typ == orm.serial {
		return 'BIGINT AUTO_INCREMENT'
	}
	if typ in orm.nums || typ in orm.num64 || typ in [orm.time_, orm.enum_] {
		return 'BIGINT'
	}
	if typ in orm.float {
		return 'DOUBLE'
	}
	if typ == orm.type_string {
		return 'VARCHAR'
	}
	return error('h2: unknown V type ${typ}')
}
