module oracle

import orm
import strconv
import time

// select is used internally by V's ORM for processing `SELECT` queries.
pub fn (mut db Connection) select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	where_with_tenant := orm.apply_tenant_filter(config.table, where)
	query := oracle_select_query(config, where_with_tenant)
	mut params := []Param{}
	for v in where_with_tenant.data {
		append_params(mut params, v)
	}
	// The rewritten tail is `OFFSET ... ROWS FETCH FIRST ... ROWS ONLY`, and
	// OCI binds by textual position. Bind the offset value before the limit
	// value so the positional binds match the statement (e.g. [20, 10] for
	// limit 10 / offset 20), mirroring the SQL produced by oracle_select_query.
	if config.has_limit && config.has_offset && data.data.len >= 2 {
		append_params(mut params, data.data[1]) // OFFSET
		append_params(mut params, data.data[0]) // FETCH FIRST (LIMIT)
	} else {
		for v in data.data {
			append_params(mut params, v)
		}
	}
	res := db.exec(query, params)!

	mut ret := [][]orm.Primitive{}
	for row in res.rows {
		mut values := []orm.Primitive{}
		for i, typ in config.types {
			if i < row.nulls.len && row.nulls[i] {
				values << orm.Primitive(orm.Null{})
				continue
			}
			values << primitive_from_text(if i < row.vals.len { row.vals[i] } else { '' }, typ)
		}
		ret << values
	}
	return ret
}

// insert is used internally by V's ORM for processing `INSERT` queries.
pub fn (mut db Connection) insert(table orm.Table, data orm.QueryData) ! {
	query, converted := orm.orm_stmt_gen(.default, table, '"', .insert, true, ':', 1, data,
		orm.QueryData{})
	mut params := []Param{}
	for v in converted.data {
		append_params(mut params, v)
	}
	db.exec(query, params)!
	// Remember which table was inserted into so last_id() resolves that
	// table's sequence rather than the last-created table's.
	db.last_insert_table = table.name
}

// update is used internally by V's ORM for processing `UPDATE` queries.
pub fn (mut db Connection) update(table orm.Table, data orm.QueryData, where orm.QueryData) ! {
	where_with_tenant := orm.apply_tenant_filter(table, where)
	query, _ := orm.orm_stmt_gen(.default, table, '"', .update, true, ':', 1, data, where_with_tenant)
	mut params := []Param{}
	for v in data.data {
		append_params(mut params, v)
	}
	for v in where_with_tenant.data {
		append_params(mut params, v)
	}
	db.exec(query, params)!
}

// delete is used internally by V's ORM for processing `DELETE` queries.
pub fn (mut db Connection) delete(table orm.Table, where orm.QueryData) ! {
	where_with_tenant := orm.apply_tenant_filter(table, where)
	query, _ := orm.orm_stmt_gen(.default, table, '"', .delete, true, ':', 1, orm.QueryData{},
		where_with_tenant)
	mut params := []Param{}
	for v in where_with_tenant.data {
		append_params(mut params, v)
	}
	db.exec(query, params)!
}

// create is used internally by V's ORM for table creation (DDL). Oracle has no
// `CREATE TABLE IF NOT EXISTS`, so that clause is stripped. `serial` fields are
// backed by a named sequence used as the column default; the sequence name is
// remembered so `last_id()` can read its CURRVAL.
pub fn (mut db Connection) create(table orm.Table, fields []orm.TableField) ! {
	mut sequence := ''
	for f in fields {
		if field_is_serial(f) {
			sequence = serial_sequence_name(table.name, f.name)
			_ = db.exec('CREATE SEQUENCE "${sequence}" START WITH 1 INCREMENT BY 1 NOCACHE', []) or {
				// Reuse an existing sequence when the table already exists.
				if !err.msg().contains('ORA-00955') {
					return err
				}
				Result{}
			}
			db.sequences[table.name] = sequence
		}
	}

	mut query := orm.orm_table_gen(.default, table, '"', false, 0, fields, oracle_type_from_v, false) or {
		return err
	}
	query = query.replace('CREATE TABLE IF NOT EXISTS ', 'CREATE TABLE ')
	if sequence != '' {
		for f in fields {
			if field_is_serial(f) {
				needle := '"${f.name}" NUMBER'
				replacement := '"${f.name}" NUMBER DEFAULT "${sequence}".NEXTVAL'
				query = query.replace(needle, replacement)
				break
			}
		}
	}
	for stmt in query.split(';') {
		if stmt.trim_space() != '' {
			_ = db.exec(stmt + ';', []) or {
				if !err.msg().contains('ORA-00955') {
					return err
				}
				Result{}
			}
		}
	}
}

// drop is used internally by V's ORM for table destroying queries (DDL).
// Only the sequence belonging to the dropped table is removed; sequences of
// other tables stay recorded so last_id() keeps resolving correctly.
pub fn (mut db Connection) drop(table orm.Table) ! {
	db.exec('DROP TABLE "${table.name}"', [])!
	sequence := db.sequences[table.name]
	if sequence != '' {
		_ = db.exec('DROP SEQUENCE "${sequence}"', []) or { Result{} }
		db.sequences.delete(table.name)
	}
}

// execute runs a raw SQL query and returns driver-agnostic orm.Row values.
pub fn (mut db Connection) execute(query string) ![]orm.Row {
	res := db.exec(query, [])!
	mut rows := []orm.Row{}
	for r in res.rows {
		rows << orm.Row{
			vals:  r.vals
			names: res.names
		}
	}
	return rows
}

// orm_begin starts a transaction for ORM helpers.
pub fn (mut db Connection) orm_begin() ! {
	db.begin()!
}

// orm_commit commits a transaction for ORM helpers.
pub fn (mut db Connection) orm_commit() ! {
	db.commit()!
}

// orm_rollback rolls back a transaction for ORM helpers.
pub fn (mut db Connection) orm_rollback() ! {
	db.rollback()!
}

// orm_savepoint creates a savepoint for ORM helpers.
pub fn (mut db Connection) orm_savepoint(name string) ! {
	db.savepoint(name)!
}

// orm_rollback_to rolls back to a savepoint for ORM helpers.
pub fn (mut db Connection) orm_rollback_to(name string) ! {
	db.rollback_to(name)!
}

// orm_release_savepoint releases a savepoint for ORM helpers.
pub fn (mut db Connection) orm_release_savepoint(name string) ! {
	db.release_savepoint(name)!
}

// oracle_select_query rewrites the `.default` dialect LIMIT/OFFSET tail into
// the standard syntax Oracle uses (`OFFSET ... ROWS FETCH FIRST ... ROWS ONLY`).
fn oracle_select_query(config orm.SelectConfig, where orm.QueryData) string {
	q := orm.orm_select_gen(config, '"', true, ':', 1, where)
	if !config.has_limit {
		return q
	}
	mut s := q.trim_space()
	if s.ends_with(';') {
		s = s[..s.len - 1]
	}
	idx := s.index(' LIMIT ') or { return q }
	base := s[..idx]
	rest := s[idx + ' LIMIT '.len..]
	if config.has_offset {
		parts := rest.split(' OFFSET ')
		if parts.len == 2 {
			return '${base} OFFSET ${parts[1]} ROWS FETCH FIRST ${parts[0]} ROWS ONLY;'
		}
	}
	return '${base} FETCH FIRST ${rest} ROWS ONLY;'
}

fn field_is_serial(f orm.TableField) bool {
	if f.typ == orm.serial {
		return true
	}
	for a in f.attrs {
		if a.name == 'serial' {
			return true
		}
		if a.name == 'sql' && a.arg.to_lower() == 'serial' {
			return true
		}
	}
	return false
}

fn serial_sequence_name(table string, field string) string {
	return '${table}_${field}_SEQ'
}

fn oracle_type_from_v(typ int) !string {
	if typ in orm.nums || typ in orm.num64 || typ in orm.float
		|| typ in [orm.serial, orm.time_, orm.enum_] {
		return 'NUMBER'
	}
	if typ == orm.type_string {
		return 'VARCHAR2(4000)'
	}
	return error('oracle: unsupported field type ${typ}')
}

fn append_params(mut params []Param, v orm.Primitive) {
	match v {
		orm.Null {
			params << Param{ value: '', is_null: true }
		}
		bool, int, i8, i16, u8, u16, u32 {
			params << Param{ value: int(v).str() }
		}
		i64, u64 {
			params << Param{ value: i64(v).str() }
		}
		f32, f64 {
			params << Param{ value: f64(v).str() }
		}
		string {
			params << Param{ value: v }
		}
		time.Time {
			params << Param{ value: v.unix().str() }
		}
		orm.InfixType {
			append_params(mut params, v.right)
		}
		[]orm.Primitive {
			for e in v {
				append_params(mut params, e)
			}
		}
		[]bool {
			append_array(mut params, v)
		}
		[]f32 {
			append_array(mut params, v)
		}
		[]f64 {
			append_array(mut params, v)
		}
		[]i8 {
			append_array(mut params, v)
		}
		[]i16 {
			append_array(mut params, v)
		}
		[]i64 {
			append_array(mut params, v)
		}
		[]int {
			append_array(mut params, v)
		}
		[]string {
			append_array(mut params, v)
		}
		[]time.Time {
			append_array(mut params, v)
		}
		[]u8 {
			append_array(mut params, v)
		}
		[]u16 {
			append_array(mut params, v)
		}
		[]u32 {
			append_array(mut params, v)
		}
		[]u64 {
			append_array(mut params, v)
		}
		[]orm.InfixType {
			append_array(mut params, v)
		}
	}
}

fn append_array[T](mut params []Param, values []T) {
	for v in values {
		append_params(mut params, orm.Primitive(v))
	}
}

fn primitive_from_text(s string, typ int) orm.Primitive {
	if typ in orm.nums {
		return orm.Primitive(s.int())
	}
	if typ in orm.num64 {
		return orm.Primitive(s.i64())
	}
	if typ in orm.float {
		return orm.Primitive(s.f64())
	}
	if typ == orm.time_ {
		return orm.Primitive(time.unix(s.i64()))
	}
	if typ == orm.enum_ {
		if n := strconv.atoi(s) {
			return orm.Primitive(n)
		}
		return orm.Primitive(s)
	}
	return orm.Primitive(s)
}
