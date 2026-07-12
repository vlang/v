module pg

import orm
import time
import strconv
import net.conv

// ---- ORM on Conn (single pinned connection) ----

// select is used internally by V's ORM for processing `SELECT ` queries.
pub fn (c &Conn) select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	c.ensure_active()!
	where_with_tenant := orm.apply_tenant_filter(config.table, where)
	query := orm.orm_select_gen(config, '"', true, '$', 1, where_with_tenant)

	rows := pg_stmt_worker(c, query, where_with_tenant, data)!

	mut ret := [][]orm.Primitive{}

	for row in rows {
		mut row_data := []orm.Primitive{}
		for i, val in row.vals {
			row_data << val_to_primitive(val, config.types[i])!
		}
		ret << row_data
	}

	return ret
}

// insert is used internally by V's ORM for processing `INSERT ` queries.
pub fn (c &Conn) insert(table orm.Table, data orm.QueryData) ! {
	c.ensure_active()!
	query, converted_data :=
		orm.orm_stmt_gen(.pg, table, '"', .insert, true, '$', 1, data, orm.QueryData{})
	pg_stmt_worker(c, query, converted_data, orm.QueryData{})!
}

// update is used internally by V's ORM for processing `UPDATE ` queries.
pub fn (c &Conn) update(table orm.Table, data orm.QueryData, where orm.QueryData) ! {
	c.ensure_active()!
	where_with_tenant := orm.apply_tenant_filter(table, where)
	query, _ := orm.orm_stmt_gen(.default, table, '"', .update, true, '$', 1, data,
		where_with_tenant)
	pg_stmt_worker(c, query, data, where_with_tenant)!
}

// delete is used internally by V's ORM for processing `DELETE ` queries.
pub fn (c &Conn) delete(table orm.Table, where orm.QueryData) ! {
	c.ensure_active()!
	where_with_tenant := orm.apply_tenant_filter(table, where)
	query, _ := orm.orm_stmt_gen(.default, table, '"', .delete, true, '$', 1, orm.QueryData{},
		where_with_tenant)
	pg_stmt_worker(c, query, orm.QueryData{}, where_with_tenant)!
}

// last_id is used internally by V's ORM for post-processing `INSERT ` queries.
pub fn (c &Conn) last_id() int {
	query := 'SELECT LASTVAL();'

	return c.q_int(query) or { 0 }
}

// create is used internally by V's ORM for processing table creation queries (DDL).
pub fn (c &Conn) create(table orm.Table, fields []orm.TableField) ! {
	query := orm.orm_table_gen(.pg, table, '"', true, 0, fields, pg_type_from_v, false) or {
		return err
	}
	stmts := query.split(';')
	for stmt in stmts {
		if stmt != '' {
			c.exec(stmt + ';')!
		}
	}
}

// drop is used internally by V's ORM for processing table destroying queries (DDL).
pub fn (c &Conn) drop(table orm.Table) ! {
	query := 'DROP TABLE "${table.name}";'
	c.exec(query)!
}

// execute runs a raw SQL query and returns result rows as driver-agnostic orm.Row values,
// with column names populated from the result metadata.
pub fn (c &Conn) execute(query string) ![]orm.Row {
	res := c.exec_result(query)!

	mut orm_rows := []orm.Row{}
	for r in res.rows {
		mut vals := []string{}
		for i in 0 .. r.vals.len {
			vals << r.val(i)
		}
		orm_rows << orm.Row{
			vals:  vals
			names: res.names
		}
	}
	return orm_rows
}

// orm_begin starts a transaction on this conn.
pub fn (c &Conn) orm_begin() ! {
	c.begin_on_conn()!
}

// orm_commit commits the transaction on this conn.
pub fn (c &Conn) orm_commit() ! {
	c.commit()!
}

// orm_rollback rolls back the transaction on this conn.
pub fn (c &Conn) orm_rollback() ! {
	c.rollback()!
}

// orm_savepoint creates a savepoint on this conn.
pub fn (c &Conn) orm_savepoint(name string) ! {
	c.savepoint(name)!
}

// orm_rollback_to rolls back to a savepoint on this conn.
pub fn (c &Conn) orm_rollback_to(name string) ! {
	c.rollback_to(name)!
}

// orm_release_savepoint releases a savepoint on this conn.
pub fn (c &Conn) orm_release_savepoint(name string) ! {
	c.release_savepoint(name)!
}

// ---- ORM on DB (acquire-use-release per call) ----

// select acquires a conn from the pool and runs the ORM SELECT on it.
pub fn (mut db DB) select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	mut c := db.pool.acquire()!
	defer {
		c.close() or {}
	}
	return c.select(config, data, where)
}

// insert acquires a conn from the pool, runs the ORM INSERT on it, and
// stashes LASTVAL() captured on the same session for the calling thread.
// V's `sql db { insert ... }` macro emits a follow-up `db.last_id()` call;
// stashing here lets that read return the correct id even though the pool
// may hand out a different conn for the second call.
pub fn (mut db DB) insert(table orm.Table, data orm.QueryData) ! {
	mut c := db.pool.acquire()!
	defer {
		c.close() or {}
	}
	c.insert(table, data)!
	db.pool.stash_last_id(c.last_id())
}

// update acquires a conn from the pool and runs the ORM UPDATE on it.
pub fn (mut db DB) update(table orm.Table, data orm.QueryData, where orm.QueryData) ! {
	mut c := db.pool.acquire()!
	defer {
		c.close() or {}
	}
	c.update(table, data, where)!
}

// delete acquires a conn from the pool and runs the ORM DELETE on it.
pub fn (mut db DB) delete(table orm.Table, where orm.QueryData) ! {
	mut c := db.pool.acquire()!
	defer {
		c.close() or {}
	}
	c.delete(table, where)!
}

// create acquires a conn from the pool and runs CREATE TABLE on it.
pub fn (mut db DB) create(table orm.Table, fields []orm.TableField) ! {
	mut c := db.pool.acquire()!
	defer {
		c.close() or {}
	}
	c.create(table, fields)!
}

// drop acquires a conn from the pool and runs DROP TABLE on it.
pub fn (mut db DB) drop(table orm.Table) ! {
	mut c := db.pool.acquire()!
	defer {
		c.close() or {}
	}
	c.drop(table)!
}

// execute runs a raw SQL query on a pooled conn and returns result rows.
pub fn (mut db DB) execute(query string) ![]orm.Row {
	mut c := db.pool.acquire()!
	defer { c.close() or {} }
	return c.execute(query)
}

// last_id returns the id stashed by this thread's most recent `DB.insert`
// (or 0 if there is none). LASTVAL() itself is session-scoped, so calling
// it on a freshly-checked-out pool conn would return the wrong value or 0;
// `DB.insert` captures it on the same conn that ran the INSERT and stashes
// it per-thread, which is what V's ORM macro expects.
pub fn (mut db DB) last_id() int {
	return db.pool.take_last_id()
}

// ---- ORM on Tx (use the pinned conn) ----

// select runs the ORM SELECT on the pinned transaction conn.
pub fn (mut tx Tx) select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	tx.ensure_active()!
	return tx.conn.select(config, data, where)
}

// insert runs the ORM INSERT on the pinned transaction conn.
pub fn (mut tx Tx) insert(table orm.Table, data orm.QueryData) ! {
	tx.ensure_active()!
	tx.conn.insert(table, data)!
}

// update runs the ORM UPDATE on the pinned transaction conn.
pub fn (mut tx Tx) update(table orm.Table, data orm.QueryData, where orm.QueryData) ! {
	tx.ensure_active()!
	tx.conn.update(table, data, where)!
}

// delete runs the ORM DELETE on the pinned transaction conn.
pub fn (mut tx Tx) delete(table orm.Table, where orm.QueryData) ! {
	tx.ensure_active()!
	tx.conn.delete(table, where)!
}

// create runs CREATE TABLE on the pinned transaction conn.
pub fn (mut tx Tx) create(table orm.Table, fields []orm.TableField) ! {
	tx.ensure_active()!
	tx.conn.create(table, fields)!
}

// drop runs DROP TABLE on the pinned transaction conn.
pub fn (mut tx Tx) drop(table orm.Table) ! {
	tx.ensure_active()!
	tx.conn.drop(table)!
}

// execute runs a raw SQL query on the pinned transaction conn and returns result rows.
pub fn (mut tx Tx) execute(query string) ![]orm.Row {
	tx.ensure_active()!
	return tx.conn.execute(query)
}

// last_id returns the last inserted id on the pinned conn.
pub fn (mut tx Tx) last_id() int {
	if tx.done || isnil(tx.conn) {
		return 0
	}
	return tx.conn.last_id()
}

// orm_begin is a no-op on Tx (begin already ran when the Tx was created).
// It exists so Tx satisfies `orm.TransactionalConnection` for nested savepoints.
pub fn (mut tx Tx) orm_begin() ! {
}

// orm_commit commits the Tx.
pub fn (mut tx Tx) orm_commit() ! {
	tx.commit()!
}

// orm_rollback rolls back the Tx.
pub fn (mut tx Tx) orm_rollback() ! {
	tx.rollback()!
}

// orm_savepoint creates a savepoint inside the Tx.
pub fn (mut tx Tx) orm_savepoint(name string) ! {
	tx.savepoint(name)!
}

// orm_rollback_to rolls back to a savepoint inside the Tx.
pub fn (mut tx Tx) orm_rollback_to(name string) ! {
	tx.rollback_to(name)!
}

// orm_release_savepoint releases a savepoint inside the Tx.
pub fn (mut tx Tx) orm_release_savepoint(name string) ! {
	tx.release_savepoint(name)!
}

// ---- utils ----

fn pg_stmt_binder(mut types []u32, mut vals []&char, mut lens []int, mut formats []int, d orm.QueryData) {
	for data in d.data {
		pg_stmt_match(mut types, mut vals, mut lens, mut formats, data)
	}
}

fn pg_stmt_match_array[T](mut types []u32, mut vals []&char, mut lens []int, mut formats []int, data []T) {
	for element in data {
		pg_stmt_match(mut types, mut vals, mut lens, mut formats, orm.Primitive(element))
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
			num := conv.htonf32(f32(data))
			vals << &char(&num)
			lens << int(sizeof(f32))
			formats << 1
		}
		f64 {
			types << u32(Oid.t_float8)
			num := conv.htonf64(f64(data))
			vals << &char(&num)
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
			// Use a microsecond-precision representation so fractional seconds survive
			// a write/read round-trip (relevant for TIMESTAMP/TIMESTAMPTZ columns).
			datetime := data.format_ss_micro()
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
			vals << &char(unsafe { nil }) // NULL pointer indicates NULL
			lens << int(0) // ignored
			formats << 0 // ignored
		}
		[]orm.Primitive {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]bool {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]f32 {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]f64 {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]i16 {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]i64 {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]i8 {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]int {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]string {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]time.Time {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]u16 {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]u32 {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]u64 {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]u8 {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
		}
		[]orm.InfixType {
			pg_stmt_match_array(mut types, mut vals, mut lens, mut formats, data)
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
		orm.time_ {
			'TIMESTAMP'
		}
		orm.enum_ {
			'BIGINT'
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

// pg_parse_timestamp parses a PostgreSQL `TIMESTAMP`/`TIMESTAMPTZ` text value into a
// `time.Time`. It accepts the form `YYYY-MM-DD HH:mm:ss[.fraction][Z|±HH[:MM[:SS]]]`,
// preserving up to nanosecond precision. When a timezone offset is present (as produced
// by `TIMESTAMPTZ` columns, e.g. `+00`, `+02`, `+02:30`), the returned time is
// normalized to UTC.
fn pg_parse_timestamp(value string) !time.Time {
	str := value.trim_space()
	if str == 'infinity' || str == '-infinity' {
		return error('pg: cannot decode special timestamp value `${str}` into time.Time')
	}
	// PostgreSQL appends ` BC` for dates before year 1. `time.Time` cannot represent
	// those unambiguously, so reject them with a clear error instead of silently
	// constructing the corresponding AD instant.
	if str.ends_with(' BC') || str.ends_with(' bc') {
		return error('pg: cannot decode BC timestamp value `${str}` into time.Time')
	}
	space_pos := str.index(' ') or {
		// Fall back to the generic parser for values without a date/time separator.
		return time.parse(str)
	}
	date_part := str[..space_pos]
	mut time_part := str[space_pos + 1..]

	// Detect and strip an optional timezone designator.
	mut offset_seconds := 0
	if time_part.ends_with('Z') || time_part.ends_with('z') {
		time_part = time_part[..time_part.len - 1]
	} else {
		// PostgreSQL appends the offset sign (`+`/`-`) directly after the time part.
		// Scan past the leading hour so a negative hour can never be mistaken for a sign.
		mut sign_pos := -1
		for i := 1; i < time_part.len; i++ {
			c := time_part[i]
			if c == `+` || c == `-` {
				sign_pos = i
				break
			}
		}
		if sign_pos != -1 {
			offset_seconds = pg_parse_offset(time_part[sign_pos..])!
			time_part = time_part[..sign_pos]
		}
	}

	// Split the optional fractional seconds off the `HH:mm:ss` part.
	mut nanosecond := 0
	mut hms := time_part
	if dot_pos := time_part.index('.') {
		hms = time_part[..dot_pos]
		mut frac := time_part[dot_pos + 1..]
		if frac.len > 9 {
			frac = frac[..9]
		}
		// strconv.atoi is strict, so any non-digit (e.g. a stray suffix) errors out
		// instead of being silently truncated by `string.int()`.
		mut scaled := strconv.atoi(frac)!
		for _ in 0 .. 9 - frac.len {
			scaled *= 10
		}
		nanosecond = scaled
	}

	ymd := date_part.split('-')
	if ymd.len != 3 {
		return error('pg: invalid timestamp date `${date_part}`')
	}
	hms_parts := hms.split(':')
	if hms_parts.len != 3 {
		return error('pg: invalid timestamp time `${hms}`')
	}

	// Use strict numeric parsing so suffixes such as ` BC` or other malformed values
	// are rejected rather than silently coerced (`string.int()` keeps the digit prefix).
	year := strconv.atoi(ymd[0])!
	month := strconv.atoi(ymd[1])!
	day := strconv.atoi(ymd[2])!
	hour := strconv.atoi(hms_parts[0])!
	minute := strconv.atoi(hms_parts[1])!
	second := strconv.atoi(hms_parts[2])!

	// Validate the ranges up front: `time.new` *panics* on out-of-range fields, but
	// PostgreSQL accepts values V cannot represent (e.g. years past 9999), so turn
	// those into a clear error instead of aborting the process. Supported AD years are
	// 1..9999; year 0 / negative years are BC (proleptic Gregorian) and unrepresentable,
	// matching the explicit ` BC` rejection above.
	if year > 9999 {
		return error('pg: year out of range in timestamp `${str}`')
	}
	if year < 1 {
		return error('pg: cannot decode BC/year-0 timestamp `${str}` into time.Time')
	}
	if month < 1 || month > 12 {
		return error('pg: month out of range in timestamp `${str}`')
	}
	if day < 1 || day > 31 {
		return error('pg: day out of range in timestamp `${str}`')
	}
	if hour < 0 || hour > 23 {
		return error('pg: hour out of range in timestamp `${str}`')
	}
	if minute < 0 || minute > 59 {
		return error('pg: minute out of range in timestamp `${str}`')
	}
	if second < 0 || second > 59 {
		return error('pg: second out of range in timestamp `${str}`')
	}

	mut result := time.new(
		year:       year
		month:      month
		day:        day
		hour:       hour
		minute:     minute
		second:     second
		nanosecond: nanosecond
		is_local:   false
	)
	if offset_seconds != 0 {
		// Normalize to UTC by subtracting the parsed offset.
		result = result.add_seconds(-offset_seconds)
		// The offset can push a boundary value past the representable range in either
		// direction, so re-check the normalized result; otherwise offset-bearing
		// timestamps would silently bypass the range guards above. Examples:
		// `9999-12-31 23:30:00-01` -> year 10000, `0001-01-01 00:30:00+01` -> year 0 (BC).
		if result.year > 9999 {
			return error('pg: year out of range in timestamp `${str}` after UTC normalization')
		}
		if result.year < 1 {
			return error('pg: timestamp `${str}` normalizes to a BC/year-0 date, which is unrepresentable')
		}
	}
	return result
}

// pg_parse_offset parses a PostgreSQL timezone offset such as `+02`, `-05`, `+02:30`
// or `+02:30:00` and returns the offset in seconds (signed).
fn pg_parse_offset(offset string) !int {
	if offset.len < 3 {
		return error('pg: invalid timezone offset `${offset}`')
	}
	sign := if offset[0] == `-` { -1 } else { 1 }
	parts := offset[1..].split(':')
	mut seconds := strconv.atoi(parts[0])! * 3600
	if parts.len > 1 {
		seconds += strconv.atoi(parts[1])! * 60
	}
	if parts.len > 2 {
		seconds += strconv.atoi(parts[2])!
	}
	return sign * seconds
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
			orm.time_ {
				// A bare (optionally signed) integer is a Unix timestamp; route every
				// other value through the PostgreSQL-aware parser so textual timestamps
				// and special values such as `infinity` are decoded (or rejected) there
				// instead of silently falling through to `time.unix(0)`.
				if timestamp := strconv.atoi64(str.trim_space()) {
					return orm.Primitive(time.unix(timestamp))
				}
				return orm.Primitive(pg_parse_timestamp(str)!)
			}
			orm.enum_ {
				return orm.Primitive(str.i64())
			}
			else {}
		}

		return error('Unknown field type ${typ}')
	} else {
		return orm.Null{}
	}
}
