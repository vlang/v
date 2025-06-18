module orm

import time
import strings.textscanner

const operators = ['=', '!=', '<>', '>=', '<=', '>', '<', 'LIKE', 'ILIKE', 'IS NULL', 'IS NOT NULL',
	'IN', 'NOT IN']!

@[heap]
pub struct QueryBuilder[T] {
pub mut:
	meta                  []TableField
	valid_sql_field_names []string
	conn                  Connection
	config                SelectConfig
	data                  QueryData
	where                 QueryData
}

// new_query create a new query object for struct `T`
pub fn new_query[T](conn Connection) &QueryBuilder[T] {
	meta := struct_meta[T]()
	return &QueryBuilder[T]{
		meta:                  meta
		valid_sql_field_names: meta.map(sql_field_name(it))
		conn:                  conn
		config:                SelectConfig{
			table: table_from_struct[T]()
		}
		data:                  QueryData{}
		where:                 QueryData{}
	}
}

// reset reset a query object, but keep the connection and table name
pub fn (qb_ &QueryBuilder[T]) reset() &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	old_table := qb.config.table
	qb.config = SelectConfig{
		table: old_table
	}
	qb.data = QueryData{}
	qb.where = QueryData{}
	return qb
}

// where create a `where` clause, it will `AND` with previous `where` clause.
// valid token in the `condition` include: `field's names`, `operator`, `(`, `)`, `?`, `AND`, `OR`, `||`, `&&`,
// valid `operator` incldue: `=`, `!=`, `<>`, `>=`, `<=`, `>`, `<`, `LIKE`, `ILIKE`, `IS NULL`, `IS NOT NULL`, `IN`, `NOT IN`
// example: `where('(a > ? AND b <= ?) OR (c <> ? AND (x = ? OR y = ?))', a, b, c, x, y)`
pub fn (qb_ &QueryBuilder[T]) where(condition string, params ...Primitive) !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	if qb.where.fields.len > 0 {
		// skip first field
		qb.where.is_and << true // and
	}
	qb.parse_conditions(condition, params)!
	qb.config.has_where = true
	return qb
}

// or_where create a `where` clause, it will `OR` with previous `where` clause.
pub fn (qb_ &QueryBuilder[T]) or_where(condition string, params ...Primitive) !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	if qb.where.fields.len > 0 {
		// skip first field
		qb.where.is_and << false // or
	}
	qb.parse_conditions(condition, params)!
	qb.config.has_where = true
	return qb
}

fn parse_error(msg string, pos int, conds string) ! {
	mut m := msg + '\n' + '\t' + conds + '\n\t' + ' '.repeat(pos) + '^\n'
	return error(m)
}

enum ParserState {
	field
	op
	qm
}

struct MyTextScanner {
	textscanner.TextScanner
mut:
	last_tok_start int
}

// next_tok get next token from scanner, skip whitespace
fn (mut ss MyTextScanner) next_tok() string {
	mut ret := ''
	ss.skip_whitespace()
	ss.last_tok_start = ss.pos

	// check for longest token first
	if ss.input[ss.pos..].starts_with('IS NOT NULL') {
		ss.pos += 11
		return 'IS NOT NULL'
	}
	if ss.input[ss.pos..].starts_with('IS NULL') {
		ss.pos += 7
		return 'IS NULL'
	}
	if ss.input[ss.pos..].starts_with('NOT IN') {
		ss.pos += 6
		return 'NOT IN'
	}
	if ss.remaining() >= 2 {
		two_chars := ss.input[ss.pos..ss.pos + 2]
		if two_chars in ['>=', '<=', '<>', '!=', '||', '&&', 'IN'] {
			ss.pos += 2
			return two_chars
		}
	}
	if ss.remaining() > 0 {
		c := ss.input[ss.pos]
		if c in [`>`, `<`, `=`] {
			ss.pos++
			return c.ascii_str()
		}
	}
	for ss.remaining() > 0 {
		c := u8(ss.next()) // only support ascii now
		if c.is_alnum() || c == `_` || c == `$` {
			ret += c.ascii_str()
		} else {
			if ret.len == 0 {
				ret = c.ascii_str()
			} else {
				// already contain a tok
				ss.back()
			}
			break
		}
	}
	return ret
}

// parse_conditions update `qb` by parsing the `conds` string
fn (qb_ &QueryBuilder[T]) parse_conditions(conds string, params []Primitive) ! {
	// conditions: '(a > ? AND b <= ?) OR (c <> ? AND (x = ? OR y = ?))'
	mut qb := unsafe { qb_ }
	if conds.len == 0 {
		return error('${@FN}(): empty condition')
	}
	required_params := conds.count('?')
	if required_params != params.len {
		parse_error('${@FN}(): condition requires `${required_params}` params but got `${params.len}`',
			0, conds)!
	}

	mut s := MyTextScanner{
		input: conds
		ilen:  conds.len
	}

	mut state := ParserState.field
	mut tok := ''
	mut current_field := ''
	mut current_op := OperationKind.eq
	mut current_is_and := true
	mut i := 0
	mut paren_stack := []int{}
	mut is_first_field := true
	for s.remaining() > 0 {
		tok = s.next_tok()
		match state {
			.field {
				// only support valid field names
				if tok in qb.valid_sql_field_names {
					current_field = tok
					state = .op
				} else if tok == '(' {
					paren_stack << qb.where.fields.len
				} else if tok == ')' {
					if paren_stack.len == 0 {
						parse_error('${@FN}: unexpected `)`', s.last_tok_start, conds)!
					}
					start_pos := paren_stack.pop()
					qb.where.parentheses << [start_pos, qb.where.fields.len - 1]
				} else {
					parse_error("${@FN}: table `${qb.config.table}` has no field's name: `${tok}`",
						s.last_tok_start, conds)!
				}
			}
			.op {
				current_op = match tok {
					'=' {
						OperationKind.eq
					}
					'<>' {
						OperationKind.neq
					}
					'!=' {
						OperationKind.neq
					}
					'>' {
						OperationKind.gt
					}
					'<' {
						OperationKind.lt
					}
					'>=' {
						OperationKind.ge
					}
					'<=' {
						OperationKind.le
					}
					'LIKE' {
						OperationKind.orm_like
					}
					'ILIKE' {
						OperationKind.orm_ilike
					}
					'IS NULL' {
						OperationKind.is_null
					}
					'IS NOT NULL' {
						OperationKind.is_not_null
					}
					'IN' {
						OperationKind.in
					}
					'NOT IN' {
						OperationKind.not_in
					}
					else {
						parse_error('${@FN}(): unsupported operator: `${tok}`', s.last_tok_start,
							conds)!
						OperationKind.eq
					}
				}
				if current_op in [.is_null, .is_not_null]! {
					qb.where.fields << current_field
					qb.where.kinds << current_op
					if is_first_field {
						is_first_field = false
					} else {
						// skip first field
						qb.where.is_and << current_is_and
					}
				}
				state = .qm
			}
			.qm {
				if tok == '?' {
					// finish an expr, update `qb`
					qb.where.fields << current_field
					qb.where.data << params[i]
					qb.where.kinds << current_op
					if is_first_field {
						is_first_field = false
					} else {
						// skip first field
						qb.where.is_and << current_is_and
					}
					i++
				} else if tok == ')' {
					if paren_stack.len == 0 {
						parse_error('${@FN}: unexpected `)`', s.last_tok_start, conds)!
					}
					start_pos := paren_stack.pop()
					qb.where.parentheses << [start_pos, qb.where.fields.len - 1]
				} else if tok == 'AND' {
					current_is_and = true
					state = .field
				} else if tok == 'OR' {
					current_is_and = false
					state = .field
				} else if tok == '&&' {
					current_is_and = true
					state = .field
				} else if tok == '||' {
					current_is_and = false
					state = .field
				} else {
					parse_error('${@FN}(): unexpected `${tok}`, maybe `AND`,`OR`', s.last_tok_start,
						conds)!
				}
			}
		}
	}
}

// order create a `order` clause
pub fn (qb_ &QueryBuilder[T]) order(order_type OrderType, field string) !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	if field in qb.valid_sql_field_names {
		qb.config.has_order = true
		qb.config.order = field
		qb.config.order_type = order_type
	} else {
		return error("${@FN}(): table `${qb.config.table}` has no field's name: `${field}`")
	}
	return qb
}

// limit create a `limit` clause
pub fn (qb_ &QueryBuilder[T]) limit(limit int) !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	if limit > 0 {
		qb.config.has_limit = true
		qb.data.data << Primitive(limit)
		qb.data.types << type_idx['int']
	} else {
		return error('${@FN}(): `limit` should be a positive integer')
	}
	return qb
}

// offset create a `offset` clause
pub fn (qb_ &QueryBuilder[T]) offset(offset int) !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	if offset >= 0 {
		qb.config.has_offset = true
		qb.data.data << Primitive(offset)
		qb.data.types << type_idx['int']
	} else {
		return error('${@FN}(): `offset` should be a integer > 0')
	}
	return qb
}

// select create a `select` clause
pub fn (qb_ &QueryBuilder[T]) select(fields ...string) !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	for f in fields {
		if f !in qb.valid_sql_field_names {
			return error("${@FN}(): table `${qb.config.table}` has no field's name: `${f}`")
		}
	}
	qb.config.fields = fields
	return qb
}

// set create a `set` clause for `update`
pub fn (qb_ &QueryBuilder[T]) set(assign string, values ...Primitive) !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	if assign.len == 0 {
		return error('${@FN}(): empty `set`')
	}
	required_params := assign.count('?')
	if required_params != values.len {
		return error('${@FN}(): `set` requires `${required_params}` params but got `${values.len}`')
	}
	mut fields := []string{}
	assign_splits := assign.split_any(',')
	for assign_split in assign_splits {
		f := assign_split.split_any('=')
		if f.len != 2 {
			return error('${@FN}(): `set` syntax error, it should look like : `a=?,b=?`')
		}
		if f[1].trim_space() != '?' {
			return error('${@FN}(): `set` syntax error, it should look like : `a=?,b=?`')
		}
		field := f[0].trim_space()
		if field !in qb.valid_sql_field_names {
			return error("${@FN}(): table `${qb.config.table}` has no field's name: `${field}`")
		}
		fields << field
	}
	qb.data.fields = fields
	qb.data.data = values
	return qb
}

// table_from_struct get table from struct
fn table_from_struct[T]() Table {
	mut table_name := T.name
	mut attrs := []VAttribute{}
	$for a in T.attributes {
		$if a.name == 'table' && a.has_arg {
			table_name = a.arg
		}
		attrs << a
	}
	return Table{
		name:  table_name
		attrs: attrs
	}
}

// struct_meta return a struct's fields info
fn struct_meta[T]() []TableField {
	mut meta := []TableField{}
	$for field in T.fields {
		mut attrs := []VAttribute{}
		mut is_skip := false
		for attr in field.attrs {
			f := attr.split_any(':')
			if f.len == 1 {
				ff := f[0].trim_space()
				if ff == 'skip' {
					is_skip = true
				}
				attrs << VAttribute{
					name: ff
				}
				continue
			}
			if f.len == 2 {
				ff := f[1].trim_space()
				if f[0].trim_space() == 'sql' && ff == '-' {
					is_skip = true
				}
				mut kind := AttributeKind.plain
				if ff == 'true' || ff == 'false' {
					kind = .bool
				} else if ff.starts_with('if ') {
					kind = .comptime_define
				} else if (ff.starts_with("'") && ff.ends_with("'"))
					|| (ff.starts_with('"') && ff.ends_with('"')) {
					kind = .string
				} else if ff.contains_only('0123456789') {
					kind = .number
				} else if ff !in ['serial', 'i8', 'i16', 'int', 'i64', 'u8', 'u16', 'u32', 'u64',
					'f32', 'f64', 'bool', 'string'] {
					// @[sql: data_type] need kind = .plain
					// @[sql: column_name] need kind = .string
					kind = .string
				}
				attrs << VAttribute{
					name:    f[0].trim_space()
					has_arg: true
					arg:     ff
					kind:    kind
				}
			}
		}

		mut field_type := field.typ
		if typeof(field).name.contains('time.Time') {
			field_type = time_
		} else if field.is_struct {
			field_type = type_idx['int']
		} else if field.is_enum {
			field_type = enum_
		}

		if !is_skip {
			meta << TableField{
				name:     field.name
				typ:      field_type
				nullable: field.is_option
				attrs:    attrs
			}
		}
	}
	return meta
}

// map_row map a row result into a struct
fn (qb &QueryBuilder[T]) map_row(row []Primitive) !T {
	mut instance := T{}

	$for field in T.fields {
		mut m := TableField{}
		mm := qb.meta.filter(it.name == field.name)
		if mm.len != 0 {
			m = mm[0]
			index := qb.config.fields.index(sql_field_name(m))
			if index >= 0 {
				value := row[index]

				$if field.typ is $option {
					if value == Primitive(Null{}) {
						instance.$(field.name) = none
					}
				}
				if value != Primitive(Null{}) {
					$if field.typ is i8 || field.typ is ?i8 {
						instance.$(field.name) = match value {
							i8 { i8(value) }
							i16 { i8(value) }
							int { i8(value) }
							i64 { i8(value) }
							u8 { i8(value) }
							u16 { i8(value) }
							u32 { i8(value) }
							u64 { i8(value) }
							bool { i8(value) }
							else { 0 }
						}
					} $else $if field.typ is i16 || field.typ is ?i16 {
						instance.$(field.name) = match value {
							i8 { i16(value) }
							i16 { i16(value) }
							int { i16(value) }
							i64 { i16(value) }
							u8 { i16(value) }
							u16 { i16(value) }
							u32 { i16(value) }
							u64 { i16(value) }
							bool { i16(value) }
							else { 0 }
						}
					} $else $if field.typ is int || field.typ is ?int {
						instance.$(field.name) = match value {
							i8 { int(value) }
							i16 { int(value) }
							int { int(value) }
							i64 { int(value) }
							u8 { int(value) }
							u16 { int(value) }
							u32 { int(value) }
							u64 { int(value) }
							bool { int(value) }
							else { 0 }
						}
					} $else $if field.typ is i64 || field.typ is ?i64 {
						instance.$(field.name) = match value {
							i8 { i64(value) }
							i16 { i64(value) }
							int { i64(value) }
							i64 { i64(value) }
							u8 { i64(value) }
							u16 { i64(value) }
							u32 { i64(value) }
							u64 { i64(value) }
							bool { i64(value) }
							else { 0 }
						}
					} $else $if field.typ is u8 || field.typ is ?u8 {
						instance.$(field.name) = match value {
							i8 { u8(value) }
							i16 { u8(value) }
							int { u8(value) }
							i64 { u8(value) }
							u8 { u8(value) }
							u16 { u8(value) }
							u32 { u8(value) }
							u64 { u8(value) }
							bool { u8(value) }
							else { 0 }
						}
					} $else $if field.typ is u16 || field.typ is ?u16 {
						instance.$(field.name) = match value {
							i8 { u16(value) }
							i16 { u16(value) }
							int { u16(value) }
							i64 { u16(value) }
							u8 { u16(value) }
							u16 { u16(value) }
							u32 { u16(value) }
							u64 { u16(value) }
							bool { u16(value) }
							else { 0 }
						}
					} $else $if field.typ is u32 || field.typ is ?u32 {
						instance.$(field.name) = match value {
							i8 { u32(value) }
							i16 { u32(value) }
							int { u32(value) }
							i64 { u32(value) }
							u8 { u32(value) }
							u16 { u32(value) }
							u32 { u32(value) }
							u64 { u32(value) }
							bool { u32(value) }
							else { 0 }
						}
					} $else $if field.typ is u64 || field.typ is ?u64 {
						instance.$(field.name) = match value {
							i8 { u64(value) }
							i16 { u64(value) }
							int { u64(value) }
							i64 { u64(value) }
							u8 { u64(value) }
							u16 { u64(value) }
							u32 { u64(value) }
							u64 { u64(value) }
							bool { u64(value) }
							else { 0 }
						}
					} $else $if field.typ is f32 || field.typ is ?f32 {
						instance.$(field.name) = match value {
							i8 { f32(value) }
							i16 { f32(value) }
							int { f32(value) }
							i64 { f32(value) }
							u8 { f32(value) }
							u16 { f32(value) }
							u32 { f32(value) }
							u64 { f32(value) }
							bool { f32(value) }
							else { 0 }
						}
					} $else $if field.typ is f64 || field.typ is ?f64 {
						instance.$(field.name) = match value {
							i8 { f64(value) }
							i16 { f64(value) }
							int { f64(value) }
							i64 { f64(value) }
							u8 { f64(value) }
							u16 { f64(value) }
							u32 { f64(value) }
							u64 { f64(value) }
							bool { f64(value) }
							else { 0 }
						}
					} $else $if field.typ is bool || field.typ is ?bool {
						instance.$(field.name) = match value {
							i8 { value != 0 }
							i16 { value != 0 }
							int { value != 0 }
							i64 { value != 0 }
							u8 { value != 0 }
							u16 { value != 0 }
							u32 { value != 0 }
							u64 { value != 0 }
							bool { value }
							else { false }
						}
					} $else $if field.typ is string || field.typ is ?string {
						instance.$(field.name) = value as string
					} $else $if field.typ is time.Time || field.typ is ?time.Time {
						if m.typ == time_ {
							instance.$(field.name) = value as time.Time
						} else if m.typ == type_string {
							instance.$(field.name) = time.parse(value as string)!
						}
					}
				}
			}
		}
	}
	return instance
}

// prepare QueryBuilder, ready for gen SQL
fn (qb_ &QueryBuilder[T]) prepare() ! {
	mut qb := unsafe { qb_ }

	// check for mismatch `(` and `)`
	for p in qb.where.parentheses {
		if p[1] == -1 {
			return error('${@FN}(): missing `)`')
		}
	}

	// auto fill field's names if not set by `select`
	if qb.config.fields.len == 0 {
		qb.config.fields = qb.meta.map(sql_field_name(it))
	}

	if qb.config.types.len == 0 {
		// set field's types
		mut types := []int{cap: qb.config.fields.len}
		for f in qb.config.fields {
			for ff in qb.meta {
				if sql_field_name(ff) == f {
					types << ff.typ
				}
			}
		}
		qb.config.types = types
	}
}

// query start a query and return result in struct `T`
pub fn (qb_ &QueryBuilder[T]) query() ![]T {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	qb.prepare()!
	rows := qb.conn.select(qb.config, qb.data, qb.where)!
	mut result := []T{cap: rows.len}
	for row in rows {
		result << qb.map_row[T](row)!
	}
	return result
}

// count start a count query and return result
pub fn (qb_ &QueryBuilder[T]) count() !int {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	mut count_config := qb.config
	count_config.is_count = true
	count_config.fields = []
	qb.prepare()!
	result := qb.conn.select(count_config, qb.data, qb.where)!

	if result.len == 0 || result[0].len == 0 {
		return 0
	}
	count_val := result[0][0]
	return match count_val {
		int { count_val }
		i64 { int(count_val) }
		u64 { int(count_val) }
		else { return error('${@FN}(): invalid count result type') }
	}
}

// insert insert a record into the database
pub fn (qb_ &QueryBuilder[T]) insert[T](value T) !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	qb.insert_many([value])!
	return qb
}

// insert_many insert records into the database
pub fn (qb_ &QueryBuilder[T]) insert_many[T](values []T) !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	qb.prepare()!
	if values.len == 0 {
		return error('${@FN}(): `insert` need at least one record')
	}
	for value in values {
		new_qb := fill_data_with_struct[T](value, qb.meta)
		qb.conn.insert(qb.config.table, new_qb)!
	}
	return qb
}

fn fill_data_with_struct[T](value T, meta []TableField) QueryData {
	mut qb := QueryData{}
	$for field in T.fields {
		sql_fields := meta.filter(it.name == field.name)
		if sql_fields.len == 1 {
			sql_f := sql_fields[0]
			sql_f_name := sql_field_name(sql_f)
			sql_f_type := sql_field_type(sql_f)

			if sql_f_type == serial {
				// `serial` should be auto field
				qb.auto_fields << qb.fields.len
			}
			qb.fields << sql_f_name

			$if field.typ is bool {
				qb.data << bool_to_primitive(value.$(field.name))
			} $else $if field.typ is ?bool {
				qb.data << option_bool_to_primitive(value.$(field.name))
			}
			$if field.typ is f32 {
				qb.data << f32_to_primitive(value.$(field.name))
			} $else $if field.typ is ?f32 {
				qb.data << option_f32_to_primitive(value.$(field.name))
			}
			$if field.typ is f64 {
				qb.data << f64_to_primitive(value.$(field.name))
			} $else $if field.typ is ?f64 {
				qb.data << option_f64_to_primitive(value.$(field.name))
			}
			$if field.typ is i8 {
				qb.data << i8_to_primitive(value.$(field.name))
			} $else $if field.typ is ?i8 {
				qb.data << option_i8_to_primitive(value.$(field.name))
			}
			$if field.typ is i16 {
				qb.data << i16_to_primitive(value.$(field.name))
			} $else $if field.typ is ?i16 {
				qb.data << option_i16_to_primitive(value.$(field.name))
			}
			$if field.typ is int {
				qb.data << int_to_primitive(value.$(field.name))
			} $else $if field.typ is ?int {
				qb.data << option_int_to_primitive(value.$(field.name))
			}
			$if field.typ is i64 {
				qb.data << i64_to_primitive(value.$(field.name))
			} $else $if field.typ is ?i64 {
				qb.data << option_i64_to_primitive(value.$(field.name))
			}
			$if field.typ is u8 {
				qb.data << u8_to_primitive(value.$(field.name))
			} $else $if field.typ is ?u8 {
				qb.data << option_u8_to_primitive(value.$(field.name))
			}
			$if field.typ is u16 {
				qb.data << u16_to_primitive(value.$(field.name))
			} $else $if field.typ is ?u16 {
				qb.data << option_u16_to_primitive(value.$(field.name))
			}
			$if field.typ is u32 {
				qb.data << u32_to_primitive(value.$(field.name))
			} $else $if field.typ is ?u32 {
				qb.data << option_u32_to_primitive(value.$(field.name))
			}
			$if field.typ is u64 {
				qb.data << u64_to_primitive(value.$(field.name))
			} $else $if field.typ is ?u64 {
				qb.data << option_u64_to_primitive(value.$(field.name))
			}
			$if field.typ is string {
				qb.data << string_to_primitive(value.$(field.name))
			} $else $if field.typ is ?string {
				qb.data << option_string_to_primitive(value.$(field.name))
			} $else $if field.typ is time.Time {
				if sql_f_type == type_string {
					qb.data << string_to_primitive(value.$(field.name).format_ss())
				} else {
					qb.data << time_to_primitive(value.$(field.name))
				}
			} $else $if field.typ is ?time.Time {
				if sql_f_type == type_string {
					b := value.$(field.name)
					if b_ := b {
						qb.data << Primitive(b_.format_ss())
					} else {
						qb.data << null_primitive
					}
				} else {
					qb.data << option_time_to_primitive(value.$(field.name))
				}
			}
		}
	}
	return qb
}

// update update record(s) in the database
pub fn (qb_ &QueryBuilder[T]) update() !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	qb.prepare()!
	if qb.data.fields.len == 0 {
		return error('${@FN}(): `update` need at least one `set` clause')
	}
	qb.conn.update(qb.config.table, qb.data, qb.where)!
	return qb
}

// delete delete record(s) in the database
pub fn (qb_ &QueryBuilder[T]) delete() !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	qb.prepare()!
	qb.conn.delete(qb.config.table, qb.where)!
	return qb
}

// create create a table
pub fn (qb_ &QueryBuilder[T]) create() !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	qb.conn.create(qb.config.table, qb.meta)!
	return qb
}

// drop drop a table
pub fn (qb_ &QueryBuilder[T]) drop() !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	qb.conn.drop(qb.config.table)!
	return qb
}

// last_id returns the last inserted id of the db
pub fn (qb_ &QueryBuilder[T]) last_id() int {
	mut qb := unsafe { qb_ }
	qb.reset()
	return qb.conn.last_id()
}
