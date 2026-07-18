module orm

import time
import strings.textscanner

const operators = ['=', '!=', '<>', '>=', '<=', '>', '<', 'LIKE', 'ILIKE', 'IS NULL', 'IS NOT NULL',
	'IN', 'NOT IN']!

pub struct AggregateValue {
pub:
	has_value bool
	value     Primitive = Null{}
}

@[heap]
pub struct QueryBuilder[T] {
mut:
	builder_error string
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
			table: table_from_struct[T](meta)
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
	qb.builder_error = ''
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_select_fields(fields []string) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	qb.config.fields = fields.map(qb.v_sql_field_name(it))
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_select_qualified_fields(fields []string) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	mut select_fields := fields.map(qb.v_sql_field_name(it))
	if select_fields.len == 0 {
		for field in qb.meta {
			if !field.is_arr {
				select_fields << sql_field_name(field)
			}
		}
	}
	qb.config.fields = select_fields
	qb.config.select_exprs = select_fields.map(qb.v_sql_qualified_select_expr(it))
	return qb
}

fn (qb &QueryBuilder[T]) v_sql_qualified_select_expr(field string) string {
	for meta_field in qb.meta {
		if sql_field_name(meta_field) == field {
			select_expr := sql_field_select_expr(meta_field)
			if select_expr != field {
				return select_expr
			}
			break
		}
	}
	return table_qualified_field(qb.config.table.name, field)
}

fn (qb_ &QueryBuilder[T]) v_sql_where_primitive(field string, kind OperationKind, value Primitive) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	normalized := if kind in [.in, .not_in]! { normalize_primitive_argument(value) } else { value }
	qb.where.fields << qb.v_sql_field_name(field)
	qb.where.data << normalized
	qb.where.types << primitive_type(normalized)
	qb.where.kinds << kind
	qb.config.has_where = true
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_where_condition(condition string, params []Primitive) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	old_where := clone_query_data(qb.where)
	if qb.where.fields.len > 0 {
		qb.where.is_and << true
	}
	qb.parse_conditions(condition, normalize_primitive_arguments(params)) or {
		qb.where = old_where
		qb.builder_error = err.msg()
		return qb
	}
	qb.config.has_where = true
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_qualified_where_condition(condition string, params []Primitive) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	old_where := clone_query_data(qb.where)
	if qb.where.fields.len > 0 {
		qb.where.is_and << true
	}
	start := qb.where.fields.len
	qb.parse_conditions(condition, normalize_primitive_arguments(params)) or {
		qb.where = old_where
		qb.builder_error = err.msg()
		return qb
	}
	for i in start .. qb.where.fields.len {
		qb.where.fields[i] = table_qualified_field(qb.config.table.name, qb.where.fields[i])
	}
	qb.config.has_where = true
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_error(message string) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	qb.builder_error = message
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_where_query_data(data QueryData) &QueryBuilder[T] {
	mapped := qb_.v_sql_mapped_query_data(data)
	return qb_.v_sql_where_mapped_query_data(mapped)
}

fn (qb_ &QueryBuilder[T]) v_sql_qualified_where_query_data(data QueryData) &QueryBuilder[T] {
	mut mapped := qb_.v_sql_mapped_query_data(data)
	for i in 0 .. mapped.fields.len {
		mapped.fields[i] = table_qualified_field(qb_.config.table.name, mapped.fields[i])
	}
	return qb_.v_sql_where_mapped_query_data(mapped)
}

fn (qb_ &QueryBuilder[T]) v_sql_where_mapped_query_data(mapped QueryData) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	if mapped.fields.len == 0 {
		if qb.data.fields.len > 0 && qb.where.fields.len == 0 && qb.builder_error.len == 0 {
			qb.builder_error = '${@FN}(): dynamic SQL UPDATE requires a WHERE clause'
		}
		return qb
	}
	if qb.where.fields.len > 0 {
		qb.where.is_and << true
	}
	offset := qb.where.fields.len
	qb.where.fields << mapped.fields
	for item in mapped.data {
		qb.where.data << item
	}
	qb.where.types << mapped.types
	qb.where.kinds << mapped.kinds
	for item in mapped.parentheses {
		if item.len == 2 {
			qb.where.parentheses << [item[0] + offset, item[1] + offset]
		}
	}
	qb.where.auto_fields << mapped.auto_fields
	qb.where.is_and << mapped.is_and
	qb.config.has_where = true
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_set_primitive(field string, value Primitive) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	qb.data.fields << qb.v_sql_field_name(field)
	qb.data.data << value
	qb.data.types << primitive_type(value)
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_set_query_data(data QueryData) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	mapped := qb.v_sql_mapped_query_data(data)
	if mapped.fields.len == 0 {
		if qb.data.fields.len == 0 && qb.builder_error.len == 0 {
			qb.builder_error = '${@FN}(): dynamic SQL UPDATE requires at least one assignment'
		}
		return qb
	}
	qb.data.fields << mapped.fields
	for item in mapped.data {
		qb.data.data << item
	}
	qb.data.types << mapped.types
	qb.data.kinds << mapped.kinds
	qb.data.parentheses << mapped.parentheses
	qb.data.auto_fields << mapped.auto_fields
	qb.data.is_and << mapped.is_and
	return qb
}

fn (qb &QueryBuilder[T]) v_sql_mapped_query_data(data QueryData) QueryData {
	mut mapped := clone_query_data(data)
	mapped.fields = []string{cap: data.fields.len}
	for field in data.fields {
		mapped.fields << qb.v_sql_field_name(field)
	}
	return mapped
}

fn v_sql_query_data_add(data QueryData, field string, kind OperationKind, value Primitive, is_and bool) QueryData {
	mut out := clone_query_data(data)
	if out.fields.len > 0 {
		out.is_and << is_and
	}
	out.fields << field
	out.kinds << kind
	if !kind.is_unary() {
		normalized := if kind in [.in, .not_in]! {
			normalize_primitive_argument(value)
		} else {
			value
		}
		out.data << normalized
		out.types << primitive_type(normalized)
	}
	return out
}

fn v_sql_query_data_parentheses(data QueryData, start int) QueryData {
	mut out := clone_query_data(data)
	if out.fields.len > start {
		out.parentheses << [start, out.fields.len - 1]
	}
	return out
}

fn (qb_ &QueryBuilder[T]) v_sql_order(field string, desc bool) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	qb.config.has_order = true
	qb.config.order = qb.v_sql_field_name(field)
	qb.config.order_type = if desc { OrderType.desc } else { OrderType.asc }
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_qualified_order(field string, desc bool) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	qb.config.has_order = true
	qb.config.order = table_qualified_field(qb.config.table.name, qb.v_sql_field_name(field))
	qb.config.order_type = if desc { OrderType.desc } else { OrderType.asc }
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_join(kind JoinType, table Table, left_col string, right_col string) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	qb.config.joins << JoinConfig{
		kind:          kind
		table:         table
		on_left_table: qb.config.table.name
		on_left_col:   qb.v_sql_field_name(left_col)
		on_right_col:  orm_table_sql_field_name(table, right_col)
	}
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_join_from(kind JoinType, left_table Table, table Table, left_col string, right_col string) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	qb.config.joins << JoinConfig{
		kind:          kind
		table:         table
		on_left_table: left_table.name
		on_left_col:   orm_table_sql_field_name(left_table, left_col)
		on_right_col:  orm_table_sql_field_name(table, right_col)
	}
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_distinct() &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	qb.config.has_distinct = true
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_limit(limit int) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	if limit >= 0 {
		qb.config.has_limit = true
		qb.data.data << Primitive(limit)
		qb.data.types << type_idx['int']
	} else {
		qb.builder_error = '${@FN}(): `limit` should be a non-negative integer'
	}
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_offset(offset int) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	if offset >= 0 {
		qb.config.has_offset = true
		qb.data.data << Primitive(offset)
		qb.data.types << type_idx['int']
	} else {
		qb.builder_error = '${@FN}(): `offset` should be a non-negative integer'
	}
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_update_and_zero() !int {
	mut qb := unsafe { qb_ }
	qb.update()!
	return 0
}

fn (qb_ &QueryBuilder[T]) v_sql_delete_and_zero() !int {
	mut qb := unsafe { qb_ }
	qb.delete()!
	return 0
}

fn (qb_ &QueryBuilder[T]) v_sql_update_many_and_zero(values []T, key_field string, field_names []string) !int {
	mut qb := unsafe { qb_ }
	mut conn := qb.conn
	update_many[T](mut conn, values, qb.v_sql_field_name(key_field),
		...field_names.map(qb.v_sql_field_name(it)))!
	return 0
}

fn (qb &QueryBuilder[T]) v_sql_field_name(field string) string {
	for meta_field in qb.meta {
		if meta_field.name == field || sql_field_name(meta_field) == field {
			return sql_field_name(meta_field)
		}
		if meta_field.name.ends_with('.${field}') {
			return sql_field_name(meta_field)
		}
	}
	return field
}

fn orm_table_sql_field_name(table Table, field string) string {
	for i, name in table.fields {
		if name == field || (i < table.columns.len && table.columns[i] == field) {
			if i < table.columns.len && table.columns[i].len > 0 {
				return table.columns[i]
			}
			return name
		}
	}
	return field
}

fn (qb_ &QueryBuilder[T]) v_sql_table_attrs(attrs []VAttribute) &QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	qb.config.table.attrs = attrs
	for attr in attrs {
		if attr_name_matches(attr.name, 'table') && attr.has_arg {
			qb.config.table.name = trim_attr_arg(attr.arg)
			break
		}
	}
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
	qb.parse_conditions(condition, normalize_primitive_arguments(params))!
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
	qb.parse_conditions(condition, normalize_primitive_arguments(params))!
	qb.config.has_where = true
	return qb
}

fn normalize_primitive_arguments(params []Primitive) []Primitive {
	mut normalized := []Primitive{cap: params.len}
	for i := 0; i < params.len; i++ {
		normalized << normalize_primitive_argument(params[i])
	}
	return normalized
}

fn normalize_primitive_argument(value Primitive) Primitive {
	return match value {
		[]Primitive {
			value
		}
		[]bool {
			Primitive(value.map(bool_to_primitive(it)))
		}
		[]f32 {
			Primitive(value.map(f32_to_primitive(it)))
		}
		[]f64 {
			Primitive(value.map(f64_to_primitive(it)))
		}
		[]i8 {
			Primitive(value.map(i8_to_primitive(it)))
		}
		[]i16 {
			Primitive(value.map(i16_to_primitive(it)))
		}
		[]int {
			Primitive(value.map(int_to_primitive(it)))
		}
		[]i64 {
			Primitive(value.map(i64_to_primitive(it)))
		}
		[]u8 {
			Primitive(value.map(u8_to_primitive(it)))
		}
		[]u16 {
			Primitive(value.map(u16_to_primitive(it)))
		}
		[]u32 {
			Primitive(value.map(u32_to_primitive(it)))
		}
		[]u64 {
			Primitive(value.map(u64_to_primitive(it)))
		}
		[]string {
			Primitive(value.map(string_to_primitive(it)))
		}
		[]time.Time {
			Primitive(value.map(time_to_primitive(it)))
		}
		[]InfixType {
			Primitive(value.map(infix_to_primitive(it)))
		}
		else {
			value
		}
	}
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
	ss_upper := ss.input.to_upper()

	// check for longest token first
	if ss_upper[ss.pos..].starts_with('IS NOT NULL') {
		ss.pos += 11
		return 'IS NOT NULL'
	}
	if ss_upper[ss.pos..].starts_with('IS NULL') {
		ss.pos += 7
		return 'IS NULL'
	}
	if ss_upper[ss.pos..].starts_with('NOT IN') {
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
		if c.is_alnum() || c == `_` || c == `$` || c == `.` {
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
				mapped_field := qb.v_sql_field_name(tok)
				if mapped_field in qb.valid_sql_field_names {
					current_field = mapped_field
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
				current_op = match tok.to_upper() {
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
					parse_error('${@FN}(): unexpected `${tok}`, maybe `AND`,`OR`',
						s.last_tok_start, conds)!
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

// distinct marks the query as `SELECT DISTINCT`.
pub fn (qb_ &QueryBuilder[T]) distinct() !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	qb.config.has_distinct = true
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
	qb.data.fields << fields
	for v in values {
		qb.data.data << v
	}
	return qb
}

// table_from_struct get table from struct
fn table_from_struct[T](meta []TableField) Table {
	mut table_name := T.name
	// Strip generic parameters from type name (e.g., Message[Payload] -> Message)
	if bracket_pos := table_name.index('[') {
		table_name = table_name[..bracket_pos]
	}
	mut has_custom_table_name := false
	mut attrs := []VAttribute{}
	$for a in T.attributes {
		attrs << a
	}
	for attr in attrs {
		if attr_name_matches(attr.name, 'table') && attr.has_arg {
			table_name = trim_attr_arg(attr.arg)
			has_custom_table_name = true
			break
		}
	}
	if !has_custom_table_name {
		// Keep default ORM table names aligned with unquoted SQL identifiers across DB drivers.
		table_name = table_name.to_lower()
	}
	return Table{
		name:    table_name
		attrs:   attrs
		fields:  meta.map(it.name)
		columns: meta.map(sql_field_name(it))
	}
}

fn orm_field_attrs_from_strings(field_attrs []string) ([]VAttribute, bool) {
	mut attrs := []VAttribute{}
	mut is_skip := false
	for attr in field_attrs {
		name, arg_text, has_arg := orm_attr_name_arg(attr)
		if !has_arg {
			ff := name
			if ff == 'skip' {
				is_skip = true
			}
			attrs << VAttribute{
				name: ff
			}
			continue
		}
		if has_arg {
			ff := arg_text
			if name == 'sql' && trim_attr_arg(ff) == '-' {
				is_skip = true
			}
			mut kind := AttributeKind.plain
			mut arg := ff
			if ff == 'true' || ff == 'false' {
				kind = .bool
			} else if ff.starts_with('if ') {
				kind = .comptime_define
			} else if (ff.starts_with("'") && ff.ends_with("'"))
				|| (ff.starts_with('"') && ff.ends_with('"')) {
				kind = .string
				arg = trim_attr_arg(ff)
			} else if ff.contains_only('0123456789') {
				kind = .number
			} else if ff !in ['serial', 'i8', 'i16', 'int', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
				'f64', 'bool', 'string'] {
				// @[sql: data_type] need kind = .plain
				// @[sql: column_name] need kind = .string
				kind = .string
			}
			attrs << VAttribute{
				name:    name
				has_arg: true
				arg:     arg
				kind:    kind
			}
		}
	}
	return attrs, is_skip
}

fn orm_attr_name_arg(attr string) (string, string, bool) {
	idx := attr.index(':') or { return attr.trim_space(), '', false }
	return attr[..idx].trim_space(), attr[idx + 1..].trim_space(), true
}

// struct_meta return a struct's fields info
fn struct_meta[T]() []TableField {
	mut meta := []TableField{}
	$for field in T.fields {
		$if field.is_embed {
			$for sub in field.typ.fields {
				attrs, is_skip := orm_field_attrs_from_strings(sub.attrs)
				mut field_type := sub.typ
				$if sub.unaliased_typ is bool || sub.unaliased_typ is ?bool {
					field_type = type_idx['bool']
				} $else $if sub.unaliased_typ is f32 || sub.unaliased_typ is ?f32 {
					field_type = type_idx['f32']
				} $else $if sub.unaliased_typ is f64 || sub.unaliased_typ is ?f64 {
					field_type = type_idx['f64']
				} $else $if sub.unaliased_typ is i8 || sub.unaliased_typ is ?i8 {
					field_type = type_idx['i8']
				} $else $if sub.unaliased_typ is i16 || sub.unaliased_typ is ?i16 {
					field_type = type_idx['i16']
				} $else $if sub.unaliased_typ is int || sub.unaliased_typ is ?int {
					field_type = type_idx['int']
				} $else $if sub.unaliased_typ is i64 || sub.unaliased_typ is ?i64 {
					field_type = type_idx['i64']
				} $else $if sub.unaliased_typ is u8 || sub.unaliased_typ is ?u8 {
					field_type = type_idx['u8']
				} $else $if sub.unaliased_typ is u16 || sub.unaliased_typ is ?u16 {
					field_type = type_idx['u16']
				} $else $if sub.unaliased_typ is u32 || sub.unaliased_typ is ?u32 {
					field_type = type_idx['u32']
				} $else $if sub.unaliased_typ is u64 || sub.unaliased_typ is ?u64 {
					field_type = type_idx['u64']
				} $else $if sub.unaliased_typ is string || sub.unaliased_typ is ?string {
					field_type = type_string
				} $else $if sub.unaliased_typ is time.Time || sub.unaliased_typ is ?time.Time {
					field_type = time_
				} $else $if sub.unaliased_typ is $enum {
					field_type = enum_
				} $else {
					if typeof(sub).name.contains('time.Time') {
						field_type = time_
					} else if sub.is_struct {
						field_type = type_idx['int']
					} else if sub.is_enum {
						field_type = enum_
					}
				}

				if !is_skip {
					mut is_arr := false
					$if sub.unaliased_typ is $array {
						is_arr = true
					}
					if orm_type_name_is_optional_array(typeof(sub).name) {
						is_arr = true
					}
					meta << TableField{
						name:     '${field.name}.${sub.name}'
						typ:      field_type
						nullable: sub.is_option
						attrs:    attrs
						is_arr:   is_arr
					}
				}
			}
		} $else {
			attrs, is_skip := orm_field_attrs_from_strings(field.attrs)
			mut field_type := field.typ
			$if field.unaliased_typ is bool || field.unaliased_typ is ?bool {
				field_type = type_idx['bool']
			} $else $if field.unaliased_typ is f32 || field.unaliased_typ is ?f32 {
				field_type = type_idx['f32']
			} $else $if field.unaliased_typ is f64 || field.unaliased_typ is ?f64 {
				field_type = type_idx['f64']
			} $else $if field.unaliased_typ is i8 || field.unaliased_typ is ?i8 {
				field_type = type_idx['i8']
			} $else $if field.unaliased_typ is i16 || field.unaliased_typ is ?i16 {
				field_type = type_idx['i16']
			} $else $if field.unaliased_typ is int || field.unaliased_typ is ?int {
				field_type = type_idx['int']
			} $else $if field.unaliased_typ is i64 || field.unaliased_typ is ?i64 {
				field_type = type_idx['i64']
			} $else $if field.unaliased_typ is u8 || field.unaliased_typ is ?u8 {
				field_type = type_idx['u8']
			} $else $if field.unaliased_typ is u16 || field.unaliased_typ is ?u16 {
				field_type = type_idx['u16']
			} $else $if field.unaliased_typ is u32 || field.unaliased_typ is ?u32 {
				field_type = type_idx['u32']
			} $else $if field.unaliased_typ is u64 || field.unaliased_typ is ?u64 {
				field_type = type_idx['u64']
			} $else $if field.unaliased_typ is string || field.unaliased_typ is ?string {
				field_type = type_string
			} $else $if field.unaliased_typ is time.Time || field.unaliased_typ is ?time.Time {
				field_type = time_
			} $else $if field.unaliased_typ is $enum {
				field_type = enum_
			} $else {
				if typeof(field).name.contains('time.Time') {
					field_type = time_
				} else if field.is_struct {
					field_type = type_idx['int']
				} else if field.is_enum {
					field_type = enum_
				}
			}

			if !is_skip {
				mut is_arr := false
				$if field.unaliased_typ is $array {
					is_arr = true
				}
				if orm_type_name_is_optional_array(typeof(field).name) {
					is_arr = true
				}
				meta << TableField{
					name:     field.name
					typ:      field_type
					nullable: field.is_option
					attrs:    attrs
					is_arr:   is_arr
				}
			}
		}
	}
	return meta
}

// map_row map a row result into a struct
fn (qb &QueryBuilder[T]) map_row(row []Primitive) !T {
	mut instance := T{}

	$for field in T.fields {
		$if field.is_embed {
			$for sub in field.typ.fields {
				mut m := TableField{}
				mm := qb.meta.filter(it.name == '${field.name}.${sub.name}')
				if mm.len != 0 {
					m = mm[0]
					index := qb.config.fields.index(sql_field_name(m))
					if index >= 0 {
						value := row[index]

						if value != Primitive(Null{}) {
							$if sub.unaliased_typ is i8 || sub.unaliased_typ is ?i8 {
								instance.$(field.name).$(sub.name) = match value {
									i8 { i8(value) }
									i16 { i8(value) }
									int { i8(value) }
									i64 { i8(value) }
									u8 { i8(value) }
									u16 { i8(value) }
									u32 { i8(value) }
									u64 { i8(value) }
									bool { i8(value) }
									f32 { i8(value) }
									f64 { i8(value) }
									else { 0 }
								}
							} $else $if sub.unaliased_typ is i16 || sub.unaliased_typ is ?i16 {
								instance.$(field.name).$(sub.name) = match value {
									i8 { i16(value) }
									i16 { i16(value) }
									int { i16(value) }
									i64 { i16(value) }
									u8 { i16(value) }
									u16 { i16(value) }
									u32 { i16(value) }
									u64 { i16(value) }
									bool { i16(value) }
									f32 { i16(value) }
									f64 { i16(value) }
									else { 0 }
								}
							} $else $if sub.unaliased_typ is int || sub.unaliased_typ is ?int {
								instance.$(field.name).$(sub.name) = match value {
									i8 { int(value) }
									i16 { int(value) }
									int { int(value) }
									i64 { int(value) }
									u8 { int(value) }
									u16 { int(value) }
									u32 { int(value) }
									u64 { int(value) }
									bool { int(value) }
									f32 { int(value) }
									f64 { int(value) }
									else { 0 }
								}
							} $else $if sub.unaliased_typ is i64 || sub.unaliased_typ is ?i64
								|| sub.unaliased_typ is $enum {
								instance.$(field.name).$(sub.name) = match value {
									i8 { i64(value) }
									i16 { i64(value) }
									int { i64(value) }
									i64 { i64(value) }
									u8 { i64(value) }
									u16 { i64(value) }
									u32 { i64(value) }
									u64 { i64(value) }
									bool { i64(value) }
									f32 { i64(value) }
									f64 { i64(value) }
									else { 0 }
								}
							} $else $if sub.unaliased_typ is u8 || sub.unaliased_typ is ?u8 {
								instance.$(field.name).$(sub.name) = match value {
									i8 { u8(value) }
									i16 { u8(value) }
									int { u8(value) }
									i64 { u8(value) }
									u8 { u8(value) }
									u16 { u8(value) }
									u32 { u8(value) }
									u64 { u8(value) }
									bool { u8(value) }
									f32 { u8(value) }
									f64 { u8(value) }
									else { 0 }
								}
							} $else $if sub.unaliased_typ is u16 || sub.unaliased_typ is ?u16 {
								instance.$(field.name).$(sub.name) = match value {
									i8 { u16(value) }
									i16 { u16(value) }
									int { u16(value) }
									i64 { u16(value) }
									u8 { u16(value) }
									u16 { u16(value) }
									u32 { u16(value) }
									u64 { u16(value) }
									bool { u16(value) }
									f32 { u16(value) }
									f64 { u16(value) }
									else { 0 }
								}
							} $else $if sub.unaliased_typ is u32 || sub.unaliased_typ is ?u32 {
								instance.$(field.name).$(sub.name) = match value {
									i8 { u32(value) }
									i16 { u32(value) }
									int { u32(value) }
									i64 { u32(value) }
									u8 { u32(value) }
									u16 { u32(value) }
									u32 { u32(value) }
									u64 { u32(value) }
									bool { u32(value) }
									f32 { u32(value) }
									f64 { u32(value) }
									else { 0 }
								}
							} $else $if sub.unaliased_typ is u64 || sub.unaliased_typ is ?u64 {
								instance.$(field.name).$(sub.name) = match value {
									i8 { u64(value) }
									i16 { u64(value) }
									int { u64(value) }
									i64 { u64(value) }
									u8 { u64(value) }
									u16 { u64(value) }
									u32 { u64(value) }
									u64 { u64(value) }
									bool { u64(value) }
									f32 { u64(value) }
									f64 { u64(value) }
									else { 0 }
								}
							} $else $if sub.unaliased_typ is f32 || sub.unaliased_typ is ?f32 {
								instance.$(field.name).$(sub.name) = match value {
									i8 { f32(value) }
									i16 { f32(value) }
									int { f32(value) }
									i64 { f32(value) }
									u8 { f32(value) }
									u16 { f32(value) }
									u32 { f32(value) }
									u64 { f32(value) }
									bool { f32(value) }
									f32 { value }
									f64 { f32(value) }
									else { 0 }
								}
							} $else $if sub.unaliased_typ is f64 || sub.unaliased_typ is ?f64 {
								instance.$(field.name).$(sub.name) = match value {
									i8 { f64(value) }
									i16 { f64(value) }
									int { f64(value) }
									i64 { f64(value) }
									u8 { f64(value) }
									u16 { f64(value) }
									u32 { f64(value) }
									u64 { f64(value) }
									bool { f64(value) }
									f32 { f64(value) }
									f64 { value }
									else { 0 }
								}
							} $else $if sub.unaliased_typ is bool || sub.unaliased_typ is ?bool {
								instance.$(field.name).$(sub.name) = match value {
									i8 { value != 0 }
									i16 { value != 0 }
									int { value != 0 }
									i64 { value != 0 }
									u8 { value != 0 }
									u16 { value != 0 }
									u32 { value != 0 }
									u64 { value != 0 }
									bool { value }
									f32 { value != 0 }
									f64 { value != 0 }
									else { false }
								}
							} $else $if sub.unaliased_typ is string || sub.unaliased_typ is ?string {
								instance.$(field.name).$(sub.name) = value as string
							} $else $if sub.unaliased_typ is time.Time
								|| sub.unaliased_typ is ?time.Time {
								if m.typ == time_ {
									instance.$(field.name).$(sub.name) = value as time.Time
								} else if m.typ == type_string {
									instance.$(field.name).$(sub.name) =
										time.parse(value as string)!
								}
							}
						}
					}
				}
			}
		} $else {
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
						$if field.unaliased_typ is i8 || field.unaliased_typ is ?i8 {
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
								f32 { i8(value) }
								f64 { i8(value) }
								else { 0 }
							}
						} $else $if field.unaliased_typ is i16 || field.unaliased_typ is ?i16 {
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
								f32 { i16(value) }
								f64 { i16(value) }
								else { 0 }
							}
						} $else $if field.unaliased_typ is int || field.unaliased_typ is ?int {
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
								f32 { int(value) }
								f64 { int(value) }
								else { 0 }
							}
						} $else $if field.unaliased_typ is i64 || field.unaliased_typ is ?i64
							|| field.unaliased_typ is $enum {
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
								f32 { i64(value) }
								f64 { i64(value) }
								else { 0 }
							}
						} $else $if field.unaliased_typ is u8 || field.unaliased_typ is ?u8 {
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
								f32 { u8(value) }
								f64 { u8(value) }
								else { 0 }
							}
						} $else $if field.unaliased_typ is u16 || field.unaliased_typ is ?u16 {
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
								f32 { u16(value) }
								f64 { u16(value) }
								else { 0 }
							}
						} $else $if field.unaliased_typ is u32 || field.unaliased_typ is ?u32 {
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
								f32 { u32(value) }
								f64 { u32(value) }
								else { 0 }
							}
						} $else $if field.unaliased_typ is u64 || field.unaliased_typ is ?u64 {
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
								f32 { u64(value) }
								f64 { u64(value) }
								else { 0 }
							}
						} $else $if field.unaliased_typ is f32 || field.unaliased_typ is ?f32 {
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
								f32 { value }
								f64 { f32(value) }
								else { 0 }
							}
						} $else $if field.unaliased_typ is f64 || field.unaliased_typ is ?f64 {
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
								f32 { f64(value) }
								f64 { value }
								else { 0 }
							}
						} $else $if field.unaliased_typ is bool || field.unaliased_typ is ?bool {
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
								f32 { value != 0 }
								f64 { value != 0 }
								else { false }
							}
						} $else $if field.unaliased_typ is string || field.unaliased_typ is ?string {
							instance.$(field.name) = value as string
						} $else $if field.unaliased_typ is time.Time
							|| field.unaliased_typ is ?time.Time {
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
	}
	parent_key := qb.selected_primary_value(row) or { Primitive(Null{}) }
	mut conn := qb.conn
	$for field in T.fields {
		field_type_name := typeof(field).name
		$if field.unaliased_typ is $array {
			fkey := orm_field_fkey(field.attrs)
			if fkey.len > 0 && parent_key != Primitive(Null{}) {
				instance.$(field.name) = query_relation_array_like(mut conn, parent_key, fkey,
					instance.$(field.name))!
			}
		} $else $if field.typ is $option {
			if orm_type_name_is_optional_array(field_type_name) {
				fkey := orm_field_fkey(field.attrs)
				if fkey.len > 0 && parent_key != Primitive(Null{}) {
					instance.$(field.name) = query_relation_optional_array_like(mut conn,
						parent_key, fkey, instance.$(field.name))!
				}
			} else if field.is_struct && !orm_type_name_is_time(field_type_name) {
				mm := qb.meta.filter(it.name == field.name)
				if mm.len != 0 {
					index := qb.config.fields.index(sql_field_name(mm[0]))
					if index >= 0 && index < row.len
						&& orm_relation_lookup_key_has_value(row[index]) {
						instance.$(field.name) = query_relation_one_optional_like(mut conn,
							row[index], instance.$(field.name))
					}
				}
			}
		} $else $if field.unaliased_typ is time.Time {
		} $else $if field.unaliased_typ is ?time.Time {
		} $else $if field.unaliased_typ is $struct {
			if !orm_type_name_is_time(field_type_name) {
				mm := qb.meta.filter(it.name == field.name)
				if mm.len != 0 {
					index := qb.config.fields.index(sql_field_name(mm[0]))
					if index >= 0 && index < row.len
						&& orm_relation_lookup_key_has_value(row[index]) {
						instance.$(field.name) = query_relation_one_like(mut conn, row[index],
							instance.$(field.name))!
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
	if qb.builder_error.len > 0 {
		return error(qb.builder_error)
	}

	// check for mismatch `(` and `)`
	for p in qb.where.parentheses {
		if p[1] == -1 {
			return error('${@FN}(): missing `)`')
		}
	}

	// auto fill field's names if not set by `select`
	if qb.config.fields.len == 0 {
		for field in qb.meta {
			if !field.is_arr {
				qb.config.fields << sql_field_name(field)
			}
		}
	}

	if qb.config.select_exprs.len != qb.config.fields.len {
		mut select_exprs := []string{cap: qb.config.fields.len}
		for f in qb.config.fields {
			mut select_expr := f
			for ff in qb.meta {
				if sql_field_name(ff) == f {
					select_expr = sql_field_select_expr(ff)
					break
				}
			}
			select_exprs << select_expr
		}
		qb.config.select_exprs = select_exprs
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

fn (qb &QueryBuilder[T]) get_meta_field_by_sql_name(field string) ?TableField {
	for meta_field in qb.meta {
		if sql_field_name(meta_field) == field {
			return meta_field
		}
	}
	return none
}

fn is_numeric_type_idx(typ int) bool {
	return typ in nums || typ in num64 || typ in float
}

fn is_min_max_supported_type_idx(typ int) bool {
	return is_numeric_type_idx(typ) || typ == type_string || typ == time_
}

fn (qb &QueryBuilder[T]) validate_aggregate_field(kind AggregateKind, field string) !TableField {
	meta_field := qb.get_meta_field_by_sql_name(field) or {
		return error("${@FN}(): table `${qb.config.table}` has no field's name: `${field}`")
	}
	match kind {
		.sum, .avg {
			if !is_numeric_type_idx(meta_field.typ) {
				msg := match kind {
					.sum { '${@FN}(): `sum` requires a numeric field' }
					.avg { '${@FN}(): `avg` requires a numeric field' }
					else { '${@FN}(): aggregate requires a numeric field' }
				}

				return error(msg)
			}
		}
		.min, .max {
			if !is_min_max_supported_type_idx(meta_field.typ) {
				msg := match kind {
					.min { '${@FN}(): `min` requires a numeric, string, or time.Time field' }
					.max { '${@FN}(): `max` requires a numeric, string, or time.Time field' }
					else { '${@FN}(): aggregate requires a numeric, string, or time.Time field' }
				}

				return error(msg)
			}
		}
		else {}
	}

	return meta_field
}

fn (qb &QueryBuilder[T]) build_aggregate_config(kind AggregateKind, field string) !SelectConfig {
	mut cfg := qb.config
	cfg.aggregate_kind = kind
	cfg.aggregate_field = ''
	cfg.fields = []
	cfg.types = []
	if kind == .count {
		cfg.types = [type_idx['int']]
		return cfg
	}

	meta_field := qb.validate_aggregate_field(kind, field)!
	cfg.aggregate_field = if cfg.joins.len > 0 {
		table_qualified_field(cfg.table.name, field)
	} else {
		field
	}
	cfg.fields = [field]
	cfg.types = [if kind == .avg { type_idx['f64'] } else { meta_field.typ }]
	return cfg
}

fn primitive_to_aggregate_value(value Primitive) AggregateValue {
	return if value == Primitive(Null{}) {
		AggregateValue{}
	} else {
		AggregateValue{
			has_value: true
			value:     value
		}
	}
}

// as_int returns the aggregate value as `int`, or `none` when it is null or not numeric.
pub fn (value AggregateValue) as_int() ?int {
	if !value.has_value {
		return none
	}
	return match value.value {
		i8 { int(value.value) }
		i16 { int(value.value) }
		int { value.value }
		i64 { int(value.value) }
		u8 { int(value.value) }
		u16 { int(value.value) }
		u32 { int(value.value) }
		u64 { int(value.value) }
		f32 { int(value.value) }
		f64 { int(value.value) }
		else { return none }
	}
}

// as_f64 returns the aggregate value as `f64`, or `none` when it is null or not numeric.
pub fn (value AggregateValue) as_f64() ?f64 {
	if !value.has_value {
		return none
	}
	return match value.value {
		i8 { f64(value.value) }
		i16 { f64(value.value) }
		int { f64(value.value) }
		i64 { f64(value.value) }
		u8 { f64(value.value) }
		u16 { f64(value.value) }
		u32 { f64(value.value) }
		u64 { f64(value.value) }
		f32 { f64(value.value) }
		f64 { value.value }
		else { return none }
	}
}

// as_string returns the aggregate value as `string`, or `none` when it is null or not a string.
pub fn (value AggregateValue) as_string() ?string {
	if !value.has_value {
		return none
	}
	return match value.value {
		string { value.value }
		else { return none }
	}
}

// as_time returns the aggregate value as `time.Time`, or `none` when it is null or not a time.
pub fn (value AggregateValue) as_time() ?time.Time {
	if !value.has_value {
		return none
	}
	return match value.value {
		time.Time { value.value }
		else { return none }
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
	qb.prepare()!
	count_config := qb.build_aggregate_config(.count, '')!
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

// sum returns the sum of the field values as an `AggregateValue`.
pub fn (qb_ &QueryBuilder[T]) sum(field string) !AggregateValue {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	qb.prepare()!
	qb.validate_aggregate_field(.sum, field)!
	cfg := qb.build_aggregate_config(.sum, field)!
	result := qb.conn.select(cfg, qb.data, qb.where)!
	if result.len == 0 || result[0].len == 0 {
		return AggregateValue{}
	}
	return primitive_to_aggregate_value(result[0][0])
}

// min returns the smallest field value as an `AggregateValue`.
pub fn (qb_ &QueryBuilder[T]) min(field string) !AggregateValue {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	qb.prepare()!
	qb.validate_aggregate_field(.min, field)!
	cfg := qb.build_aggregate_config(.min, field)!
	result := qb.conn.select(cfg, qb.data, qb.where)!
	if result.len == 0 || result[0].len == 0 {
		return AggregateValue{}
	}
	return primitive_to_aggregate_value(result[0][0])
}

// max returns the largest field value as an `AggregateValue`.
pub fn (qb_ &QueryBuilder[T]) max(field string) !AggregateValue {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	qb.prepare()!
	qb.validate_aggregate_field(.max, field)!
	cfg := qb.build_aggregate_config(.max, field)!
	result := qb.conn.select(cfg, qb.data, qb.where)!
	if result.len == 0 || result[0].len == 0 {
		return AggregateValue{}
	}
	return primitive_to_aggregate_value(result[0][0])
}

// avg returns the average field value as an `AggregateValue`.
pub fn (qb_ &QueryBuilder[T]) avg(field string) !AggregateValue {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	qb.prepare()!
	qb.validate_aggregate_field(.avg, field)!
	cfg := qb.build_aggregate_config(.avg, field)!
	result := qb.conn.select(cfg, qb.data, qb.where)!
	if result.len == 0 || result[0].len == 0 {
		return AggregateValue{}
	}
	return primitive_to_aggregate_value(result[0][0])
}

// insert insert a record into the database
pub fn (qb_ &QueryBuilder[T]) insert[T](value T) !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	_ = qb.insert_value_with_fields(value, []string{})!
	return qb
}

fn (qb_ &QueryBuilder[T]) v_sql_insert_and_last_id(value T) !int {
	mut qb := unsafe { qb_ }
	_ = qb.insert_value_with_fields(value, []string{})!
	return qb.conn.last_id()
}

fn (qb_ &QueryBuilder[T]) v_sql_insert_with_fields_and_last_id(value T, initialized_fields []string) !int {
	mut qb := unsafe { qb_ }
	_ = qb.insert_value_with_sql_fields(value, initialized_fields)!
	return qb.conn.last_id()
}

fn (qb_ &QueryBuilder[T]) v_sql_insert_many_and_last_id(values []T) !int {
	mut qb := unsafe { qb_ }
	qb.insert_many(values)!
	return qb.conn.last_id()
}

fn (qb_ &QueryBuilder[T]) v_sql_create_and_zero() !int {
	qb_.create()!
	return 0
}

fn (qb_ &QueryBuilder[T]) v_sql_drop_and_zero() !int {
	qb_.drop()!
	return 0
}

fn v_sql_create_table[T](conn Connection, attrs []VAttribute) !int {
	mut qb := new_query[T](conn)
	for attr in attrs {
		if attr_name_matches(attr.name, 'table') && attr.has_arg {
			qb.config.table.name = trim_attr_arg(attr.arg)
			break
		}
	}
	qb.conn.create(qb.config.table, qb.meta)!
	return 0
}

fn v_sql_drop_table[T](conn Connection, attrs []VAttribute) !int {
	mut qb := new_query[T](conn)
	for attr in attrs {
		if attr_name_matches(attr.name, 'table') && attr.has_arg {
			qb.config.table.name = trim_attr_arg(attr.arg)
			break
		}
	}
	qb.conn.drop(qb.config.table)!
	return 0
}

fn (qb_ &QueryBuilder[T]) v_sql_upsert_and_last_id(value T, initialized_fields []string) !int {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	qb.prepare()!
	data := filter_sql_insert_default_fields(qb.fill_insert_data_with_fields(value,
		initialized_fields)!, qb.meta, initialized_fields)
	conflict_groups := qb.v_sql_upsert_conflict_groups()
	prepared := prepare_upsert(data, conflict_groups)
	if !prepared.valid {
		upsert_missing_conflict_error(qb.config.table)!
	}
	mut count_config := qb.build_aggregate_config(.count, '')!
	count_config.has_where = true
	count_result := qb.conn.select(count_config, QueryData{}, prepared.where)!
	count := upsert_count(count_result)
	if count == 0 {
		qb.conn.insert(qb.config.table, prepared.insert_data)!
	} else if count == 1 {
		qb.conn.update(qb.config.table, prepared.insert_data, prepared.where)!
	} else {
		upsert_ambiguous_error(qb.config.table)!
	}
	return qb.conn.last_id()
}

fn (qb &QueryBuilder[T]) v_sql_upsert_conflict_groups() [][]string {
	mut groups := [][]string{}
	for field in qb.meta {
		for attr in field.attrs {
			if attr_name_matches(attr.name, 'primary') || attr_name_matches(attr.name, 'unique') {
				groups << [sql_field_name(field)]
				break
			}
		}
	}
	for attr in qb.config.table.attrs {
		if attr_name_matches(attr.name, 'unique_key') && attr.has_arg {
			mut group := []string{}
			for field in trim_attr_arg(attr.arg).split(',') {
				name := field.trim_space()
				if name.len > 0 {
					group << qb.v_sql_field_name(name)
				}
			}
			if group.len > 0 {
				groups << group
			}
		}
	}
	return groups
}

// insert_many insert records into the database
// Uses batch INSERT for efficiency when inserting multiple records.
pub fn (qb_ &QueryBuilder[T]) insert_many[T](values []T) !&QueryBuilder[T] {
	mut qb := unsafe { qb_ }
	defer {
		qb.reset()
	}
	qb.prepare()!
	if values.len == 0 {
		return error('${@FN}(): `insert` need at least one record')
	}
	mut batch := qb.fill_insert_data_with_fields(values[0], []string{})!
	batch.batch_rows = values.len
	if batch.auto_fields.len > 0 {
		for value in values {
			_ = qb.insert_value_with_fields(value, []string{})!
		}
		return qb
	}
	for i in 1 .. values.len {
		next := qb.fill_insert_data_with_fields(values[i], []string{})!
		for d in next.data {
			batch.data << d
		}
	}
	qb.conn.insert(qb.config.table, batch)!
	for value in values {
		data := fill_data_with_struct[T](value, qb.meta)
		key := qb.inserted_primary_value(value, data)
		qb.insert_child_relations(value, key, []string{})!
	}
	return qb
}

// save updates all mapped fields in `value` using the struct primary key or `id` field.
pub fn save[T](conn Connection, value T) ! {
	mut qb := new_query[T](conn)
	data, where := build_save_query_data[T](qb.meta, qb.config.table.name, value)!
	qb.conn.update(qb.config.table, data, where)!
}

fn build_save_query_data[T](meta []TableField, table_name string, value T) !(QueryData, QueryData) {
	data := fill_data_with_struct[T](value, meta)
	if data.fields.len != data.data.len {
		return error('${@FN}(): table `${table_name}` contains fields that `save` cannot map automatically')
	}
	primary_field_name := find_save_primary_field_name(meta) or {
		return error('${@FN}(): table `${table_name}` needs a primary key or `id` field to use `save`')
	}
	mut update_data := QueryData{}
	mut where_data := QueryData{
		kinds: [.eq]
	}
	for i, field_name in data.fields {
		if field_name == primary_field_name {
			where_data.fields << field_name
			where_data.data << data.data[i]
			continue
		}
		update_data.fields << field_name
		update_data.data << data.data[i]
	}
	if where_data.fields.len == 0 {
		return error('${@FN}(): struct value is missing the primary key field `${primary_field_name}`')
	}
	if update_data.fields.len == 0 {
		return error('${@FN}(): no updatable fields were found for table `${table_name}`')
	}
	return update_data, where_data
}

fn find_save_primary_field_name(meta []TableField) ?string {
	for field in meta {
		for attr in field.attrs {
			if attr_name_matches(attr.name, 'primary') {
				return sql_field_name(field)
			}
		}
	}
	for field in meta {
		field_name := sql_field_name(field)
		if field.name == 'id' || field_name == 'id' {
			return field_name
		}
	}
	return none
}

fn primary_field_is_serial(meta []TableField, primary string) bool {
	for field in meta {
		if sql_field_name(field) == primary {
			return sql_field_type(field) == serial
		}
	}
	return false
}

fn orm_type_name_is_time(name string) bool {
	clean := name.trim_left('?')
	return clean == 'time.Time'
}

fn orm_type_name_is_optional_array(name string) bool {
	return name.starts_with('?[]')
}

fn orm_field_fkey(attrs []string) string {
	for attr in attrs {
		name, arg, has_arg := orm_attr_name_arg(attr)
		if has_arg && name == 'fkey' {
			return trim_attr_arg(arg)
		}
	}
	return ''
}

fn orm_field_sql_name(attrs []string, field_name string) string {
	for attr in attrs {
		name, arg, has_arg := orm_attr_name_arg(attr)
		if has_arg && name == 'sql' {
			sql_name := trim_attr_arg(arg)
			if sql_name.len > 0
				&& sql_name !in ['serial', 'i8', 'i16', 'int', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool', 'string'] {
				return sql_name
			}
		}
	}
	return field_name
}

fn orm_relation_field_enabled(field string, initialized_fields []string) bool {
	return initialized_fields.len == 0 || '*' in initialized_fields || field in initialized_fields
}

fn orm_field_has_default(field TableField) bool {
	if field.default_val != '' {
		return true
	}
	for attr in field.attrs {
		if attr_name_matches(attr.name, 'default') {
			return true
		}
	}
	return false
}

fn orm_field_has_reference(field TableField) bool {
	for attr in field.attrs {
		if attr_name_matches(attr.name, 'references') {
			return true
		}
	}
	return false
}

fn orm_sql_insert_field_initialized(field TableField, initialized_fields []string) bool {
	return '*' in initialized_fields || field.name in initialized_fields
		|| sql_field_name(field) in initialized_fields
}

fn filter_sql_insert_default_fields(data QueryData, meta []TableField, initialized_fields []string) QueryData {
	mut auto_fields := map[int]bool{}
	for index in data.auto_fields {
		auto_fields[index] = true
	}
	mut filtered := QueryData{
		batch_rows: data.batch_rows
		batch_key:  data.batch_key
	}
	for i, field_name in data.fields {
		mut skip := false
		for field in meta {
			if sql_field_name(field) == field_name {
				skip = (orm_field_has_default(field) || orm_field_has_reference(field))
					&& !orm_sql_insert_field_initialized(field, initialized_fields)
				break
			}
		}
		if skip {
			continue
		}
		new_index := filtered.fields.len
		filtered.fields << field_name
		if i < data.data.len {
			filtered.data << data.data[i]
		}
		if i < data.types.len {
			filtered.types << data.types[i]
		}
		if i < data.kinds.len {
			filtered.kinds << data.kinds[i]
		}
		if i < data.is_and.len {
			filtered.is_and << data.is_and[i]
		}
		if i in auto_fields {
			filtered.auto_fields << new_index
		}
	}
	filtered.parentheses = data.parentheses
	return filtered
}

fn (qb &QueryBuilder[T]) selected_primary_value(row []Primitive) ?Primitive {
	primary := find_save_primary_field_name(qb.meta) or { return none }
	index := qb.config.fields.index(primary)
	if index < 0 || index >= row.len {
		return none
	}
	return row[index]
}

fn (qb &QueryBuilder[T]) inserted_primary_value(value T, data QueryData) Primitive {
	mut conn := qb.conn
	primary := find_save_primary_field_name(qb.meta) or { return Primitive(conn.last_id()) }
	if primary_field_is_serial(qb.meta, primary) {
		return Primitive(conn.last_id())
	}
	index := data.fields.index(primary)
	if index >= 0 && index < data.data.len {
		return data.data[index]
	}
	return field_primitive_by_sql_name[T](value, primary)
}

fn field_primitive_by_sql_name[T](value T, sql_name string) Primitive {
	$for field in T.fields {
		if field.name == sql_name || orm_field_sql_name(field.attrs, field.name) == sql_name {
			return primitive_from_field_value(value.$(field.name))
		}
	}
	return Primitive(Null{})
}

fn v_sql_struct_primary_primitive[T](value T) Primitive {
	meta := struct_meta[T]()
	primary := find_save_primary_field_name(meta) or { return Primitive(Null{}) }
	return field_primitive_by_sql_name[T](value, primary)
}

fn v_sql_optional_struct_primary_primitive[T](value ?T) Primitive {
	if sub := value {
		return v_sql_struct_primary_primitive[T](sub)
	}
	return null_primitive
}

fn primitive_from_field_value[V](value V) Primitive {
	$if V is bool {
		return bool_to_primitive(value)
	} $else $if V is f32 {
		return f32_to_primitive(value)
	} $else $if V is f64 {
		return f64_to_primitive(value)
	} $else $if V is i8 {
		return i8_to_primitive(value)
	} $else $if V is i16 {
		return i16_to_primitive(value)
	} $else $if V is int {
		return int_to_primitive(value)
	} $else $if V is i64 {
		return i64_to_primitive(value)
	} $else $if V is u8 {
		return u8_to_primitive(value)
	} $else $if V is u16 {
		return u16_to_primitive(value)
	} $else $if V is u32 {
		return u32_to_primitive(value)
	} $else $if V is u64 {
		return u64_to_primitive(value)
	} $else $if V is string {
		return string_to_primitive(value)
	} $else $if V is time.Time {
		return time_to_primitive(value)
	} $else $if V is $enum {
		return i64_to_primitive(i64(value))
	} $else {
		return Primitive(Null{})
	}
}

fn set_field_from_primitive[T](mut value T, field_name string, primitive Primitive) {
	$for field in T.fields {
		if field.name == field_name || orm_field_sql_name(field.attrs, field.name) == field_name {
			$if field.typ is $option {
				if primitive == Primitive(Null{}) {
					value.$(field.name) = none
					return
				}
			}
			if primitive == Primitive(Null{}) {
				return
			}
			$if field.unaliased_typ is bool || field.unaliased_typ is ?bool {
				value.$(field.name) = primitive_to_bool(primitive)
			} $else $if field.unaliased_typ is f32 || field.unaliased_typ is ?f32 {
				value.$(field.name) = f32(primitive_to_f64(primitive))
			} $else $if field.unaliased_typ is f64 || field.unaliased_typ is ?f64 {
				value.$(field.name) = primitive_to_f64(primitive)
			} $else $if field.unaliased_typ is i8 || field.unaliased_typ is ?i8 {
				value.$(field.name) = i8(primitive_to_int(primitive))
			} $else $if field.unaliased_typ is i16 || field.unaliased_typ is ?i16 {
				value.$(field.name) = i16(primitive_to_int(primitive))
			} $else $if field.unaliased_typ is int || field.unaliased_typ is ?int {
				value.$(field.name) = primitive_to_int(primitive)
			} $else $if field.unaliased_typ is i64 || field.unaliased_typ is ?i64
				|| field.unaliased_typ is $enum {
				value.$(field.name) = i64(primitive_to_int(primitive))
			} $else $if field.unaliased_typ is u8 || field.unaliased_typ is ?u8 {
				value.$(field.name) = u8(primitive_to_int(primitive))
			} $else $if field.unaliased_typ is u16 || field.unaliased_typ is ?u16 {
				value.$(field.name) = u16(primitive_to_int(primitive))
			} $else $if field.unaliased_typ is u32 || field.unaliased_typ is ?u32 {
				value.$(field.name) = u32(primitive_to_int(primitive))
			} $else $if field.unaliased_typ is u64 || field.unaliased_typ is ?u64 {
				value.$(field.name) = u64(primitive_to_int(primitive))
			} $else $if field.unaliased_typ is string || field.unaliased_typ is ?string {
				value.$(field.name) = primitive_to_string(primitive)
			}
		}
	}
}

fn primitive_to_bool(value Primitive) bool {
	return match value {
		bool { value }
		else { primitive_to_int(value) != 0 }
	}
}

fn primitive_to_int(value Primitive) int {
	return match value {
		i8 { int(value) }
		i16 { int(value) }
		int { value }
		i64 { int(value) }
		u8 { int(value) }
		u16 { int(value) }
		u32 { int(value) }
		u64 { int(value) }
		bool { int(value) }
		f32 { int(value) }
		f64 { int(value) }
		else { 0 }
	}
}

fn primitive_to_f64(value Primitive) f64 {
	return match value {
		i8 { f64(value) }
		i16 { f64(value) }
		int { f64(value) }
		i64 { f64(value) }
		u8 { f64(value) }
		u16 { f64(value) }
		u32 { f64(value) }
		u64 { f64(value) }
		bool { f64(int(value)) }
		f32 { f64(value) }
		f64 { value }
		else { 0.0 }
	}
}

fn primitive_to_string(value Primitive) string {
	return match value {
		string { value }
		else { '' }
	}
}

fn orm_relation_lookup_key_has_value(value Primitive) bool {
	return match value {
		Null { false }
		string { value.len > 0 }
		i8 { value != 0 }
		i16 { value != 0 }
		int { value != 0 }
		i64 { value != 0 }
		u8 { value != 0 }
		u16 { value != 0 }
		u32 { value != 0 }
		u64 { value != 0 }
		f32 { value != 0 }
		f64 { value != 0 }
		else { true }
	}
}

fn primitive_for_field[U](value Primitive, field_name string) Primitive {
	$for field in U.fields {
		if field.name == field_name || orm_field_sql_name(field.attrs, field.name) == field_name {
			$if field.unaliased_typ is bool || field.unaliased_typ is ?bool {
				return Primitive(primitive_to_int(value) != 0)
			} $else $if field.unaliased_typ is f32 || field.unaliased_typ is ?f32 {
				return Primitive(f32(primitive_to_f64(value)))
			} $else $if field.unaliased_typ is f64 || field.unaliased_typ is ?f64 {
				return Primitive(primitive_to_f64(value))
			} $else $if field.unaliased_typ is i8 || field.unaliased_typ is ?i8 {
				return Primitive(i8(primitive_to_int(value)))
			} $else $if field.unaliased_typ is i16 || field.unaliased_typ is ?i16 {
				return Primitive(i16(primitive_to_int(value)))
			} $else $if field.unaliased_typ is int || field.unaliased_typ is ?int {
				return Primitive(primitive_to_int(value))
			} $else $if field.unaliased_typ is i64 || field.unaliased_typ is ?i64 {
				return Primitive(i64(primitive_to_int(value)))
			} $else $if field.unaliased_typ is u8 || field.unaliased_typ is ?u8 {
				return Primitive(u8(primitive_to_int(value)))
			} $else $if field.unaliased_typ is u16 || field.unaliased_typ is ?u16 {
				return Primitive(u16(primitive_to_int(value)))
			} $else $if field.unaliased_typ is u32 || field.unaliased_typ is ?u32 {
				return Primitive(u32(primitive_to_int(value)))
			} $else $if field.unaliased_typ is u64 || field.unaliased_typ is ?u64 {
				return Primitive(u64(primitive_to_int(value)))
			} $else $if field.unaliased_typ is string || field.unaliased_typ is ?string {
				return Primitive(primitive_to_string(value))
			} $else {
				return value
			}
		}
	}
	return value
}

fn query_relation_one[U](mut conn Connection, key Primitive) !U {
	mut qb := new_query[U](conn)
	primary := find_save_primary_field_name(qb.meta) or { return U{} }
	rows := qb.v_sql_where_primitive(primary, .eq, primitive_for_field[U](key, primary)).query()!
	if rows.len == 0 {
		return U{}
	}
	return rows[0]
}

fn query_relation_one_like[U](mut conn Connection, key Primitive, _ U) !U {
	return query_relation_one[U](mut conn, key)
}

fn query_relation_one_optional_like[U](mut conn Connection, key Primitive, _ ?U) ?U {
	$if U is time.Time {
		return none
	} $else $if U is $struct {
		return query_relation_one[U](mut conn, key) or { return none }
	} $else {
		return none
	}
}

fn query_relation_array[U](mut conn Connection, key Primitive, fkey string) ![]U {
	mut qb := new_query[U](conn)
	field_key := primitive_for_field[U](key, fkey)
	return qb.v_sql_where_primitive(fkey, .eq, field_key).query() or {
		if err.msg().contains('no such table') {
			return []U{}
		}
		return err
	}
}

fn query_relation_array_like[U](mut conn Connection, key Primitive, fkey string, _ []U) ![]U {
	return query_relation_array[U](mut conn, key, fkey)
}

fn query_relation_array_from_optional[U](mut conn Connection, key Primitive, fkey string, _ ?[]U) ![]U {
	return query_relation_array[U](mut conn, key, fkey)!
}

fn query_relation_optional_array_like[U](mut conn Connection, key Primitive, fkey string, value ?U) !U {
	$if U is $array {
		return query_relation_array_from_optional(mut conn, key, fkey, value)!
	}
	return U{}
}

fn insert_relation_one[U](mut conn Connection, value U, initialized_fields []string) !Primitive {
	mut qb := new_query[U](conn)
	return qb.insert_value_with_fields(value, initialized_fields)!
}

fn insert_optional_relation_one[U](mut conn Connection, value ?U, initialized_fields []string) !Primitive {
	$if U is time.Time {
		return Primitive(Null{})
	} $else $if U is $struct {
		if sub := value {
			return insert_relation_one[U](mut conn, sub, initialized_fields)!
		}
	}
	return Primitive(Null{})
}

fn insert_relation_array[U](mut conn Connection, values []U, key Primitive, fkey string) ! {
	field_key := primitive_for_field[U](key, fkey)
	for value in values {
		mut child := value
		set_field_from_primitive[U](mut child, fkey, field_key)
		_ = insert_relation_one[U](mut conn, child, []string{})!
	}
}

fn insert_optional_relation_array_values[U](mut conn Connection, values ?[]U, key Primitive, fkey string) ! {
	if items := values {
		insert_relation_array[U](mut conn, items, key, fkey)!
	}
}

fn insert_optional_relation_array[U](mut conn Connection, values ?U, key Primitive, fkey string) ! {
	$if U is $array {
		insert_optional_relation_array_values(mut conn, values, key, fkey)!
	}
}

fn (qb &QueryBuilder[T]) fill_insert_data_with_fields(value T, initialized_fields []string) !QueryData {
	mut data := fill_data_with_struct[T](value, qb.meta)
	mut conn := qb.conn
	$for field in T.fields {
		field_type_name := typeof(field).name
		$if field.unaliased_typ is time.Time {
		} $else $if field.unaliased_typ is ?time.Time {
		} $else $if field.typ is $option {
			if orm_relation_field_enabled(field.name, initialized_fields) && field.is_struct
				&& !orm_type_name_is_time(field_type_name) {
				index := data.fields.index(qb.v_sql_field_name(field.name))
				if index >= 0 && index < data.data.len {
					data.data[index] = insert_optional_relation_one(mut conn, value.$(field.name),
						[]string{})!
				}
			}
		} $else $if field.unaliased_typ is $struct {
			if orm_relation_field_enabled(field.name, initialized_fields)
				&& !orm_type_name_is_time(field_type_name) {
				index := data.fields.index(qb.v_sql_field_name(field.name))
				if index >= 0 && index < data.data.len {
					data.data[index] = insert_relation_one(mut conn, value.$(field.name),
						[]string{})!
				}
			}
		}
	}
	return data
}

fn (qb &QueryBuilder[T]) insert_child_relations(value T, key Primitive, initialized_fields []string) ! {
	mut conn := qb.conn
	$for field in T.fields {
		$if field.unaliased_typ is $array {
			if orm_relation_field_enabled(field.name, initialized_fields) {
				fkey := orm_field_fkey(field.attrs)
				if fkey.len > 0 {
					insert_relation_array(mut conn, value.$(field.name), key, fkey)!
				}
			}
		} $else $if field.typ is $option {
			if orm_type_name_is_optional_array(typeof(field).name)
				&& orm_relation_field_enabled(field.name, initialized_fields) {
				fkey := orm_field_fkey(field.attrs)
				if fkey.len > 0 {
					insert_optional_relation_array(mut conn, value.$(field.name), key, fkey)!
				}
			}
		}
	}
}

fn (qb_ &QueryBuilder[T]) insert_value_with_fields(value T, initialized_fields []string) !Primitive {
	mut qb := unsafe { qb_ }
	qb.prepare()!
	data := qb.fill_insert_data_with_fields(value, initialized_fields)!
	qb.conn.insert(qb.config.table, data)!
	key := qb.inserted_primary_value(value, data)
	qb.insert_child_relations(value, key, initialized_fields)!
	return key
}

fn (qb_ &QueryBuilder[T]) insert_value_with_sql_fields(value T, initialized_fields []string) !Primitive {
	mut qb := unsafe { qb_ }
	qb.prepare()!
	data := filter_sql_insert_default_fields(qb.fill_insert_data_with_fields(value,
		initialized_fields)!, qb.meta, initialized_fields)
	qb.conn.insert(qb.config.table, data)!
	key := qb.inserted_primary_value(value, data)
	qb.insert_child_relations(value, key, initialized_fields)!
	return key
}

fn append_field_value_data[V](mut qb QueryData, sql_f TableField, value V) {
	if sql_f.is_arr {
		return
	}
	sql_f_name := sql_field_name(sql_f)
	sql_f_type := sql_field_type(sql_f)

	if sql_f_type == serial {
		// `serial` should be auto field
		qb.auto_fields << qb.fields.len
	}
	qb.fields << sql_f_name

	$if V is bool {
		qb.data << bool_to_primitive(bool(value))
	} $else $if V is ?bool {
		if v := value {
			qb.data << bool_to_primitive(v)
		} else {
			qb.data << null_primitive
		}
	} $else $if V is f32 {
		qb.data << f32_to_primitive(f32(value))
	} $else $if V is ?f32 {
		if v := value {
			qb.data << f32_to_primitive(v)
		} else {
			qb.data << null_primitive
		}
	} $else $if V is f64 {
		qb.data << f64_to_primitive(f64(value))
	} $else $if V is ?f64 {
		if v := value {
			qb.data << f64_to_primitive(v)
		} else {
			qb.data << null_primitive
		}
	} $else $if V is i8 {
		qb.data << i8_to_primitive(i8(value))
	} $else $if V is ?i8 {
		if v := value {
			qb.data << i8_to_primitive(v)
		} else {
			qb.data << null_primitive
		}
	} $else $if V is i16 {
		qb.data << i16_to_primitive(i16(value))
	} $else $if V is ?i16 {
		if v := value {
			qb.data << i16_to_primitive(v)
		} else {
			qb.data << null_primitive
		}
	} $else $if V is int {
		qb.data << int_to_primitive(int(value))
	} $else $if V is ?int {
		if v := value {
			qb.data << int_to_primitive(v)
		} else {
			qb.data << null_primitive
		}
	} $else $if V is i64 {
		qb.data << i64_to_primitive(i64(value))
	} $else $if V is ?i64 {
		if v := value {
			qb.data << i64_to_primitive(v)
		} else {
			qb.data << null_primitive
		}
	} $else $if V is u8 {
		qb.data << u8_to_primitive(u8(value))
	} $else $if V is ?u8 {
		if v := value {
			qb.data << u8_to_primitive(v)
		} else {
			qb.data << null_primitive
		}
	} $else $if V is u16 {
		qb.data << u16_to_primitive(u16(value))
	} $else $if V is ?u16 {
		if v := value {
			qb.data << u16_to_primitive(v)
		} else {
			qb.data << null_primitive
		}
	} $else $if V is u32 {
		qb.data << u32_to_primitive(u32(value))
	} $else $if V is ?u32 {
		if v := value {
			qb.data << u32_to_primitive(v)
		} else {
			qb.data << null_primitive
		}
	} $else $if V is u64 {
		qb.data << u64_to_primitive(u64(value))
	} $else $if V is ?u64 {
		if v := value {
			qb.data << u64_to_primitive(v)
		} else {
			qb.data << null_primitive
		}
	} $else $if V is string {
		qb.data << string_to_primitive(string(value))
	} $else $if V is ?string {
		if v := value {
			qb.data << string_to_primitive(v)
		} else {
			qb.data << null_primitive
		}
	} $else $if V is time.Time {
		if sql_f_type == type_string {
			qb.data << string_to_primitive(value.format_ss())
		} else {
			qb.data << time_to_primitive(value)
		}
	} $else $if V is ?time.Time {
		if sql_f_type == type_string {
			if b_ := value {
				qb.data << Primitive(b_.format_ss())
			} else {
				qb.data << null_primitive
			}
		} else {
			if v := value {
				qb.data << time_to_primitive(v)
			} else {
				qb.data << null_primitive
			}
		}
	} $else $if V is $enum {
		qb.data << i64_to_primitive(i64(value))
	} $else {
		qb.data << null_primitive
	}
}

fn fill_data_with_struct[T](value T, meta []TableField) QueryData {
	mut qb := QueryData{}
	$for field in T.fields {
		$if field.is_embed {
			$for sub in field.typ.fields {
				sql_fields := meta.filter(it.name == '${field.name}.${sub.name}')
				if sql_fields.len == 1 {
					sql_f := sql_fields[0]
					if !sql_f.is_arr {
						sql_f_name := sql_field_name(sql_f)
						sql_f_type := sql_field_type(sql_f)

						if sql_f_type == serial {
							// `serial` should be auto field
							qb.auto_fields << qb.fields.len
						}
						qb.fields << sql_f_name

						$if sub.unaliased_typ is bool {
							qb.data << bool_to_primitive(bool(value.$(field.name).$(sub.name)))
						} $else $if sub.unaliased_typ is ?bool {
							qb.data << option_bool_to_primitive(value.$(field.name).$(sub.name))
						} $else $if sub.unaliased_typ is f32 {
							qb.data << f32_to_primitive(f32(value.$(field.name).$(sub.name)))
						} $else $if sub.unaliased_typ is ?f32 {
							qb.data << option_f32_to_primitive(value.$(field.name).$(sub.name))
						} $else $if sub.unaliased_typ is f64 {
							qb.data << f64_to_primitive(f64(value.$(field.name).$(sub.name)))
						} $else $if sub.unaliased_typ is ?f64 {
							qb.data << option_f64_to_primitive(value.$(field.name).$(sub.name))
						} $else $if sub.unaliased_typ is i8 {
							qb.data << i8_to_primitive(i8(value.$(field.name).$(sub.name)))
						} $else $if sub.unaliased_typ is ?i8 {
							qb.data << option_i8_to_primitive(value.$(field.name).$(sub.name))
						} $else $if sub.unaliased_typ is i16 {
							qb.data << i16_to_primitive(i16(value.$(field.name).$(sub.name)))
						} $else $if sub.unaliased_typ is ?i16 {
							qb.data << option_i16_to_primitive(value.$(field.name).$(sub.name))
						} $else $if sub.unaliased_typ is int {
							qb.data << int_to_primitive(int(value.$(field.name).$(sub.name)))
						} $else $if sub.unaliased_typ is ?int {
							qb.data << option_int_to_primitive(value.$(field.name).$(sub.name))
						} $else $if sub.unaliased_typ is i64 {
							qb.data << i64_to_primitive(i64(value.$(field.name).$(sub.name)))
						} $else $if sub.unaliased_typ is ?i64 {
							qb.data << option_i64_to_primitive(value.$(field.name).$(sub.name))
						} $else $if sub.unaliased_typ is u8 {
							qb.data << u8_to_primitive(u8(value.$(field.name).$(sub.name)))
						} $else $if sub.unaliased_typ is ?u8 {
							qb.data << option_u8_to_primitive(value.$(field.name).$(sub.name))
						} $else $if sub.unaliased_typ is u16 {
							qb.data << u16_to_primitive(u16(value.$(field.name).$(sub.name)))
						} $else $if sub.unaliased_typ is ?u16 {
							qb.data << option_u16_to_primitive(value.$(field.name).$(sub.name))
						} $else $if sub.unaliased_typ is u32 {
							qb.data << u32_to_primitive(u32(value.$(field.name).$(sub.name)))
						} $else $if sub.unaliased_typ is ?u32 {
							qb.data << option_u32_to_primitive(value.$(field.name).$(sub.name))
						} $else $if sub.unaliased_typ is u64 {
							qb.data << u64_to_primitive(u64(value.$(field.name).$(sub.name)))
						} $else $if sub.unaliased_typ is ?u64 {
							qb.data << option_u64_to_primitive(value.$(field.name).$(sub.name))
						} $else $if sub.unaliased_typ is string {
							qb.data << string_to_primitive(string(value.$(field.name).$(sub.name)))
						} $else $if sub.unaliased_typ is ?string {
							qb.data << option_string_to_primitive(value.$(field.name).$(sub.name))
						} $else $if sub.unaliased_typ is time.Time {
							if sql_f_type == type_string {
								qb.data << string_to_primitive(value.$(field.name).$(sub.name).format_ss())
							} else {
								qb.data << time_to_primitive(value.$(field.name).$(sub.name))
							}
						} $else $if sub.unaliased_typ is ?time.Time {
							if sql_f_type == type_string {
								b := value.$(field.name).$(sub.name)
								if b_ := b {
									qb.data << Primitive(b_.format_ss())
								} else {
									qb.data << null_primitive
								}
							} else {
								qb.data << option_time_to_primitive(value.$(field.name).$(sub.name))
							}
						} $else $if sub.unaliased_typ is $enum {
							qb.data << i64_to_primitive(i64(value.$(field.name).$(sub.name)))
						} $else {
							qb.data << null_primitive
						}
					}
				}
			}
		} $else {
			sql_fields := meta.filter(it.name == field.name)
			if sql_fields.len == 1 {
				append_field_value_data(mut qb, sql_fields[0], value.$(field.name))
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

// update_many updates multiple records by a key field, using batch CASE WHEN for efficiency.
// key_field is the column used to match rows (e.g. 'id').
// field_names selects which columns to update; if empty, all struct fields except key_field are updated.
pub fn update_many[T](mut conn Connection, values []T, key_field string, field_names ...string) ! {
	if values.len == 0 {
		return error('${@FN}(): need at least one record')
	}
	mut qb := new_query[T](conn)

	// Build the field list from the first value
	first := fill_data_with_struct[T](values[0], qb.meta)
	mut key_index := -1
	mut value_fields := []string{}
	mut value_indexes := []int{}

	for i, field in first.fields {
		if field == key_field {
			key_index = i
		} else if field_names.len == 0 || field in field_names {
			value_fields << field
			value_indexes << i
		}
	}

	if key_index < 0 {
		return error('${@FN}(): key field `${key_field}` not found in table `${qb.config.table.name}`')
	}

	if value_fields.len == 0 {
		return error('${@FN}(): no updatable fields found for table `${qb.config.table.name}`')
	}

	mut update_data := QueryData{
		fields:     value_fields
		batch_rows: values.len
		batch_key:  key_field
	}

	// Build data: per value_field, per row: [key_value, value_field_value]
	rows := values.map(fill_data_with_struct[T](it, qb.meta))

	for fj in value_indexes {
		for row in rows {
			if key_index < row.data.len {
				update_data.data << row.data[key_index]
			}
			if fj < row.data.len {
				update_data.data << row.data[fj]
			}
		}
	}

	// Build WHERE clause using IN
	mut key_values := []Primitive{}
	for row in rows {
		if key_index < row.data.len {
			key_values << row.data[key_index]
		}
	}

	mut where_data := QueryData{
		fields: [key_field]
		data:   [Primitive(key_values)]
		kinds:  [.in]
	}

	conn.update(qb.config.table, update_data, where_data)!
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
