module orm

import time

pub const num64 = [typeof[i64]().idx, typeof[u64]().idx]
pub const nums = [
	typeof[i8]().idx,
	typeof[i16]().idx,
	typeof[int]().idx,
	typeof[u8]().idx,
	typeof[u16]().idx,
	typeof[u32]().idx,
	typeof[bool]().idx,
]
pub const float = [
	typeof[f32]().idx,
	typeof[f64]().idx,
]
pub const type_string = typeof[string]().idx
pub const serial = -1
pub const time_ = -2
pub const enum_ = -3
pub const type_idx = {
	'i8':     typeof[i8]().idx
	'i16':    typeof[i16]().idx
	'int':    typeof[int]().idx
	'i64':    typeof[i64]().idx
	'u8':     typeof[u8]().idx
	'u16':    typeof[u16]().idx
	'u32':    typeof[u32]().idx
	'u64':    typeof[u64]().idx
	'f32':    typeof[f32]().idx
	'f64':    typeof[f64]().idx
	'bool':   typeof[bool]().idx
	'string': typeof[string]().idx
}
pub const string_max_len = 2048
pub const null_primitive = Primitive(Null{})

pub type Primitive = InfixType
	| Null
	| bool
	| f32
	| f64
	| i16
	| i64
	| i8
	| int
	| string
	| time.Time
	| u16
	| u32
	| u64
	| u8
	| []Primitive

pub struct Null {}

pub enum OperationKind {
	neq         // !=
	eq          // ==
	gt          // >
	lt          // <
	ge          // >=
	le          // <=
	orm_like    // LIKE
	orm_ilike   // ILIKE
	is_null     // IS NULL
	is_not_null // IS NOT NULL
	in          // IN
	not_in      // NOT IN
}

pub enum MathOperationKind {
	add // +
	sub // -
	mul // *
	div // /
}

pub enum StmtKind {
	insert
	update
	delete
}

pub enum OrderType {
	asc
	desc
}

pub enum SQLDialect {
	default
	mysql
	pg
	sqlite
}

fn (kind OperationKind) to_str() string {
	str := match kind {
		// While most SQL databases support "!=" for not equal, "<>" is the standard
		// operator.
		.neq { '<>' }
		.eq { '=' }
		.gt { '>' }
		.lt { '<' }
		.ge { '>=' }
		.le { '<=' }
		.orm_like { 'LIKE' }
		.orm_ilike { 'ILIKE' }
		.is_null { 'IS NULL' }
		.is_not_null { 'IS NOT NULL' }
		.in { 'IN' }
		.not_in { 'NOT IN' }
	}
	return str
}

fn (kind OperationKind) is_unary() bool {
	return kind in [.is_null, .is_not_null]
}

fn (kind OrderType) to_str() string {
	return match kind {
		.desc {
			'DESC'
		}
		.asc {
			'ASC'
		}
	}
}

// Examples for QueryData in SQL: abc == 3 && b == 'test'
// => fields[abc, b]; data[3, 'test']; types[index of int, index of string]; kinds[.eq, .eq]; is_and[true];
// Every field, data, type & kind of operation in the expr share the same index in the arrays
// is_and defines how they're addicted to each other either and or or
// parentheses defines which fields will be inside ()
// auto_fields are indexes of fields where db should generate a value when absent in an insert
pub struct QueryData {
pub mut:
	fields      []string
	data        []Primitive
	types       []int
	parentheses [][]int
	kinds       []OperationKind
	auto_fields []int
	is_and      []bool
}

pub struct InfixType {
pub:
	name     string
	operator MathOperationKind
	right    Primitive
}

pub struct Table {
pub mut:
	name  string
	attrs []VAttribute
}

pub struct TableField {
pub mut:
	name        string
	typ         int
	nullable    bool
	default_val string
	attrs       []VAttribute
	is_arr      bool
}

// table - Table struct
// is_count - Either the data will be returned or an integer with the count
// has_where - Select all or use a where expr
// has_order - Order the results
// order - Name of the column which will be ordered
// order_type - Type of order (asc, desc)
// has_limit - Limits the output data
// primary - Name of the primary field
// has_offset - Add an offset to the result
// fields - Fields to select
// types - Types to select
pub struct SelectConfig {
pub mut:
	table      Table
	is_count   bool
	has_where  bool
	has_order  bool
	order      string
	order_type OrderType
	has_limit  bool
	primary    string = 'id' // should be set if primary is different than 'id' and 'has_limit' is false
	has_offset bool
	fields     []string
	types      []int
}

// Interfaces gets called from the backend and can be implemented
// Since the orm supports arrays aswell, they have to be returned too.
// A row is represented as []Primitive, where the data is connected to the fields of the struct by their
// index. The indices are mapped with the SelectConfig.field array. This is the mapping for a struct.
// To have an array, there has to be an array of structs, basically [][]Primitive
//
// Every function without last_id() returns an optional, which returns an error if present
// last_id returns the last inserted id of the db
pub interface Connection {
mut:
	select(config SelectConfig, data QueryData, where QueryData) ![][]Primitive
	insert(table Table, data QueryData) !
	update(table Table, data QueryData, where QueryData) !
	delete(table Table, where QueryData) !
	create(table Table, fields []TableField) !
	drop(table Table) !
	last_id() int
}

// Generates an sql stmt, from universal parameter
// q - The quotes character, which can be different in every type, so it's variable
// num - Stmt uses nums at prepared statements (? or ?1)
// qm - Character for prepared statement (qm for question mark, as in sqlite)
// start_pos - When num is true, it's the start position of the counter
pub fn orm_stmt_gen(sql_dialect SQLDialect, table Table, q string, kind StmtKind, num bool, qm string,
	start_pos int, data QueryData, where QueryData) (string, QueryData) {
	mut str := ''
	mut c := start_pos
	mut data_fields := []string{}
	mut data_data := []Primitive{}

	match kind {
		.insert {
			mut values := []string{}
			mut select_fields := []string{}

			for i in 0 .. data.fields.len {
				column_name := data.fields[i]
				is_auto_field := i in data.auto_fields

				if data.data.len > 0 {
					// skip fields and allow the database to insert default and
					// serial (auto-increment) values where a default (or no)
					// value was provided
					if is_auto_field {
						mut x := data.data[i]
						skip_auto_field := match mut x {
							Null { true }
							string { x == '' }
							i8, i16, int, i64, u8, u16, u32, u64 { u64(x) == 0 }
							f32, f64 { f64(x) == 0 }
							time.Time { x == time.Time{} }
							bool { !x }
							else { false }
						}
						if skip_auto_field {
							continue
						}
					}

					data_data << data.data[i]
				}
				select_fields << '${q}${column_name}${q}'
				values << factory_insert_qm_value(num, qm, c)
				data_fields << column_name
				c++
			}

			str += 'INSERT INTO ${q}${table.name}${q} '

			are_values_empty := values.len == 0

			if sql_dialect == .sqlite && are_values_empty {
				str += 'DEFAULT VALUES'
			} else {
				str += '('
				str += select_fields.join(', ')
				str += ') VALUES ('
				str += values.join(', ')
				str += ')'
			}
		}
		.update {
			str += 'UPDATE ${q}${table.name}${q} SET '
			for i, field in data.fields {
				str += '${q}${field}${q} = '
				if data.data.len > i {
					d := data.data[i]
					if d is InfixType {
						op := match d.operator {
							.add {
								'+'
							}
							.sub {
								'-'
							}
							.mul {
								'*'
							}
							.div {
								'/'
							}
						}
						str += '${d.name} ${op} ${qm}'
					} else {
						str += '${qm}'
					}
				} else {
					str += '${qm}'
				}
				if num {
					str += '${c}'
					c++
				}
				if i < data.fields.len - 1 {
					str += ', '
				}
			}
			str += ' WHERE '
		}
		.delete {
			str += 'DELETE FROM ${q}${table.name}${q} WHERE '
		}
	}
	// where
	if kind == .update || kind == .delete {
		str += gen_where_clause(where, q, qm, num, mut &c)
	}
	str += ';'
	$if trace_orm_stmt ? {
		eprintln('> orm_stmt sql_dialect: ${sql_dialect} | table: ${table.name} | kind: ${kind} | query: ${str}')
	}
	$if trace_orm ? {
		eprintln('> orm: ${str}')
	}

	return str, QueryData{
		fields: data_fields
		data:   data_data
		types:  data.types
		kinds:  data.kinds
		is_and: data.is_and
	}
}

// Generates an sql select stmt, from universal parameter
// orm - See SelectConfig
// q, num, qm, start_pos - see orm_stmt_gen
// where - See QueryData
pub fn orm_select_gen(cfg SelectConfig, q string, num bool, qm string, start_pos int, where QueryData) string {
	mut str := 'SELECT '

	if cfg.is_count {
		str += 'COUNT(*)'
	} else {
		for i, field in cfg.fields {
			str += '${q}${field}${q}'
			if i < cfg.fields.len - 1 {
				str += ', '
			}
		}
	}

	str += ' FROM ${q}${cfg.table.name}${q}'

	mut c := start_pos

	if cfg.has_where {
		str += ' WHERE '
		str += gen_where_clause(where, q, qm, num, mut &c)
	}

	// Note: do not order, if the user did not want it explicitly,
	// ordering is *slow*, especially if there are no indexes!
	if cfg.has_order {
		str += ' ORDER BY '
		str += '${q}${cfg.order}${q} '
		str += cfg.order_type.to_str()
	}

	if cfg.has_limit {
		str += ' LIMIT ${qm}'
		if num {
			str += '${c}'
			c++
		}
	}

	if cfg.has_offset {
		str += ' OFFSET ${qm}'
		if num {
			str += '${c}'
			c++
		}
	}

	str += ';'
	$if trace_orm_query ? {
		eprintln('> orm_query: ${str}')
	}
	$if trace_orm ? {
		eprintln('> orm: ${str}')
	}
	return str
}

fn gen_where_clause(where QueryData, q string, qm string, num bool, mut c &int) string {
	mut str := ''

	for i, field in where.fields {
		current_pre_par := where.parentheses.count(it[0] == i)
		current_post_par := where.parentheses.count(it[1] == i)

		if current_pre_par > 0 {
			str += ' ( '.repeat(current_pre_par)
		}
		str += '${q}${field}${q} ${where.kinds[i].to_str()}'
		if !where.kinds[i].is_unary() {
			if where.data.len > i && where.data[i] is []Primitive {
				len := (where.data[i] as []Primitive).len
				mut tmp := []string{len: len}
				for j in 0 .. len {
					tmp[j] = '${qm}'
					if num {
						tmp[j] += '${c}'
						c++
					}
				}
				str += ' (${tmp.join(', ')})'
			} else {
				str += ' ${qm}'
				if num {
					str += '${c}'
					c++
				}
			}
		}
		if current_post_par > 0 {
			str += ' ) '.repeat(current_post_par)
		}
		if i < where.fields.len - 1 {
			if where.is_and[i] {
				str += ' AND '
			} else {
				str += ' OR '
			}
		}
	}
	return str
}

// Generates an sql table stmt, from universal parameter
// table - Table struct
// q - see orm_stmt_gen
// defaults - enables default values in stmt
// def_unique_len - sets default unique length for texts
// fields - See TableField
// sql_from_v - Function which maps type indices to sql type names
// alternative - Needed for msdb
pub fn orm_table_gen(sql_dialect SQLDialect, table Table, q string, defaults bool, def_unique_len int, fields []TableField, sql_from_v fn (int) !string,
	alternative bool) !string {
	mut str := 'CREATE TABLE IF NOT EXISTS ${q}${table.name}${q} ('

	if alternative {
		str = 'IF NOT EXISTS (SELECT * FROM sysobjects WHERE name=${q}${table.name}${q} and xtype=${q}U${q}) CREATE TABLE ${q}${table.name}${q} ('
	}

	mut fs := []string{}
	mut unique_fields := []string{}
	mut unique := map[string][]string{}
	mut primary := ''
	mut primary_typ := 0
	mut table_comment := ''
	mut field_comments := map[string]string{}

	for attr in table.attrs {
		match attr.name {
			'comment' {
				if attr.arg != '' && attr.kind == .string {
					table_comment = attr.arg.replace('"', '\\"')
				}
			}
			else {}
		}
	}

	for field in fields {
		if field.is_arr {
			continue
		}
		mut default_val := field.default_val
		mut nullable := field.nullable
		mut is_unique := false
		mut is_skip := false
		mut unique_len := 0
		mut references_table := ''
		mut references_field := ''
		mut field_comment := ''
		mut field_name := sql_field_name(field)
		mut col_typ := sql_from_v(sql_field_type(field)) or {
			field_name = '${field_name}_id'
			sql_from_v(primary_typ)!
		}
		for attr in field.attrs {
			match attr.name {
				'sql' {
					// [sql:'-']
					if attr.arg == '-' {
						is_skip = true
					}
				}
				'primary' {
					primary = field_name
					primary_typ = field.typ
				}
				'unique' {
					if attr.arg != '' {
						if attr.kind == .string {
							unique[attr.arg] << field_name
							continue
						} else if attr.kind == .number {
							unique_len = attr.arg.int()
							is_unique = true
							continue
						}
					}
					is_unique = true
				}
				'skip' {
					is_skip = true
				}
				'sql_type' {
					col_typ = attr.arg.str()
				}
				'default' {
					if default_val == '' {
						default_val = attr.arg.str()
					}
				}
				'references' {
					if attr.arg == '' {
						if field.name.ends_with('_id') {
							references_table = field.name.trim_right('_id')
							references_field = 'id'
						} else {
							return error("references attribute can only be implicit if the field name ends with '_id'")
						}
					} else {
						if attr.arg.trim(' ') == '' {
							return error("references attribute needs to be in the format [references], [references: 'tablename'], or [references: 'tablename(field_id)']")
						}
						if attr.arg.contains('(') {
							if ref_table, ref_field := attr.arg.split_once('(') {
								if !ref_field.ends_with(')') {
									return error("explicit references attribute should be written as [references: 'tablename(field_id)']")
								}
								references_table = ref_table
								references_field = ref_field[..ref_field.len - 1]
							}
						} else {
							references_table = attr.arg
							references_field = 'id'
						}
					}
				}
				'comment' {
					if attr.arg != '' && attr.kind == .string {
						field_comment = attr.arg.replace("'", "\\'")
						field_comments[field_name] = field_comment
					}
				}
				else {}
			}
		}
		if is_skip {
			continue
		}
		mut stmt := ''
		if col_typ == '' {
			return error('Unknown type (${field.typ}) for field ${field.name} in struct ${table.name}')
		}
		stmt = '${q}${field_name}${q} ${col_typ}'
		if defaults && default_val != '' {
			stmt += ' DEFAULT ${default_val}'
		}
		if sql_dialect == .mysql && field_comment != '' {
			stmt += " COMMENT '${field_comment}'"
		}
		if !nullable {
			stmt += ' NOT NULL'
		}
		if is_unique {
			mut f := 'UNIQUE(${q}${field_name}${q}'
			if col_typ == 'TEXT' && def_unique_len > 0 {
				if unique_len > 0 {
					f += '(${unique_len})'
				} else {
					f += '(${def_unique_len})'
				}
			}
			f += ')'
			unique_fields << f
		}
		if references_table != '' {
			stmt += ' REFERENCES ${q}${references_table}${q}(${q}${references_field}${q})'
		}
		fs << stmt
	}

	if unique.len > 0 {
		for k, v in unique {
			mut tmp := []string{}
			for f in v {
				tmp << '${q}${f}${q}'
			}
			fs << '/* ${k} */UNIQUE(${tmp.join(', ')})'
		}
	}

	if primary != '' {
		fs << 'PRIMARY KEY(${q}${primary}${q})'
	}

	fs << unique_fields
	str += fs.join(', ')
	str += ')'
	if sql_dialect == .mysql && table_comment != '' {
		str += " COMMENT = '${table_comment}'"
	}
	str += ';'

	if sql_dialect == .pg {
		if table_comment != '' {
			str += "\nCOMMENT ON TABLE \"${table.name}\" IS '${table_comment}';"
		}
		for f, c in field_comments {
			str += "\nCOMMENT ON COLUMN \"${table.name}\".\"${f}\" IS '${c}';"
		}
	}
	$if trace_orm_create ? {
		eprintln('> orm_create table: ${table.name} | query: ${str}')
	}
	$if trace_orm ? {
		eprintln('> orm: ${str}')
	}

	return str
}

// Get's the sql field type
fn sql_field_type(field TableField) int {
	mut typ := field.typ
	for attr in field.attrs {
		// @[serial]
		if attr.name == 'serial' && attr.kind == .plain && !attr.has_arg {
			typ = serial
			break
		}

		if attr.kind == .plain && attr.name == 'sql' && attr.arg != '' {
			// @[sql: serial]
			if attr.arg.to_lower() == 'serial' {
				typ = serial
				break
			}
			typ = type_idx[attr.arg]
			break
		}
	}
	return typ
}

// Get's the sql field name
fn sql_field_name(field TableField) string {
	mut name := field.name
	for attr in field.attrs {
		if attr.name == 'sql' && attr.has_arg && attr.kind == .string {
			name = attr.arg
			break
		}
	}
	return name
}

// needed for backend functions

fn bool_to_primitive(b bool) Primitive {
	return Primitive(b)
}

fn option_bool_to_primitive(b ?bool) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_bool_to_primitive(b []bool) Primitive {
	return Primitive(b.map(bool_to_primitive(it)))
}

fn f32_to_primitive(b f32) Primitive {
	return Primitive(b)
}

fn option_f32_to_primitive(b ?f32) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_f32_to_primitive(b []f32) Primitive {
	return Primitive(b.map(f32_to_primitive(it)))
}

fn f64_to_primitive(b f64) Primitive {
	return Primitive(b)
}

fn option_f64_to_primitive(b ?f64) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_f64_to_primitive(b []f64) Primitive {
	return Primitive(b.map(f64_to_primitive(it)))
}

fn i8_to_primitive(b i8) Primitive {
	return Primitive(b)
}

fn option_i8_to_primitive(b ?i8) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_i8_to_primitive(b []i8) Primitive {
	return Primitive(b.map(i8_to_primitive(it)))
}

fn i16_to_primitive(b i16) Primitive {
	return Primitive(b)
}

fn option_i16_to_primitive(b ?i16) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_i16_to_primitive(b []i16) Primitive {
	return Primitive(b.map(i16_to_primitive(it)))
}

fn int_to_primitive(b int) Primitive {
	return Primitive(b)
}

fn option_int_to_primitive(b ?int) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_int_to_primitive(b []int) Primitive {
	return Primitive(b.map(int_to_primitive(it)))
}

// int_literal_to_primitive handles int literal value
fn int_literal_to_primitive(b int) Primitive {
	return Primitive(b)
}

fn option_int_literal_to_primitive(b ?int) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_int_literal_to_primitive(b []int) Primitive {
	return Primitive(b.map(int_literal_to_primitive(it)))
}

// float_literal_to_primitive handles float literal value
fn float_literal_to_primitive(b f64) Primitive {
	return Primitive(b)
}

fn option_float_literal_to_primitive(b ?f64) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_float_literal_to_primitive(b []f64) Primitive {
	return Primitive(b.map(float_literal_to_primitive(it)))
}

fn i64_to_primitive(b i64) Primitive {
	return Primitive(b)
}

fn option_i64_to_primitive(b ?i64) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_i64_to_primitive(b []i64) Primitive {
	return Primitive(b.map(i64_to_primitive(it)))
}

fn u8_to_primitive(b u8) Primitive {
	return Primitive(b)
}

fn option_u8_to_primitive(b ?u8) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_u8_to_primitive(b []u8) Primitive {
	return Primitive(b.map(u8_to_primitive(it)))
}

fn u16_to_primitive(b u16) Primitive {
	return Primitive(b)
}

fn option_u16_to_primitive(b ?u16) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_u16_to_primitive(b []u16) Primitive {
	return Primitive(b.map(u16_to_primitive(it)))
}

fn u32_to_primitive(b u32) Primitive {
	return Primitive(b)
}

fn option_u32_to_primitive(b ?u32) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_u32_to_primitive(b []u32) Primitive {
	return Primitive(b.map(u32_to_primitive(it)))
}

fn u64_to_primitive(b u64) Primitive {
	return Primitive(b)
}

fn option_u64_to_primitive(b ?u64) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_u64_to_primitive(b []u64) Primitive {
	return Primitive(b.map(u64_to_primitive(it)))
}

fn string_to_primitive(b string) Primitive {
	return Primitive(b)
}

fn option_string_to_primitive(b ?string) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_string_to_primitive(b []string) Primitive {
	return Primitive(b.map(string_to_primitive(it)))
}

fn time_to_primitive(b time.Time) Primitive {
	return Primitive(b)
}

fn option_time_to_primitive(b ?time.Time) Primitive {
	return if b_ := b { Primitive(b_) } else { null_primitive }
}

fn array_time_to_primitive(b []time.Time) Primitive {
	return Primitive(b.map(time_to_primitive(it)))
}

fn infix_to_primitive(b InfixType) Primitive {
	return Primitive(b)
}

fn factory_insert_qm_value(num bool, qm string, c int) string {
	if num {
		return '${qm}${c}'
	} else {
		return '${qm}'
	}
}
