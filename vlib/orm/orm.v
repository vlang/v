module orm

import time

pub const (
	num64       = [typeof[i64]().idx, typeof[u64]().idx]
	nums        = [
		typeof[i8]().idx,
		typeof[i16]().idx,
		typeof[int]().idx,
		typeof[u8]().idx,
		typeof[u16]().idx,
		typeof[u32]().idx,
		typeof[bool]().idx,
	]
	float       = [
		typeof[f32]().idx,
		typeof[f64]().idx,
	]
	type_string = typeof[string]().idx
	time        = -2
	serial      = -1
	type_idx    = {
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
	string_max_len = 2048
)

pub type Primitive = InfixType
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

pub enum OperationKind {
	neq // !=
	eq // ==
	gt // >
	lt // <
	ge // >=
	le // <=
	orm_like // LIKE
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
	sqlite
}

fn (kind OperationKind) to_str() string {
	str := match kind {
		.neq { '!=' }
		.eq { '=' }
		.gt { '>' }
		.lt { '<' }
		.ge { '>=' }
		.le { '<=' }
		.orm_like { 'LIKE' }
	}
	return str
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
pub struct QueryData {
pub:
	fields              []string
	data                []Primitive
	types               []int
	parentheses         [][]int
	kinds               []OperationKind
	primary_column_name string
	is_and              []bool
}

pub struct InfixType {
pub:
	name     string
	operator MathOperationKind
	right    Primitive
}

pub struct TableField {
pub:
	name        string
	typ         int
	is_time     bool
	default_val string
	is_arr      bool
	attrs       []StructAttribute
}

// table - Table name
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
pub:
	table      string
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
	@select(config SelectConfig, data QueryData, where QueryData) ![][]Primitive
	insert(table string, data QueryData) !
	update(table string, data QueryData, where QueryData) !
	delete(table string, where QueryData) !
	create(table string, fields []TableField) !
	drop(table string) !
	last_id() int
}

// Generates an sql stmt, from universal parameter
// q - The quotes character, which can be different in every type, so it's variable
// num - Stmt uses nums at prepared statements (? or ?1)
// qm - Character for prepared statement, qm because of quotation mark like in sqlite
// start_pos - When num is true, it's the start position of the counter
pub fn orm_stmt_gen(sql_dialect SQLDialect, table string, q string, kind StmtKind, num bool, qm string, start_pos int, data QueryData, where QueryData) (string, QueryData) {
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
				is_primary_column := column_name == data.primary_column_name

				if data.data.len > 0 {
					// Allow the database to insert an automatically generated primary key
					// under the hood if it is not passed by the user.
					tidx := data.data[i].type_idx()
					if is_primary_column && (tidx in orm.nums || tidx in orm.num64) {
						x := data.data[i]
						match x {
							i8, i16, int, i64, u8, u16, u32, u64 {
								if i64(x) == 0 {
									continue
								}
							}
							else {}
						}
					}

					match data.data[i].type_name() {
						'string' {
							if (data.data[i] as string).len == 0 {
								continue
							}
						}
						'time.Time' {
							if (data.data[i] as time.Time).unix == 0 {
								continue
							}
						}
						else {}
					}
					data_data << data.data[i]
				}
				select_fields << '${q}${column_name}${q}'
				values << factory_insert_qm_value(num, qm, c)
				data_fields << column_name
				c++
			}

			str += 'INSERT INTO ${q}${table}${q} '

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
			str += 'UPDATE ${q}${table}${q} SET '
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
			str += 'DELETE FROM ${q}${table}${q} WHERE '
		}
	}
	if kind == .update || kind == .delete {
		for i, field in where.fields {
			mut pre_par := false
			mut post_par := false
			for par in where.parentheses {
				if i in par {
					pre_par = par[0] == i
					post_par = par[1] == i
				}
			}
			if pre_par {
				str += '('
			}
			str += '${q}${field}${q} ${where.kinds[i].to_str()} ${qm}'
			if num {
				str += '${c}'
				c++
			}
			if post_par {
				str += ')'
			}
			if i < where.fields.len - 1 {
				if where.is_and[i] {
					str += ' AND '
				} else {
					str += ' OR '
				}
			}
		}
	}
	str += ';'
	$if trace_orm_stmt ? {
		eprintln('> orm_stmt sql_dialect: ${sql_dialect} | table: ${table} | kind: ${kind} | query: ${str}')
	}
	$if trace_orm ? {
		eprintln('> orm: ${str}')
	}

	return str, QueryData{
		fields: data_fields
		data: data_data
		types: data.types
		kinds: data.kinds
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

	str += ' FROM ${q}${cfg.table}${q}'

	mut c := start_pos

	if cfg.has_where {
		str += ' WHERE '
		for i, field in where.fields {
			mut pre_par := false
			mut post_par := false
			for par in where.parentheses {
				if i in par {
					pre_par = par[0] == i
					post_par = par[1] == i
				}
			}
			if pre_par {
				str += '('
			}
			str += '${q}${field}${q} ${where.kinds[i].to_str()} ${qm}'
			if num {
				str += '${c}'
				c++
			}
			if post_par {
				str += ')'
			}
			if i < where.fields.len - 1 {
				if where.is_and[i] {
					str += ' AND '
				} else {
					str += ' OR '
				}
			}
		}
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

// Generates an sql table stmt, from universal parameter
// table - Table name
// q - see orm_stmt_gen
// defaults - enables default values in stmt
// def_unique_len - sets default unique length for texts
// fields - See TableField
// sql_from_v - Function which maps type indices to sql type names
// alternative - Needed for msdb
pub fn orm_table_gen(table string, q string, defaults bool, def_unique_len int, fields []TableField, sql_from_v fn (int) !string, alternative bool) !string {
	mut str := 'CREATE TABLE IF NOT EXISTS ${q}${table}${q} ('

	if alternative {
		str = 'IF NOT EXISTS (SELECT * FROM sysobjects WHERE name=${q}${table}${q} and xtype=${q}U${q}) CREATE TABLE ${q}${table}${q} ('
	}

	mut fs := []string{}
	mut unique_fields := []string{}
	mut unique := map[string][]string{}
	mut primary := ''
	mut primary_typ := 0

	for field in fields {
		if field.is_arr {
			continue
		}
		mut default_val := field.default_val
		mut no_null := false
		mut is_unique := false
		mut is_skip := false
		mut unique_len := 0
		mut field_name := sql_field_name(field)
		mut ctyp := sql_from_v(sql_field_type(field)) or {
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
					primary = field.name
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
				'nonull' {
					no_null = true
				}
				'skip' {
					is_skip = true
				}
				'sql_type' {
					if attr.kind != .string {
						return error("sql_type attribute needs to be string. Try [sql_type: '${attr.arg}'] instead of [sql_type: ${attr.arg}]")
					}
					ctyp = attr.arg
				}
				'default' {
					if attr.kind != .string {
						return error("default attribute needs to be string. Try [default: '${attr.arg}'] instead of [default: ${attr.arg}]")
					}
					if default_val == '' {
						default_val = attr.arg
					}
				}
				else {}
			}
		}
		if is_skip {
			continue
		}
		mut stmt := ''
		if ctyp == '' {
			return error('Unknown type (${field.typ}) for field ${field.name} in struct ${table}')
		}
		stmt = '${q}${field_name}${q} ${ctyp}'
		if defaults && default_val != '' {
			stmt += ' DEFAULT ${default_val}'
		}
		if no_null {
			stmt += ' NOT NULL'
		}
		if is_unique {
			mut f := 'UNIQUE(${q}${field_name}${q}'
			if ctyp == 'TEXT' && def_unique_len > 0 {
				if unique_len > 0 {
					f += '(${unique_len})'
				} else {
					f += '(${def_unique_len})'
				}
			}
			f += ')'
			unique_fields << f
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
	str += ');'
	$if trace_orm_create ? {
		eprintln('> orm_create table: ${table} | query: ${str}')
	}
	$if trace_orm ? {
		eprintln('> orm: ${str}')
	}

	return str
}

// Get's the sql field type
fn sql_field_type(field TableField) int {
	mut typ := field.typ
	if field.is_time {
		return -2
	}
	for attr in field.attrs {
		if attr.kind == .plain && attr.name == 'sql' && attr.arg != '' {
			if attr.arg.to_lower() == 'serial' {
				typ = -1
				break
			}
			typ = orm.type_idx[attr.arg]
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

pub fn bool_to_primitive(b bool) Primitive {
	return Primitive(b)
}

pub fn f32_to_primitive(b f32) Primitive {
	return Primitive(b)
}

pub fn f64_to_primitive(b f64) Primitive {
	return Primitive(b)
}

pub fn i8_to_primitive(b i8) Primitive {
	return Primitive(b)
}

pub fn i16_to_primitive(b i16) Primitive {
	return Primitive(b)
}

pub fn int_to_primitive(b int) Primitive {
	return Primitive(b)
}

// int_literal_to_primitive handles int literal value
pub fn int_literal_to_primitive(b int) Primitive {
	return Primitive(b)
}

// float_literal_to_primitive handles float literal value
pub fn float_literal_to_primitive(b f64) Primitive {
	return Primitive(b)
}

pub fn i64_to_primitive(b i64) Primitive {
	return Primitive(b)
}

pub fn u8_to_primitive(b u8) Primitive {
	return Primitive(b)
}

pub fn u16_to_primitive(b u16) Primitive {
	return Primitive(b)
}

pub fn u32_to_primitive(b u32) Primitive {
	return Primitive(b)
}

pub fn u64_to_primitive(b u64) Primitive {
	return Primitive(b)
}

pub fn string_to_primitive(b string) Primitive {
	return Primitive(b)
}

pub fn time_to_primitive(b time.Time) Primitive {
	return Primitive(b)
}

pub fn infix_to_primitive(b InfixType) Primitive {
	return Primitive(b)
}

fn factory_insert_qm_value(num bool, qm string, c int) string {
	if num {
		return '${qm}${c}'
	} else {
		return '${qm}'
	}
}
