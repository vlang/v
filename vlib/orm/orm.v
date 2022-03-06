module orm

import time
import v.ast

pub const (
	num64    = [ast.i64_type_idx, ast.u64_type_idx]
	nums     = [
		ast.i8_type_idx,
		ast.i16_type_idx,
		ast.int_type_idx,
		ast.byte_type_idx,
		ast.u16_type_idx,
		ast.u32_type_idx,
		ast.bool_type_idx,
	]
	float    = [
		ast.f32_type_idx,
		ast.f64_type_idx,
	]
	string   = ast.string_type_idx
	time     = -2
	type_idx = {
		'i8':     ast.i8_type_idx
		'i16':    ast.i16_type_idx
		'int':    ast.int_type_idx
		'i64':    ast.i64_type_idx
		'byte':   ast.byte_type_idx
		'u16':    ast.u16_type_idx
		'u32':    ast.u32_type_idx
		'u64':    ast.u64_type_idx
		'f32':    ast.f32_type_idx
		'f64':    ast.f64_type_idx
		'bool':   ast.bool_type_idx
		'string': ast.string_type_idx
	}
	string_max_len = 2048
)

pub type Primitive = InfixType
	| bool
	| byte
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

pub enum OperationKind {
	neq // !=
	eq // ==
	gt // >
	lt // <
	ge // >=
	le // <=
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

fn (kind OperationKind) to_str() string {
	str := match kind {
		.neq { '!=' }
		.eq { '=' }
		.gt { '>' }
		.lt { '<' }
		.ge { '>=' }
		.le { '<=' }
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

pub struct QueryData {
pub:
	fields []string
	data   []Primitive
	types  []int
	kinds  []OperationKind
	is_and []bool
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

pub interface Connection {
	@select(config SelectConfig, data QueryData, where QueryData) ?[][]Primitive
	insert(table string, data QueryData) ?
	update(table string, data QueryData, where QueryData) ?
	delete(table string, where QueryData) ?
	create(table string, fields []TableField) ?
	drop(table string) ?
	last_id() Primitive
}

pub fn orm_stmt_gen(table string, para string, kind StmtKind, num bool, qm string, start_pos int, data QueryData, where QueryData) string {
	mut str := ''

	mut c := start_pos

	match kind {
		.insert {
			mut values := []string{}

			for _ in 0 .. data.fields.len {
				// loop over the length of data.field and generate ?0, ?1 or just ? based on the $num parameter for value placeholders
				if num {
					values << '$qm$c'
					c++
				} else {
					values << '$qm'
				}
			}

			str += 'INSERT INTO $para$table$para ('
			str += data.fields.map('$para$it$para').join(', ')
			str += ') VALUES ('
			str += values.join(', ')
			str += ')'
		}
		.update {
			str += 'UPDATE $para$table$para SET '
			for i, field in data.fields {
				str += '$para$field$para = '
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
						str += '$d.name $op $qm'
					} else {
						str += '$qm'
					}
				} else {
					str += '$qm'
				}
				if num {
					str += '$c'
					c++
				}
				if i < data.fields.len - 1 {
					str += ', '
				}
			}
			str += ' WHERE '
		}
		.delete {
			str += 'DELETE FROM $para$table$para WHERE '
		}
	}
	if kind == .update || kind == .delete {
		for i, field in where.fields {
			str += '$para$field$para ${where.kinds[i].to_str()} $qm'
			if num {
				str += '$c'
				c++
			}
			if i < where.fields.len - 1 {
				str += ' AND '
			}
		}
	}
	str += ';'
	return str
}

pub fn orm_select_gen(orm SelectConfig, para string, num bool, qm string, start_pos int, where QueryData) string {
	mut str := 'SELECT '

	if orm.is_count {
		str += 'COUNT(*)'
	} else {
		for i, field in orm.fields {
			str += '$para$field$para'
			if i < orm.fields.len - 1 {
				str += ', '
			}
		}
	}

	str += ' FROM $para$orm.table$para'

	mut c := start_pos

	if orm.has_where {
		str += ' WHERE '
		for i, field in where.fields {
			str += '$para$field$para ${where.kinds[i].to_str()} $qm'
			if num {
				str += '$c'
				c++
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
	if orm.has_order {
		str += ' ORDER BY '
		str += '$para$orm.order$para '
		str += orm.order_type.to_str()
	}

	if orm.has_limit {
		str += ' LIMIT $qm'
		if num {
			str += '$c'
			c++
		}
	}

	if orm.has_offset {
		str += ' OFFSET $qm'
		if num {
			str += '$c'
			c++
		}
	}

	str += ';'
	return str
}

pub fn orm_table_gen(table string, para string, defaults bool, def_unique_len int, fields []TableField, sql_from_v fn (int) ?string, alternative bool) ?string {
	mut str := 'CREATE TABLE IF NOT EXISTS $para$table$para ('

	if alternative {
		str = 'IF NOT EXISTS (SELECT * FROM sysobjects WHERE name=$para$table$para and xtype=${para}U$para) CREATE TABLE $para$table$para ('
	}

	mut fs := []string{}
	mut unique_fields := []string{}
	mut unique := map[string][]string{}
	mut primary := ''

	for field in fields {
		if field.is_arr {
			continue
		}
		mut no_null := false
		mut is_unique := false
		mut is_skip := false
		mut unique_len := 0
		// mut fkey := ''
		mut field_name := sql_field_name(field)
		for attr in field.attrs {
			match attr.name {
				'primary' {
					primary = field.name
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
				/*'fkey' {
					if attr.arg != '' {
						if attr.kind == .string {
							fkey = attr.arg
							continue
						}
					}
				}*/
				else {}
			}
		}
		if is_skip {
			continue
		}
		mut stmt := ''
		mut ctyp := sql_from_v(sql_field_type(field)) or {
			field_name = '${field_name}_id'
			sql_from_v(7) ?
		}
		if ctyp == '' {
			return error('Unknown type ($field.typ) for field $field.name in struct $table')
		}
		stmt = '$para$field_name$para $ctyp'
		if defaults && field.default_val != '' {
			stmt += ' DEFAULT $field.default_val'
		}
		if no_null {
			stmt += ' NOT NULL'
		}
		if is_unique {
			mut f := 'UNIQUE($para$field_name$para'
			if ctyp == 'TEXT' && def_unique_len > 0 {
				if unique_len > 0 {
					f += '($unique_len)'
				} else {
					f += '($def_unique_len)'
				}
			}
			f += ')'
			unique_fields << f
		}
		fs << stmt
	}
	if primary == '' {
		return error('A primary key is required for $table')
	}
	if unique.len > 0 {
		for k, v in unique {
			mut tmp := []string{}
			for f in v {
				tmp << '$para$f$para'
			}
			fs << '/* $k */UNIQUE(${tmp.join(', ')})'
		}
	}
	fs << 'PRIMARY KEY($para$primary$para)'
	fs << unique_fields
	str += fs.join(', ')
	str += ');'
	return str
}

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

pub fn i64_to_primitive(b i64) Primitive {
	return Primitive(b)
}

pub fn byte_to_primitive(b byte) Primitive {
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
