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

pub type Primitive = Null
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
	| InfixType
	| []bool
	| []f32
	| []f64
	| []i16
	| []i64
	| []i8
	| []int
	| []string
	| []time.Time
	| []u16
	| []u32
	| []u64
	| []u8
	| []InfixType
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

pub enum AggregateKind {
	none
	count
	sum
	avg
	min
	max
}

// JoinType represents the type of SQL JOIN operation
pub enum JoinType {
	inner      // INNER JOIN - returns only matching rows
	left       // LEFT JOIN - returns all left rows, NULL for non-matching right
	right      // RIGHT JOIN - returns all right rows, NULL for non-matching left
	full_outer // FULL OUTER JOIN - returns all rows from both tables
}

fn (jt JoinType) to_str() string {
	return match jt {
		.inner { 'INNER JOIN' }
		.left { 'LEFT JOIN' }
		.right { 'RIGHT JOIN' }
		.full_outer { 'FULL OUTER JOIN' }
	}
}

// JoinConfig holds configuration for a JOIN clause in a SELECT query
pub struct JoinConfig {
pub mut:
	kind         JoinType
	table        Table
	on_left_col  string // Column from main table (e.g., 'user_id')
	on_right_col string // Column from joined table (e.g., 'id')
}

pub enum SQLDialect {
	default
	h2
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

fn (kind AggregateKind) to_str() string {
	return match kind {
		.none { '' }
		.count { 'COUNT(*)' }
		.sum { 'SUM' }
		.avg { 'AVG' }
		.min { 'MIN' }
		.max { 'MAX' }
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
	batch_rows  int
	batch_key   string
}

pub struct InfixType {
pub:
	name     string
	operator MathOperationKind
	right    Primitive
}

pub struct Table {
pub mut:
	name    string
	attrs   []VAttribute
	fields  []string // struct field names, used to skip scope filters that don't apply
	columns []string // SQL column names (parallel to fields), used for SQL generation
}

// new_table creates a Table with the given name and attributes.
// Prefer using this constructor over positional initialization,
// as new fields may be added to Table in future versions.
pub fn new_table(name string, attrs []VAttribute) Table {
	return Table{
		name:  name
		attrs: attrs
	}
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
// aggregate_kind - Select rows or return a single aggregate value
// has_where - Select all or use a where expr
// has_order - Order the results
// order - Name of the column which will be ordered
// order_type - Type of order (asc, desc)
// has_limit - Limits the output data
// primary - Name of the primary field
// has_offset - Add an offset to the result
// fields - Fields to select
// types - Types to select
// joins - JOIN clauses for this query
pub struct SelectConfig {
pub mut:
	table           Table
	aggregate_kind  AggregateKind
	aggregate_field string
	has_where       bool
	has_order       bool
	order           string
	order_type      OrderType
	has_limit       bool
	primary         string = 'id' // should be set if primary is different than 'id' and 'has_limit' is false
	has_offset      bool
	has_distinct    bool
	fields          []string
	select_exprs    []string
	types           []int
	joins           []JoinConfig // JOIN clauses for this query
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

// TransactionalConnection extends Connection with transaction primitives.
pub interface TransactionalConnection {
	Connection
mut:
	orm_begin() !
	orm_commit() !
	orm_rollback() !
	orm_savepoint(name string) !
	orm_rollback_to(name string) !
	orm_release_savepoint(name string) !
}

enum ScopeMode {
	where
	insert
}

fn table_ignores_data_scope(table Table) bool {
	for attr in table.attrs {
		if attr_name_matches(attr.name, 'unscoped') {
			return true
		}
	}
	return false
}

// DB implements orm.Connection with DataScope support.
// When the wrapped connection also implements TransactionalConnection,
// the DB will transparently proxy transaction methods (orm_begin, orm_commit, ...).
pub struct DB {
mut:
	conn Connection
pub:
	scope           DataScope
	skip_all_scopes bool
	skip_fields     []string // specific scope filter fields to skip, when skip_all_scopes is false
}

// DataScope holds the per-connection data scope configuration for automatic filtering.
pub struct DataScope {
pub:
	enabled bool = true
	filters []QueryFilter
}

// QueryFilter represents a single filter condition in a DataScope.
// `field` should normally be a struct field name rather than a SQL column name.
// When `Table.fields`/`Table.columns` metadata is available, it is resolved to the
// corresponding SQL column name at query time. If that metadata is unavailable,
// the ORM may fall back to using `field` directly as the SQL column name. In
// metadata-driven paths, unresolved fields are skipped for that table.
pub struct QueryFilter {
pub:
	field    string
	value    Primitive
	operator OperationKind = .eq
}

// new_db creates a new DB with DataScope applied.
pub fn new_db(conn Connection, scope DataScope) DB {
	return DB{
		conn:            conn
		scope:           scope
		skip_all_scopes: false
		skip_fields:     []
	}
}

// unscoped returns a new DB with the specified fields excluded from DataScope filtering.
// Call without arguments to skip ALL scope filters.
pub fn (db DB) unscoped(unscoped_fields ...string) DB {
	if unscoped_fields.len == 0 {
		return DB{
			conn:            db.conn
			scope:           db.scope
			skip_all_scopes: true
			skip_fields:     []
		}
	}
	return DB{
		conn:            db.conn
		scope:           db.scope
		skip_all_scopes: false
		skip_fields:     unscoped_fields.map(it)
	}
}

// table_field_to_column_map builds an O(1) lookup from struct field names
// to SQL column names.
fn table_field_to_column_map(table Table) map[string]string {
	mut m := map[string]string{}
	if table.columns.len > 0 && table.columns.len == table.fields.len {
		for j, field_name in table.fields {
			m[field_name] = table.columns[j]
		}
	}
	return m
}

// apply_data_scope applies DataScope filters to a WHERE QueryData and returns the scoped query data.
pub fn apply_data_scope(scope DataScope, table Table, where QueryData, scope_skip_fields []string) QueryData {
	return apply_scope_filters(scope, table, where, scope_skip_fields, .where)
}

// apply_data_scope_insert applies DataScope filters to an INSERT QueryData and returns the scoped query data.
pub fn apply_data_scope_insert(scope DataScope, table Table, data QueryData, scope_skip_fields []string) QueryData {
	return apply_scope_filters(scope, table, data, scope_skip_fields, .insert)
}

// apply_scope_filters is the common core shared by apply_data_scope and
// apply_data_scope_insert. In WHERE mode it also wraps original conditions
// in parentheses and appends is_and / kinds markers.
fn apply_scope_filters(scope DataScope, table Table, qd QueryData, scope_skip_fields []string, mode ScopeMode) QueryData {
	if !scope.enabled || scope.filters.len == 0 {
		return qd
	}
	if table_ignores_data_scope(table) {
		return qd
	}
	mut result := clone_query_data(qd)
	field_to_column := table_field_to_column_map(table)
	// Wrap original WHERE clause in parentheses once, before adding scope filters
	if mode == .where && result.fields.len > 1 {
		result.parentheses << [0, result.fields.len - 1]
	}
	for filter in scope.filters {
		if filter.field == '' || filter.field in result.fields {
			continue
		}
		if filter.field in scope_skip_fields {
			continue
		}
		if table.fields.len > 0 && filter.field !in table.fields {
			continue
		}
		// Resolve SQL column name from struct field name (O(1) via lookup map)
		mut column_name := filter.field
		if resolved := field_to_column[filter.field] {
			column_name = resolved
		}
		// Check deduplication against SQL column name
		if column_name in result.fields {
			continue
		}
		if mode == .where {
			result.is_and << true
		}
		result.fields << column_name.clone()
		if !filter.operator.is_unary() {
			result.data << filter.value
			result.types << primitive_type(filter.value)
		}
		if mode == .where {
			result.kinds << filter.operator
		}
	}
	return result
}

// primitive_type returns the type index for a Primitive value.
fn primitive_type(value Primitive) int {
	return match value {
		bool {
			type_idx['bool']
		}
		i8 {
			type_idx['i8']
		}
		i16 {
			type_idx['i16']
		}
		int {
			type_idx['int']
		}
		i64 {
			type_idx['i64']
		}
		u8 {
			type_idx['u8']
		}
		u16 {
			type_idx['u16']
		}
		u32 {
			type_idx['u32']
		}
		u64 {
			type_idx['u64']
		}
		f32 {
			type_idx['f32']
		}
		f64 {
			type_idx['f64']
		}
		string {
			type_string
		}
		time.Time {
			time_
		}
		Null {
			type_idx['int']
		}
		InfixType {
			primitive_type(value.right)
		}
		[]Primitive {
			if value.len > 0 {
				primitive_type(value[0])
			} else {
				type_idx['int']
			}
		}
		[]bool {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
		[]f32 {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
		[]f64 {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
		[]i16 {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
		[]i64 {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
		[]i8 {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
		[]int {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
		[]string {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
		[]time.Time {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
		[]u16 {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
		[]u32 {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
		[]u64 {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
		[]u8 {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
		[]InfixType {
			if value.len > 0 {
				primitive_type(Primitive(value[0]))
			} else {
				type_idx['int']
			}
		}
	}
}

fn trim_attr_arg(arg string) string {
	mut out := arg.trim_space()
	if out.len >= 2 && ((out.starts_with("'") && out.ends_with("'"))
		|| (out.starts_with('"') && out.ends_with('"'))) {
		out = out[1..out.len - 1].trim_space()
	}
	return out
}

fn attr_name_matches(name string, expected string) bool {
	return name == expected || name.ends_with('.${expected}')
}

// DB implements orm.Connection ------------------------------------------------

// select fetches rows through the wrapped connection, with DataScope applied.
pub fn (mut db DB) select(config SelectConfig, data QueryData, where QueryData) ![][]Primitive {
	mut cfg := config
	if db.scope.enabled && db.scope.filters.len > 0 && !db.skip_all_scopes
		&& !table_ignores_data_scope(cfg.table) {
		where_scoped := apply_data_scope(db.scope, cfg.table, where, db.skip_fields)
		if where_scoped.fields.len > where.fields.len {
			cfg.has_where = true
		}
		return db.conn.select(cfg, data, where_scoped)
	}
	return db.conn.select(cfg, data, where)
}

// insert inserts rows through the wrapped connection, with DataScope applied.
pub fn (mut db DB) insert(table Table, data QueryData) ! {
	mut data_scoped := data
	if db.scope.enabled && db.scope.filters.len > 0 && !db.skip_all_scopes
		&& !table_ignores_data_scope(table) {
		data_scoped = apply_data_scope_insert(db.scope, table, data, db.skip_fields)
	}
	return db.conn.insert(table, data_scoped)
}

// update updates rows through the wrapped connection, with DataScope applied.
pub fn (mut db DB) update(table Table, data QueryData, where QueryData) ! {
	mut where_scoped := where
	if db.scope.enabled && db.scope.filters.len > 0 && !db.skip_all_scopes
		&& !table_ignores_data_scope(table) {
		where_scoped = apply_data_scope(db.scope, table, where, db.skip_fields)
	}
	return db.conn.update(table, data, where_scoped)
}

// delete deletes rows through the wrapped connection, with DataScope applied.
pub fn (mut db DB) delete(table Table, where QueryData) ! {
	mut where_scoped := where
	if db.scope.enabled && db.scope.filters.len > 0 && !db.skip_all_scopes
		&& !table_ignores_data_scope(table) {
		where_scoped = apply_data_scope(db.scope, table, where, db.skip_fields)
	}
	return db.conn.delete(table, where_scoped)
}

// create creates a table through the wrapped connection.
pub fn (mut db DB) create(table Table, fields []TableField) ! {
	return db.conn.create(table, fields)
}

// drop drops a table through the wrapped connection.
pub fn (mut db DB) drop(table Table) ! {
	return db.conn.drop(table)
}

// last_id returns the last inserted id from the wrapped connection.
pub fn (mut db DB) last_id() int {
	return db.conn.last_id()
}

// DB implements orm.TransactionalConnection (decorator) -----------------------

// orm_begin begins a transaction on the underlying connection.
// Returns an error if the underlying connection does not support transactions.
pub fn (mut db DB) orm_begin() ! {
	if db.conn is TransactionalConnection {
		mut conn := db.conn
		mut tc := unsafe { &conn as TransactionalConnection }
		tc.orm_begin()!
	} else {
		return error('orm.DB: underlying connection does not support transactions')
	}
}

// orm_commit commits the current transaction on the underlying connection.
// Returns an error if the underlying connection does not support transactions.
pub fn (mut db DB) orm_commit() ! {
	if db.conn is TransactionalConnection {
		mut conn := db.conn
		mut tc := unsafe { &conn as TransactionalConnection }
		tc.orm_commit()!
	} else {
		return error('orm.DB: underlying connection does not support transactions')
	}
}

// orm_rollback rolls back the current transaction on the underlying connection.
// Returns an error if the underlying connection does not support transactions.
pub fn (mut db DB) orm_rollback() ! {
	if db.conn is TransactionalConnection {
		mut conn := db.conn
		mut tc := unsafe { &conn as TransactionalConnection }
		tc.orm_rollback()!
	} else {
		return error('orm.DB: underlying connection does not support transactions')
	}
}

// orm_savepoint creates a savepoint with the given name on the underlying connection.
// Returns an error if the underlying connection does not support transactions.
pub fn (mut db DB) orm_savepoint(name string) ! {
	if db.conn is TransactionalConnection {
		mut conn := db.conn
		mut tc := unsafe { &conn as TransactionalConnection }
		tc.orm_savepoint(name)!
	} else {
		return error('orm.DB: underlying connection does not support transactions')
	}
}

// orm_rollback_to rolls back to the named savepoint on the underlying connection.
// Returns an error if the underlying connection does not support transactions.
pub fn (mut db DB) orm_rollback_to(name string) ! {
	if db.conn is TransactionalConnection {
		mut conn := db.conn
		mut tc := unsafe { &conn as TransactionalConnection }
		tc.orm_rollback_to(name)!
	} else {
		return error('orm.DB: underlying connection does not support transactions')
	}
}

// orm_release_savepoint releases the named savepoint on the underlying connection.
// Returns an error if the underlying connection does not support transactions.
pub fn (mut db DB) orm_release_savepoint(name string) ! {
	if db.conn is TransactionalConnection {
		mut conn := db.conn
		mut tc := unsafe { &conn as TransactionalConnection }
		tc.orm_release_savepoint(name)!
	} else {
		return error('orm.DB: underlying connection does not support transactions')
	}
}

fn clone_query_data(data QueryData) QueryData {
	return QueryData{
		fields:      data.fields.clone()
		data:        data.data.clone()
		types:       data.types.clone()
		parentheses: data.parentheses.map(it.clone())
		kinds:       data.kinds.clone()
		auto_fields: data.auto_fields.clone()
		is_and:      data.is_and.clone()
		batch_rows:  data.batch_rows
		batch_key:   data.batch_key
	}
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
	insert_data := prepare_insert_query_data(data)

	match kind {
		.insert {
			row_count := if insert_data.batch_rows > 0 { insert_data.batch_rows } else { 1 }
			mut values := []string{}
			mut select_fields := []string{}
			are_values_empty := insert_data.fields.len == 0

			for column_name in insert_data.fields {
				select_fields << '${q}${column_name}${q}'
			}
			if !are_values_empty {
				for _ in 0 .. row_count {
					mut row_values := []string{}
					for _ in insert_data.fields {
						row_values << factory_insert_qm_value(num, qm, c)
						c++
					}
					values << '(${row_values.join(', ')})'
				}
			}

			str += 'INSERT INTO ${q}${table.name}${q} '

			if are_values_empty {
				if row_count == 1 && sql_dialect in [.sqlite, .pg, .h2] {
					str += 'DEFAULT VALUES'
				} else {
					str += '() VALUES '
					str += []string{len: row_count, init: '()'}.join(', ')
				}
			} else {
				str += '('
				str += select_fields.join(', ')
				str += ') VALUES '
				str += values.join(', ')
			}
		}
		.update {
			str += 'UPDATE ${q}${table.name}${q} SET '
			if data.batch_rows > 0 {
				for i, field in data.fields {
					str += '${q}${field}${q} = CASE ${q}${data.batch_key}${q} '
					for _ in 0 .. data.batch_rows {
						str += 'WHEN ${qm}'
						if num {
							str += '${c}'
							c++
						}
						str += ' THEN ${qm}'
						if num {
							str += '${c}'
							c++
						}
						str += ' '
					}
					str += 'ELSE ${q}${field}${q} END'
					if i < data.fields.len - 1 {
						str += ', '
					}
				}
			} else {
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
	returned_data := if kind == .insert { insert_data } else { data }

	return str, returned_data
}

fn prepare_insert_query_data(data QueryData) QueryData {
	mut prepared := QueryData{
		batch_rows:  data.batch_rows
		batch_key:   data.batch_key
		parentheses: data.parentheses.clone()
		is_and:      data.is_and.clone()
	}
	mut included_indexes := []int{}
	if data.batch_rows > 0 && data.fields.len > 0 {
		for i, column_name in data.fields {
			mut skip_auto_field := i in data.auto_fields
			if skip_auto_field {
				for row in 0 .. data.batch_rows {
					data_idx := row * data.fields.len + i
					if data_idx >= data.data.len
						|| !should_skip_insert_auto_field(data.data[data_idx]) {
						skip_auto_field = false
						break
					}
				}
			}
			if skip_auto_field {
				continue
			}
			prepared.fields << column_name
			if i < data.types.len {
				prepared.types << data.types[i]
			}
			if i < data.kinds.len {
				prepared.kinds << data.kinds[i]
			}
			if i in data.auto_fields {
				prepared.auto_fields << prepared.fields.len - 1
			}
			included_indexes << i
		}
		for row in 0 .. data.batch_rows {
			for i in included_indexes {
				data_idx := row * data.fields.len + i
				if data_idx < data.data.len {
					prepared.data << data.data[data_idx]
				}
			}
		}
		return prepared
	}
	for i, column_name in data.fields {
		if i >= data.data.len {
			prepared.fields << column_name
			if i < data.types.len {
				prepared.types << data.types[i]
			}
			if i < data.kinds.len {
				prepared.kinds << data.kinds[i]
			}
			if i in data.auto_fields {
				prepared.auto_fields << prepared.fields.len - 1
			}
			continue
		}
		if i in data.auto_fields && should_skip_insert_auto_field(data.data[i]) {
			continue
		}
		prepared.fields << column_name
		prepared.data << data.data[i]
		if i < data.types.len {
			prepared.types << data.types[i]
		}
		if i < data.kinds.len {
			prepared.kinds << data.kinds[i]
		}
		if i in data.auto_fields {
			prepared.auto_fields << prepared.fields.len - 1
		}
	}
	return prepared
}

fn should_skip_insert_auto_field(value Primitive) bool {
	mut x := value
	return match mut x {
		Null { true }
		string { x == '' }
		i8, i16, int, i64, u8, u16, u32, u64 { u64(x) == 0 }
		f32, f64 { f64(x) == 0 }
		time.Time { x == time.Time{} }
		bool { !x }
		else { false }
	}
}

fn build_upsert_where(data QueryData, conflict_groups [][]string) !QueryData {
	mut field_indexes := map[string]int{}
	for i, field in data.fields {
		field_indexes[field] = i
	}
	mut where := QueryData{}
	for group in conflict_groups {
		if group.len == 0 {
			continue
		}
		start := where.fields.len
		if start > 0 {
			where.is_and << false
		}
		for i, field_name in group {
			idx := field_indexes[field_name] or {
				return error('${@FN}(): missing conflict field `${field_name}` in upsert data')
			}
			if idx >= data.data.len {
				return error('${@FN}(): missing conflict value for `${field_name}` in upsert data')
			}
			where.fields << field_name
			where.data << data.data[idx]
			where.kinds << .eq
			if i > 0 {
				where.is_and << true
			}
		}
		if group.len > 1 {
			where.parentheses << [start, where.fields.len - 1]
		}
	}
	return where
}

fn upsert_conflict_groups(data QueryData, conflict_groups [][]string) [][]string {
	mut present_fields := map[string]bool{}
	for field in data.fields {
		present_fields[field] = true
	}
	mut usable := [][]string{}
	for group in conflict_groups {
		if group.len == 0 {
			continue
		}
		mut ok := true
		for field_name in group {
			if field_name !in present_fields {
				ok = false
				break
			}
		}
		if ok {
			usable << group
		}
	}
	return usable
}

pub struct UpsertData {
pub:
	valid bool
pub mut:
	insert_data QueryData
	where       QueryData
}

// prepare_upsert resolves the filtered insert data and the conflict `WHERE` clause for an upsert.
pub fn prepare_upsert(data QueryData, conflict_groups [][]string) UpsertData {
	insert_data := prepare_insert_query_data(data)
	usable_groups := upsert_conflict_groups(insert_data, conflict_groups)
	if usable_groups.len == 0 {
		return UpsertData{
			insert_data: insert_data
		}
	}
	where := build_upsert_where(insert_data, usable_groups) or {
		return UpsertData{
			insert_data: insert_data
		}
	}
	return UpsertData{
		valid:       true
		insert_data: insert_data
		where:       where
	}
}

// upsert_count converts a `select count(*)` ORM result into an integer count.
pub fn upsert_count(result [][]Primitive) int {
	if result.len == 0 || result[0].len == 0 {
		return 0
	}
	count_val := result[0][0]
	return match count_val {
		int { count_val }
		i64 { int(count_val) }
		u64 { int(count_val) }
		else { 0 }
	}
}

// upsert_missing_conflict_error returns the standard missing-conflict error for SQL upserts.
pub fn upsert_missing_conflict_error(table Table) ! {
	return error('upsert(): table `${table.name}` needs at least one primary or unique field with a concrete value')
}

// upsert_ambiguous_error returns the standard ambiguous-match error for SQL upserts.
pub fn upsert_ambiguous_error(table Table) ! {
	return error('upsert(): upsert on table `${table.name}` matched multiple rows')
}

// Generates an sql select stmt, from universal parameter
// orm - See SelectConfig
// q, num, qm, start_pos - see orm_stmt_gen
// where - See QueryData
pub fn orm_select_gen(cfg SelectConfig, q string, num bool, qm string, start_pos int, where QueryData) string {
	mut str := 'SELECT '

	if cfg.has_distinct {
		str += 'DISTINCT '
	}

	if cfg.aggregate_kind != .none {
		if cfg.aggregate_kind == .count {
			str += cfg.aggregate_kind.to_str()
		} else {
			str += '${cfg.aggregate_kind.to_str()}(${q}${cfg.aggregate_field}${q})'
		}
	} else {
		for i, field in cfg.fields {
			select_expr := if cfg.select_exprs.len > i && cfg.select_exprs[i] != '' {
				cfg.select_exprs[i]
			} else {
				field
			}
			if select_expr == field {
				str += '${q}${field}${q}'
			} else {
				str += select_expr
			}
			if i < cfg.fields.len - 1 {
				str += ', '
			}
		}
	}

	str += ' FROM ${q}${cfg.table.name}${q}'

	// Generate JOIN clauses
	for join in cfg.joins {
		str += ' ${join.kind.to_str()} ${q}${join.table.name}${q}'
		str += ' ON ${q}${cfg.table.name}${q}.${q}${join.on_left_col}${q}'
		str += ' = ${q}${join.table.name}${q}.${q}${join.on_right_col}${q}'
	}

	mut c := start_pos

	if cfg.has_where {
		str += ' WHERE '
		$if trace_orm_where ? {
			eprintln('> orm_select_gen: where.fields.len = ${where.fields.len}')
			eprintln('> orm_select_gen: where.kinds.len = ${where.kinds.len}')
			for i, field in where.fields {
				eprintln('> orm_select_gen: field[${i}] = ${field}')
			}
		}
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
fn parse_table_attr_fields(table Table, attr VAttribute, valid_sql_field_names []string) ![]string {
	if attr.arg == '' || attr.kind != .string {
		return error("${attr.name} attribute needs to be in the format [${attr.name}: 'f1, f2, f3']")
	}
	mut attr_fields := []string{}
	for raw_field_name in attr.arg.split(',') {
		field_name := raw_field_name.trim_space()
		if field_name == '' {
			return error("${attr.name} attribute needs to be in the format [${attr.name}: 'f1, f2, f3']")
		}
		if field_name !in valid_sql_field_names {
			return error("table `${table.name}` has no field's name: `${field_name}`")
		}
		if field_name !in attr_fields {
			attr_fields << field_name
		}
	}
	return attr_fields
}

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
	mut index_fields := []string{}
	mut unique_key_fields := [][]string{}

	valid_sql_field_names := fields.map(sql_field_name(it))

	for attr in table.attrs {
		match attr.name {
			'comment' {
				if attr.arg != '' && attr.kind == .string {
					table_comment = attr.arg.replace('"', '\\"')
				}
			}
			'index' {
				attr_fields := parse_table_attr_fields(table, attr, valid_sql_field_names) or {
					return err
				}
				for field_name in attr_fields {
					if field_name !in index_fields {
						index_fields << field_name
					}
				}
			}
			'unique_key' {
				attr_fields := parse_table_attr_fields(table, attr, valid_sql_field_names) or {
					return err
				}
				if attr_fields.len > 0 {
					unique_key_fields << attr_fields
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
		mut has_default := default_val != ''
		mut nullable := field.nullable
		mut is_unique := false
		mut is_skip := false
		mut unique_len := 0
		mut references_table := ''
		mut references_field := ''
		mut field_comment := ''
		mut field_name := sql_field_name(field)
		mut col_typ := sql_from_v(sql_field_type(field)) or {
			// Struct fields are treated as foreign key references, which requires a primary key
			if primary_typ == 0 {
				return error('struct field `${field_name}` in table `${table.name}` requires a primary key field for foreign key reference - add a field with [primary] attribute or use [sql: \'-\'] to skip this field')
			}
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
							if attr.arg !in unique {
								unique[attr.arg] = []string{}
							}
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
					has_default = true
					if default_val == '' {
						default_val = attr.arg.str()
					}
				}
				'references' {
					nullable = true
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
				'index' {
					if field_name !in index_fields {
						index_fields << field_name
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
		if defaults && has_default {
			if default_val != '' {
				stmt += ' DEFAULT ${default_val}'
			} else {
				// Handle @[default: ''] - explicitly set DEFAULT '' for the column
				stmt += " DEFAULT ''"
			}
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
	for key_fields in unique_key_fields {
		mut tmp := []string{}
		for field_name in key_fields {
			tmp << '${q}${field_name}${q}'
		}
		fs << 'UNIQUE(${tmp.join(', ')})'
	}

	if primary != '' {
		fs << 'PRIMARY KEY(${q}${primary}${q})'
	}

	fs << unique_fields
	unique_fields.clear() // ownership transferred to fs to avoid double-free under -autofree
	str += fs.join(', ')
	if index_fields.len > 0 && sql_dialect == .mysql {
		str += ', INDEX `idx_${table.name}` (`'
		str += index_fields.join('`,`')
		str += '`)'
	}
	str += ')'
	if sql_dialect == .mysql && table_comment != '' {
		str += " COMMENT = '${table_comment}'"
	}
	str += ';'

	if sql_dialect in [.pg, .h2] {
		if table_comment != '' {
			str += "\nCOMMENT ON TABLE \"${table.name}\" IS '${table_comment}';"
		}
		for f, c in field_comments {
			str += "\nCOMMENT ON COLUMN \"${table.name}\".\"${f}\" IS '${c}';"
		}
	}
	if sql_dialect in [.pg, .sqlite, .h2] && index_fields.len > 0 {
		str += '\nCREATE INDEX "idx_${table.name}" ON "${table.name}" ("'
		str += index_fields.join('","')
		str += '");'
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

// Get's the SQL select expression for a field.
fn sql_field_select_expr(field TableField) string {
	for attr in field.attrs {
		if attr.name == 'sql_select' && attr.has_arg {
			return trim_attr_arg(attr.arg)
		}
	}
	return sql_field_name(field)
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
