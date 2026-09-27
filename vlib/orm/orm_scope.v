module orm

import time

// DataScope provides per-instance request-level data filtering for ORM queries.
// It works with both `sql` block syntax and orm_func (QueryBuilder).
//
// Use `orm.new_db(raw_conn, scope)` to create a scoped connection, then pass it
// to `sql db { ... }` blocks or `orm.new_query[T](db)`. Call `db.unscoped()` or
// `db.unscoped('field')` to selectively skip scope filters.

// QueryFilterMode describes whether a DataScope filter has a stable SQL shape or
// needs runtime handling.
pub enum QueryFilterMode {
	unset // .static is not yet implemented — use .dynamic explicitly
	static
	dynamic
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
// `mode` must be explicitly set to .static or .dynamic. Static filters are
// reserved for future compiler-generated scope clauses. The runtime DB wrapper
// applies only filters explicitly marked with .dynamic.
pub struct QueryFilter {
pub:
	field    string
	value    Primitive
	operator OperationKind = .eq
	mode     QueryFilterMode // must be explicitly set to .static or .dynamic
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
pub fn apply_data_scope(scope DataScope, table Table, where QueryData, scope_skip_fields []string, has_joins bool) !QueryData {
	return apply_scope_filters(scope, table, where, scope_skip_fields, has_joins)
}

// apply_data_scope_insert applies DataScope filters to an INSERT QueryData and returns the scoped query data.
pub fn apply_data_scope_insert(scope DataScope, table Table, data QueryData, scope_skip_fields []string) !QueryData {
	return apply_scope_insert_filters(scope, table, data, scope_skip_fields)
}

// apply_scope_filters applies DataScope filters to WHERE data. It wraps original
// conditions in parentheses and appends is_and / kinds markers.
// When has_joins is true, scope filter column names are qualified with table.name
// to avoid ambiguity in JOIN queries where joined tables share column names.
fn apply_scope_filters(scope DataScope, table Table, qd QueryData, scope_skip_fields []string, has_joins bool) !QueryData {
	if !scope.enabled || scope.filters.len == 0 {
		return qd
	}
	if table_ignores_data_scope(table) {
		return qd
	}
	mut result := clone_query_data(qd)
	field_to_column := table_field_to_column_map(table)
	// Wrap original WHERE clause in parentheses once, before adding scope filters
	if result.fields.len > 1 {
		result.parentheses << [0, result.fields.len - 1]
	}
	for filter in scope.filters {
		if filter.mode == .unset {
			return error('orm.DataScope: QueryFilter.mode must be explicitly set. .static is not yet implemented — use .dynamic. Got .unset for field `${filter.field}`')
		}
		if filter.mode != .dynamic {
			continue
		}
		if filter.field == '' {
			return error('orm.DataScope: dynamic filter field must not be empty')
		}
		if filter.field in scope_skip_fields {
			continue
		}
		// Note: we do NOT skip when filter.field is already in result.fields.
		// The scope filter is always appended as an additional AND condition.
		// This prevents a user from bypassing tenant isolation by including the
		// scoped field in their own WHERE clause. The resolved SQL column name is
		// also appended without deduplication for the same reason.
		if table.fields.len > 0 && filter.field !in table.fields {
			continue
		}
		if !filter_value_matches_operator(filter) {
			return invalid_scope_filter_error(filter)
		}
		// Resolve SQL column name from struct field name (O(1) via lookup map)
		mut column_name := filter.field
		if resolved := field_to_column[filter.field] {
			column_name = resolved
		}
		// Qualify with table name when joins are present to avoid ambiguity
		if has_joins && table.name != '' {
			column_name = table_qualified_field(table.name, column_name)
		}
		// Note: we do NOT skip when column_name is already in result.fields.
		// The scope filter is always appended as an additional AND condition
		// to prevent bypassing tenant isolation.
		result.is_and << true
		result.fields << column_name.clone()
		if !filter.operator.is_unary() {
			result.data << filter.value
			result.types << primitive_type(filter.value)
		}
		result.kinds << filter.operator
	}
	return result
}

fn invalid_scope_filter_error(filter QueryFilter) IError {
	if filter.operator in [.in, .not_in] {
		return error('orm.DataScope: dynamic filter `${filter.field}` with `${filter.operator}` requires a non-empty array value')
	}
	return error('orm.DataScope: dynamic filter `${filter.field}` with `${filter.operator}` requires a scalar value')
}

fn filter_value_matches_operator(filter QueryFilter) bool {
	array_len := primitive_array_len(filter.value)
	if filter.operator in [.in, .not_in] {
		return array_len > 0
	}
	return array_len < 0
}

fn apply_scope_insert_filters(scope DataScope, table Table, data QueryData, scope_skip_fields []string) !QueryData {
	if !scope.enabled || scope.filters.len == 0 {
		return data
	}
	if table_ignores_data_scope(table) {
		return data
	}
	mut result := clone_query_data(data)
	original_field_count := data.fields.len
	field_to_column := table_field_to_column_map(table)
	for filter in scope.filters {
		if filter.mode == .unset {
			return error('orm.DataScope: QueryFilter.mode must be explicitly set. .static is not yet implemented — use .dynamic. Got .unset for field `${filter.field}`')
		}
		if filter.mode != .dynamic {
			continue
		}
		if filter.field == '' {
			return error('orm.DataScope: dynamic filter field must not be empty')
		}
		if filter.field in scope_skip_fields {
			continue
		}
		if table.fields.len > 0 && filter.field !in table.fields {
			continue
		}
		if !filter_value_matches_operator(filter) {
			return invalid_scope_filter_error(filter)
		}
		if filter.operator == .is_null {
			continue
		}
		if filter.operator != .eq {
			return error('orm.DataScope: dynamic filter `${filter.field}` with `${filter.operator}` cannot be applied to INSERT')
		}
		mut column_name := filter.field
		if resolved := field_to_column[filter.field] {
			column_name = resolved
		}
		field_index := result.fields.index(column_name)
		if field_index >= 0 {
			if result.batch_rows > 0 {
				if field_index < original_field_count {
					// Original field — data is per-row with original_field_count stride
					for row in 0 .. result.batch_rows {
						data_index := row * original_field_count + field_index
						if data_index < result.data.len {
							result.data[data_index] = filter.value
						}
						if data_index < result.types.len {
							result.types[data_index] = primitive_type(filter.value)
						}
					}
				} else {
					// Scope field appended by a previous filter — single value at the end
					data_index := original_field_count * result.batch_rows +
						(field_index - original_field_count)
					if data_index < result.data.len {
						result.data[data_index] = filter.value
					}
					if data_index < result.types.len {
						result.types[data_index] = primitive_type(filter.value)
					}
				}
			} else {
				// Single row — stride is irrelevant; directly index by field position
				if field_index < result.data.len {
					result.data[field_index] = filter.value
				}
				if field_index < result.types.len {
					result.types[field_index] = primitive_type(filter.value)
				}
			}
			continue
		}
		result.fields << column_name.clone()
		result.data << filter.value
		result.types << primitive_type(filter.value)
	}
	if result.batch_rows > 0 {
		scope_field_count := result.fields.len - original_field_count
		if scope_field_count > 0 {
			mut new_data := []Primitive{cap: result.fields.len * result.batch_rows}
			scope_data_start := original_field_count * result.batch_rows
			for row in 0 .. result.batch_rows {
				for col in 0 .. original_field_count {
					new_data << result.data[row * original_field_count + col]
				}
				for s in 0 .. scope_field_count {
					new_data << result.data[scope_data_start + s]
				}
			}
			result.data = new_data
		}
	}
	return result
}

fn primitive_is_array(value Primitive) bool {
	return primitive_array_len(value) >= 0
}

fn primitive_array_len(value Primitive) int {
	return match value {
		[]Primitive, []bool, []f32, []f64, []i16, []i64, []i8, []int, []string, []time.Time, []u16,
		[]u32, []u64, []u8, []InfixType {
			value.len
		}
		else {
			-1
		}
	}
}

// DB implements orm.Connection ------------------------------------------------

// select fetches rows through the wrapped connection, with DataScope applied.
pub fn (mut db DB) select(config SelectConfig, data QueryData, where QueryData) ![][]Primitive {
	mut cfg := config
	if db.scope.enabled && db.scope.filters.len > 0 && !db.skip_all_scopes
		&& !table_ignores_data_scope(cfg.table) {
		where_scoped := apply_data_scope(db.scope, cfg.table, where, db.skip_fields,
			cfg.joins.len > 0)!
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
		data_scoped = apply_data_scope_insert(db.scope, table, data, db.skip_fields)!
	}
	return db.conn.insert(table, data_scoped)
}

// update updates rows through the wrapped connection, with DataScope applied.
pub fn (mut db DB) update(table Table, data QueryData, where QueryData) ! {
	mut where_scoped := where
	if db.scope.enabled && db.scope.filters.len > 0 && !db.skip_all_scopes
		&& !table_ignores_data_scope(table) {
		where_scoped = apply_data_scope(db.scope, table, where, db.skip_fields, false)!
	}
	return db.conn.update(table, data, where_scoped)
}

// delete deletes rows through the wrapped connection, with DataScope applied.
pub fn (mut db DB) delete(table Table, where QueryData) ! {
	mut where_scoped := where
	if db.scope.enabled && db.scope.filters.len > 0 && !db.skip_all_scopes
		&& !table_ignores_data_scope(table) {
		where_scoped = apply_data_scope(db.scope, table, where, db.skip_fields, false)!
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

// execute runs a raw SQL query on the wrapped connection and returns the result rows.
pub fn (mut db DB) execute(query string) ![]Row {
	return db.conn.execute(query)
}

// DB implements orm.TransactionalConnection (decorator) -----------------------

// unwrap_to_tx extracts a TransactionalConnection from a Connection interface.
fn unwrap_to_tx(mut conn Connection) TransactionalConnection {
	return conn as TransactionalConnection
}

// orm_begin begins a transaction on the underlying connection.
// Returns an error if the underlying connection does not support transactions.
pub fn (mut db DB) orm_begin() ! {
	if db.conn is TransactionalConnection {
		mut tc := unwrap_to_tx(mut db.conn)
		tc.orm_begin()!
	} else {
		return error('orm.DB: underlying connection does not support transactions')
	}
}

// orm_commit commits the current transaction on the underlying connection.
// Returns an error if the underlying connection does not support transactions.
pub fn (mut db DB) orm_commit() ! {
	if db.conn is TransactionalConnection {
		mut tc := unwrap_to_tx(mut db.conn)
		tc.orm_commit()!
	} else {
		return error('orm.DB: underlying connection does not support transactions')
	}
}

// orm_rollback rolls back the current transaction on the underlying connection.
// Returns an error if the underlying connection does not support transactions.
pub fn (mut db DB) orm_rollback() ! {
	if db.conn is TransactionalConnection {
		mut tc := unwrap_to_tx(mut db.conn)
		tc.orm_rollback()!
	} else {
		return error('orm.DB: underlying connection does not support transactions')
	}
}

// orm_savepoint creates a savepoint with the given name on the underlying connection.
// Returns an error if the underlying connection does not support transactions.
pub fn (mut db DB) orm_savepoint(name string) ! {
	if db.conn is TransactionalConnection {
		mut tc := unwrap_to_tx(mut db.conn)
		tc.orm_savepoint(name)!
	} else {
		return error('orm.DB: underlying connection does not support transactions')
	}
}

// orm_rollback_to rolls back to the named savepoint on the underlying connection.
// Returns an error if the underlying connection does not support transactions.
pub fn (mut db DB) orm_rollback_to(name string) ! {
	if db.conn is TransactionalConnection {
		mut tc := unwrap_to_tx(mut db.conn)
		tc.orm_rollback_to(name)!
	} else {
		return error('orm.DB: underlying connection does not support transactions')
	}
}

// orm_release_savepoint releases the named savepoint on the underlying connection.
// Returns an error if the underlying connection does not support transactions.
pub fn (mut db DB) orm_release_savepoint(name string) ! {
	if db.conn is TransactionalConnection {
		mut tc := unwrap_to_tx(mut db.conn)
		tc.orm_release_savepoint(name)!
	} else {
		return error('orm.DB: underlying connection does not support transactions')
	}
}
