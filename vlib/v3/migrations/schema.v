module migrations

import hash.fnv1a
import orm

// Dialect selects the SQL emitted by schema helpers.
pub enum Dialect {
	sqlite
	pg
	mysql
}

// ColumnType is a portable subset of common SQL column types.
pub enum ColumnType {
	boolean
	integer
	bigint
	real
	double_precision
	text
	varchar
	blob
	date
	datetime
	timestamp
	json
	jsonb
	uuid
	decimal
}

// Column describes a column used by create_table, add_column, or change_column.
// default_sql is inserted verbatim and should contain a trusted SQL expression.
// Constraint fields are optional so change_column can distinguish omitted
// options from explicit false or empty values.
pub struct Column {
pub:
	name           string
	kind           ColumnType
	limit          int
	precision      int
	scale          int
	nullable       ?bool
	default_sql    ?string
	primary_key    ?bool
	auto_increment ?bool
	unique         ?bool
}

// Table describes a table for Context.create_table. An auto-incrementing
// bigint `id` primary key is added unless id is false.
pub struct Table {
pub:
	name          string
	columns       []Column
	foreign_keys  []ForeignKey
	id            bool   = true
	id_name       string = 'id'
	if_not_exists bool
}

// Index describes an index for Context.add_index.
pub struct Index {
pub:
	table   string
	columns []string
	name    string
	unique  bool
}

// ForeignKey describes a foreign-key constraint.
pub struct ForeignKey {
pub:
	from_table  string
	column      string
	to_table    string
	primary_key string = 'id'
	name        string
	on_delete   string
	on_update   string
}

// Context is passed to migration callbacks. It implements orm.Connection, so
// normal V3 `sql ctx { ... }` ORM statements can be used alongside schema helpers.
pub struct Context {
mut:
	conn orm.Connection
pub:
	dialect Dialect
}

fn new_context(conn orm.Connection, dialect Dialect) Context {
	return Context{
		conn:    conn
		dialect: dialect
	}
}

// execute runs trusted raw SQL through the migration connection.
pub fn (mut ctx Context) execute(query string) ![]orm.Row {
	return ctx.conn.execute(query)
}

// create_table creates a table from a Rails-style table definition.
pub fn (mut ctx Context) create_table(table Table) ! {
	validate_identifier_for_dialect(ctx.dialect, table.name, 'table')!
	mut definitions := []string{}
	mut column_names := map[string]bool{}
	mut has_primary_key := false
	mut auto_increment_columns := 0
	if table.id {
		validate_unqualified_identifier_for_dialect(ctx.dialect, table.id_name, 'primary key')!
		definitions << auto_primary_key_sql(ctx.dialect, table.id_name)
		column_names[table.id_name] = true
		has_primary_key = true
		if ctx.dialect == .mysql {
			auto_increment_columns = 1
		}
	}
	for column in table.columns {
		if column.name in column_names {
			return error('table `${table.name}` has duplicate column `${column.name}`')
		}
		is_primary_key := column_primary_key(column)
		if has_primary_key && is_primary_key {
			return error('table `${table.name}` cannot have more than one primary key column')
		}
		definition := column_sql(ctx.dialect, column)!
		if ctx.dialect == .mysql && column_auto_increment(column) {
			auto_increment_columns++
			if auto_increment_columns > 1 {
				return error('MySQL table `${table.name}` cannot have more than one auto-increment column')
			}
		}
		definitions << definition
		column_names[column.name] = true
		has_primary_key = has_primary_key || is_primary_key
	}
	for key in table.foreign_keys {
		validate_foreign_key(ctx.dialect, key)!
		if key.from_table != table.name {
			return error('foreign key `${key.name}` must use from_table `${table.name}`')
		}
		if key.column !in column_names {
			return error('foreign key column `${key.column}` does not exist in table `${table.name}`')
		}
		definitions << foreign_key_constraint_sql(ctx.dialect, key)!
	}
	if definitions.len == 0 {
		return error('table `${table.name}` must have at least one column')
	}
	guard := if table.if_not_exists { ' IF NOT EXISTS' } else { '' }
	name := quote_identifier(ctx.dialect, table.name)
	ctx.execute('CREATE TABLE${guard} ${name} (${definitions.join(', ')});')!
}

// drop_table drops a table by name.
pub fn (mut ctx Context) drop_table(name string) ! {
	validate_identifier_for_dialect(ctx.dialect, name, 'table')!
	ctx.execute('DROP TABLE ${quote_identifier(ctx.dialect, name)};')!
}

// rename_table renames a table. PostgreSQL and SQLite targets must be unqualified.
pub fn (mut ctx Context) rename_table(from string, to string) ! {
	validate_identifier_for_dialect(ctx.dialect, from, 'table')!
	validate_identifier_for_dialect(ctx.dialect, to, 'table')!
	if ctx.dialect in [.sqlite, .pg] && to.contains('.') {
		return error('rename_table target `${to}` must be unqualified for PostgreSQL and SQLite')
	}
	ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, from)} RENAME TO ${quote_identifier(ctx.dialect,
		to)};')!
}

// add_column adds a column to an existing table.
pub fn (mut ctx Context) add_column(table string, column Column) ! {
	validate_identifier_for_dialect(ctx.dialect, table, 'table')!
	definition := column_sql(ctx.dialect, column)!
	if ctx.dialect == .sqlite {
		validate_sqlite_add_column(column)!
	}
	ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, table)} ADD COLUMN ${definition};')!
}

// remove_column removes a column from an existing table.
pub fn (mut ctx Context) remove_column(table string, column string) ! {
	validate_identifier_for_dialect(ctx.dialect, table, 'table')!
	validate_unqualified_identifier_for_dialect(ctx.dialect, column, 'column')!
	ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, table)} DROP COLUMN ${quote_identifier(ctx.dialect,
		column)};')!
}

// rename_column renames a column.
pub fn (mut ctx Context) rename_column(table string, from string, to string) ! {
	validate_identifier_for_dialect(ctx.dialect, table, 'table')!
	validate_unqualified_identifier_for_dialect(ctx.dialect, from, 'column')!
	validate_unqualified_identifier_for_dialect(ctx.dialect, to, 'column')!
	ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, table)} RENAME COLUMN ${quote_identifier(ctx.dialect,
		from)} TO ${quote_identifier(ctx.dialect, to)};')!
}

// change_column changes a column definition. SQLite requires a table rebuild.
// PostgreSQL supports type-related fields only and rejects constraint changes
// before executing SQL; use execute for explicit PostgreSQL constraint DDL.
// MySQL requires nullable, default_sql, and auto_increment to be supplied because
// MODIFY COLUMN replaces those attributes. Omitted key options are preserved.
pub fn (mut ctx Context) change_column(table string, column Column) ! {
	validate_identifier_for_dialect(ctx.dialect, table, 'table')!
	match ctx.dialect {
		.sqlite {
			column_sql(ctx.dialect, column)!
			return error('change_column is not directly supported by SQLite; create a replacement table in the migration')
		}
		.pg {
			unsupported := pg_change_column_unsupported_options(column)
			if unsupported.len > 0 {
				return error('PostgreSQL change_column only supports type, limit, precision, and scale; unsupported options: ${unsupported.join(', ')}; use ctx.execute() for constraint changes')
			}
			column_sql(ctx.dialect, column)!
			ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, table)} ALTER COLUMN ${quote_identifier(ctx.dialect,
				column.name)} TYPE ${column_type_sql(ctx.dialect, column)!};')!
		}
		.mysql {
			removals := mysql_change_column_unsupported_key_removals(column)
			if removals.len > 0 {
				return error('MySQL change_column cannot remove key constraints; unsupported false options: ${removals.join(', ')}; use remove_index() or ctx.execute()')
			}
			missing := mysql_change_column_missing_options(column)
			if missing.len > 0 {
				return error('MySQL change_column requires a complete column definition; missing options: ${missing.join(', ')}')
			}
			definition := mysql_change_column_sql(column)!
			ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, table)} MODIFY COLUMN ${definition};')!
		}
	}
}

fn pg_change_column_unsupported_options(column Column) []string {
	mut options := []string{}
	if _ := column.nullable {
		options << 'nullable'
	}
	if _ := column.default_sql {
		options << 'default_sql'
	}
	if _ := column.unique {
		options << 'unique'
	}
	if _ := column.primary_key {
		options << 'primary_key'
	}
	if _ := column.auto_increment {
		options << 'auto_increment'
	}
	return options
}

fn mysql_change_column_missing_options(column Column) []string {
	mut options := []string{}
	if column.nullable == none {
		options << 'nullable'
	}
	if column.default_sql == none {
		options << 'default_sql'
	}
	if column.auto_increment == none {
		options << 'auto_increment'
	}
	return options
}

fn mysql_change_column_unsupported_key_removals(column Column) []string {
	mut options := []string{}
	if unique := column.unique {
		if !unique {
			options << 'unique'
		}
	}
	if primary_key := column.primary_key {
		if !primary_key {
			options << 'primary_key'
		}
	}
	return options
}

// add_index adds an index. When name is empty, a deterministic Rails-style
// `index_<table>_on_<columns>` name is used. SQLite tables and PostgreSQL/MySQL
// index names must be unqualified.
pub fn (mut ctx Context) add_index(index Index) ! {
	name := index_name(ctx.dialect, index)!
	if ctx.dialect == .sqlite && index.table.contains('.') {
		return error('SQLite add_index table `${index.table}` must be unqualified')
	}
	if ctx.dialect == .pg && name.contains('.') {
		return error('PostgreSQL add_index name `${name}` must be unqualified')
	}
	if ctx.dialect == .mysql && name.contains('.') {
		return error('MySQL add_index name `${name}` must be unqualified')
	}
	columns := quoted_columns(ctx.dialect, index.columns)!
	unique := if index.unique { 'UNIQUE ' } else { '' }
	ctx.execute('CREATE ${unique}INDEX ${quote_identifier(ctx.dialect, name)} ON ${quote_identifier(ctx.dialect,
		index.table)} (${columns});')!
}

// remove_index removes an index by name. PostgreSQL derives the index schema
// from a qualified table unless name is already qualified. MySQL index names
// must be unqualified.
pub fn (mut ctx Context) remove_index(table string, name string) ! {
	validate_identifier_for_dialect(ctx.dialect, table, 'table')!
	validate_identifier_for_dialect(ctx.dialect, name, 'index')!
	if ctx.dialect == .mysql && name.contains('.') {
		return error('MySQL remove_index name `${name}` must be unqualified')
	}
	match ctx.dialect {
		.mysql {
			ctx.execute('DROP INDEX ${quote_identifier(ctx.dialect, name)} ON ${quote_identifier(ctx.dialect,
				table)};')!
		}
		.pg {
			drop_name := postgres_index_drop_name(table, name)
			ctx.execute('DROP INDEX ${quote_identifier(ctx.dialect, drop_name)};')!
		}
		.sqlite {
			ctx.execute('DROP INDEX ${quote_identifier(ctx.dialect, name)};')!
		}
	}
}

fn postgres_index_drop_name(table string, name string) string {
	if name.contains('.') || !table.contains('.') {
		return name
	}
	mut parts := table.split('.')
	parts[parts.len - 1] = name
	return parts.join('.')
}

// add_foreign_key adds a named foreign-key constraint. SQLite cannot add one
// to an existing table without rebuilding the table.
pub fn (mut ctx Context) add_foreign_key(key ForeignKey) ! {
	if ctx.dialect == .sqlite {
		return error('add_foreign_key is not directly supported by SQLite; define it while creating a replacement table')
	}
	constraint := foreign_key_constraint_sql(ctx.dialect, key)!
	ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, key.from_table)} ADD ${constraint};')!
}

// remove_foreign_key removes a named foreign-key constraint.
pub fn (mut ctx Context) remove_foreign_key(table string, name string) ! {
	validate_identifier_for_dialect(ctx.dialect, table, 'table')!
	validate_unqualified_identifier_for_dialect(ctx.dialect, name, 'foreign key')!
	match ctx.dialect {
		.sqlite {
			return error('remove_foreign_key is not directly supported by SQLite; create a replacement table in the migration')
		}
		.pg {
			ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, table)} DROP CONSTRAINT ${quote_identifier(ctx.dialect,
				name)};')!
		}
		.mysql {
			ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, table)} DROP FOREIGN KEY ${quote_identifier(ctx.dialect,
				name)};')!
		}
	}
}

// create_orm_table creates the table represented by T through V's ORM metadata.
pub fn create_orm_table[T](mut ctx Context) ! {
	mut query := orm.new_query[T](ctx)
	query.create()!
}

// drop_orm_table drops the table represented by T through V's ORM metadata.
pub fn drop_orm_table[T](mut ctx Context) ! {
	mut query := orm.new_query[T](ctx)
	query.drop()!
}

// select forwards ORM queries through the migration connection.
pub fn (mut ctx Context) select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	return ctx.conn.select(config, data, where)
}

// insert forwards ORM inserts through the migration connection.
pub fn (mut ctx Context) insert(table orm.Table, data orm.QueryData) ! {
	ctx.conn.insert(table, data)!
}

// update forwards ORM updates through the migration connection.
pub fn (mut ctx Context) update(table orm.Table, data orm.QueryData, where orm.QueryData) ! {
	ctx.conn.update(table, data, where)!
}

// delete forwards ORM deletes through the migration connection.
pub fn (mut ctx Context) delete(table orm.Table, where orm.QueryData) ! {
	ctx.conn.delete(table, where)!
}

// create forwards ORM table creation through the migration connection.
pub fn (mut ctx Context) create(table orm.Table, fields []orm.TableField) ! {
	ctx.conn.create(table, fields)!
}

// drop forwards ORM table removal through the migration connection.
pub fn (mut ctx Context) drop(table orm.Table) ! {
	ctx.conn.drop(table)!
}

// last_id forwards the last inserted id from the migration connection.
pub fn (mut ctx Context) last_id() int {
	return ctx.conn.last_id()
}

fn auto_primary_key_sql(dialect Dialect, name string) string {
	quoted := quote_identifier(dialect, name)
	return match dialect {
		.sqlite { '${quoted} INTEGER PRIMARY KEY AUTOINCREMENT' }
		.pg { '${quoted} BIGSERIAL PRIMARY KEY' }
		.mysql { '${quoted} BIGINT AUTO_INCREMENT PRIMARY KEY' }
	}
}

fn column_sql(dialect Dialect, column Column) !string {
	return column_sql_internal(dialect, column, false)
}

fn mysql_change_column_sql(column Column) !string {
	return column_sql_internal(.mysql, column, true)
}

fn column_sql_internal(dialect Dialect, column Column, preserve_omitted_mysql_key bool) !string {
	validate_unqualified_identifier_for_dialect(dialect, column.name, 'column')!
	if column.limit < 0 {
		return error('column `${column.name}` limit must not be negative')
	}
	auto_increment := column_auto_increment(column)
	primary_key := column_primary_key(column)
	unique := column_unique(column)
	default_sql := column_default_sql(column)
	if auto_increment && column.kind !in [.integer, .bigint] {
		return error('auto-increment column `${column.name}` must be integer or bigint')
	}
	if dialect == .pg && auto_increment && default_sql != '' {
		return error('PostgreSQL auto-increment column `${column.name}` cannot specify default_sql')
	}
	if dialect == .sqlite && auto_increment && !primary_key {
		return error('SQLite auto-increment column `${column.name}` must be a primary key')
	}
	preserves_mysql_key := preserve_omitted_mysql_key && column.primary_key == none
		&& column.unique == none
	if dialect == .mysql && auto_increment && !primary_key && !unique && !preserves_mysql_key {
		return error('MySQL auto-increment column `${column.name}` must be a primary key or unique')
	}
	mut sql_type := column_type_sql(dialect, column)!
	if auto_increment {
		sql_type = match dialect {
			.sqlite {
				'INTEGER'
			}
			.pg {
				if column.kind == .bigint { 'BIGSERIAL' } else { 'SERIAL' }
			}
			.mysql {
				'${sql_type} AUTO_INCREMENT'
			}
		}
	}
	mut parts := [quote_identifier(dialect, column.name), sql_type]
	if primary_key {
		if dialect == .sqlite && auto_increment {
			parts << 'PRIMARY KEY AUTOINCREMENT'
		} else {
			parts << 'PRIMARY KEY'
		}
	}
	if (!column_nullable(column) && !primary_key)
		|| (dialect == .sqlite && primary_key && sql_type != 'INTEGER') {
		parts << 'NOT NULL'
	}
	if unique {
		parts << 'UNIQUE'
	}
	if default_sql != '' {
		parts << 'DEFAULT ${default_sql}'
	}
	return parts.join(' ')
}

fn validate_sqlite_add_column(column Column) ! {
	if column_primary_key(column) || column_unique(column) || column_auto_increment(column) {
		return error('SQLite add_column does not support primary-key, unique, or auto-increment columns; rebuild the table in the migration')
	}
	if default_sql := column.default_sql {
		if sqlite_add_column_default_is_nonconstant(default_sql) {
			return error('SQLite add_column does not support nonconstant default `${default_sql}`; rebuild the table in the migration')
		}
	}
	if !column_nullable(column) && !sqlite_has_non_null_default(column) {
		return error('SQLite add_column requires a non-NULL default for a NOT NULL column; rebuild the table in the migration')
	}
}

fn sqlite_add_column_default_is_nonconstant(default_sql string) bool {
	normalized := default_sql.trim_space().to_upper()
	return normalized in ['CURRENT_TIME', 'CURRENT_DATE', 'CURRENT_TIMESTAMP']
		|| (normalized.starts_with('(') && normalized.ends_with(')'))
}

fn sqlite_has_non_null_default(column Column) bool {
	if default_sql := column.default_sql {
		normalized := default_sql.trim_space().to_upper()
		return normalized != '' && normalized != 'NULL'
	}
	return false
}

fn column_nullable(column Column) bool {
	return column.nullable or { true }
}

fn column_default_sql(column Column) string {
	return column.default_sql or { '' }
}

fn column_primary_key(column Column) bool {
	return column.primary_key or { false }
}

fn column_auto_increment(column Column) bool {
	return column.auto_increment or { false }
}

fn column_unique(column Column) bool {
	return column.unique or { false }
}

fn column_type_sql(dialect Dialect, column Column) !string {
	return match column.kind {
		.boolean {
			'BOOLEAN'
		}
		.integer {
			'INTEGER'
		}
		.bigint {
			'BIGINT'
		}
		.real {
			'REAL'
		}
		.double_precision {
			if dialect == .mysql {
				'DOUBLE'
			} else {
				'DOUBLE PRECISION'
			}
		}
		.text {
			'TEXT'
		}
		.varchar {
			limit := if column.limit > 0 { column.limit } else { 255 }
			'VARCHAR(${limit})'
		}
		.blob {
			if dialect == .pg {
				'BYTEA'
			} else {
				'BLOB'
			}
		}
		.date {
			'DATE'
		}
		.datetime {
			if dialect == .pg {
				'TIMESTAMP'
			} else {
				'DATETIME'
			}
		}
		.timestamp {
			'TIMESTAMP'
		}
		.json {
			'JSON'
		}
		.jsonb {
			if dialect == .pg {
				'JSONB'
			} else {
				'JSON'
			}
		}
		.uuid {
			if dialect == .pg {
				'UUID'
			} else {
				'CHAR(36)'
			}
		}
		.decimal {
			if column.precision < 0 || column.scale < 0 {
				return error('decimal precision and scale must not be negative')
			}
			if column.precision == 0 && column.scale > 0 {
				return error('decimal scale requires a positive precision')
			}
			if column.precision == 0 {
				'DECIMAL'
			} else if column.scale == 0 {
				'DECIMAL(${column.precision})'
			} else {
				if column.scale > column.precision {
					return error('decimal scale must not exceed precision')
				}
				'DECIMAL(${column.precision}, ${column.scale})'
			}
		}
	}
}

fn index_name(dialect Dialect, index Index) !string {
	validate_identifier_for_dialect(dialect, index.table, 'table')!
	if index.columns.len == 0 {
		return error('index on `${index.table}` must include at least one column')
	}
	for column in index.columns {
		validate_unqualified_identifier_for_dialect(dialect, column, 'column')!
	}
	name := if index.name != '' {
		index.name
	} else {
		'index_${index.table.replace('.', '_')}_on_${index.columns.join('_and_')}'
	}
	validate_identifier(name, 'index')!
	return bounded_identifier_name(dialect, name, index.name == '', 'index')
}

fn identifier_name_limit(dialect Dialect) int {
	return match dialect {
		.sqlite { 0 }
		.pg { 63 }
		.mysql { 64 }
	}
}

fn bounded_identifier_name(dialect Dialect, name string, generated bool, kind string) !string {
	limit := identifier_name_limit(dialect)
	if limit == 0 || name.len <= limit {
		return name
	}
	if !generated {
		return error('${dialect_name(dialect)} ${kind} name `${name}` must not exceed ${limit} bytes')
	}
	hash := fnv1a.sum64_string(name).hex()
	return '${name[..limit - hash.len - 1]}_${hash}'
}

fn dialect_name(dialect Dialect) string {
	return match dialect {
		.sqlite { 'SQLite' }
		.pg { 'PostgreSQL' }
		.mysql { 'MySQL' }
	}
}

fn quoted_columns(dialect Dialect, columns []string) !string {
	if columns.len == 0 {
		return error('at least one column is required')
	}
	mut quoted := []string{cap: columns.len}
	for column in columns {
		validate_unqualified_identifier_for_dialect(dialect, column, 'column')!
		quoted << quote_identifier(dialect, column)
	}
	return quoted.join(', ')
}

fn foreign_key_name(dialect Dialect, key ForeignKey) !string {
	name := if key.name != '' {
		key.name
	} else {
		'fk_${key.from_table.replace('.', '_')}_${key.column}'
	}
	validate_unqualified_identifier(name, 'foreign key')!
	return bounded_identifier_name(dialect, name, key.name == '', 'foreign key')
}

fn foreign_key_constraint_sql(dialect Dialect, key ForeignKey) !string {
	validate_foreign_key(dialect, key)!
	if dialect == .sqlite && key.to_table.contains('.') {
		return error('SQLite foreign-key target table `${key.to_table}` must be unqualified')
	}
	name := foreign_key_name(dialect, key)!
	mut query := 'CONSTRAINT ${quote_identifier(dialect, name)} FOREIGN KEY (${quote_identifier(dialect,
		key.column)}) REFERENCES ${quote_identifier(dialect, key.to_table)} (${quote_identifier(dialect,
		key.primary_key)})'
	if key.on_delete != '' {
		query += ' ON DELETE ${foreign_key_action(dialect, key.on_delete)!}'
	}
	if key.on_update != '' {
		query += ' ON UPDATE ${foreign_key_action(dialect, key.on_update)!}'
	}
	return query
}

fn validate_foreign_key(dialect Dialect, key ForeignKey) ! {
	validate_identifier_for_dialect(dialect, key.from_table, 'table')!
	validate_unqualified_identifier_for_dialect(dialect, key.column, 'column')!
	validate_identifier_for_dialect(dialect, key.to_table, 'table')!
	validate_unqualified_identifier_for_dialect(dialect, key.primary_key, 'column')!
}

fn foreign_key_action(dialect Dialect, action string) !string {
	normalized := action.trim_space().to_upper().replace('_', ' ')
	if normalized !in ['CASCADE', 'RESTRICT', 'NO ACTION', 'SET NULL', 'SET DEFAULT'] {
		return error('unsupported foreign-key action `${action}`')
	}
	if dialect == .mysql && normalized == 'SET DEFAULT' {
		return error('MySQL does not support SET DEFAULT for foreign-key actions')
	}
	return normalized
}

fn validate_identifier(value string, kind string) ! {
	if value == '' {
		return error('${kind} name must not be empty')
	}
	for part in value.split('.') {
		if part == '' || !(part[0].is_letter() || part[0] == `_`) {
			return error('invalid ${kind} name `${value}`')
		}
		for ch in part[1..] {
			if !(ch.is_alnum() || ch == `_`) {
				return error('invalid ${kind} name `${value}`')
			}
		}
	}
}

fn validate_identifier_for_dialect(dialect Dialect, value string, kind string) ! {
	validate_identifier(value, kind)!
	parts := value.split('.')
	max_components := identifier_max_components(dialect)
	if parts.len > max_components {
		return error('${dialect_name(dialect)} ${kind} name `${value}` must not exceed ${max_components} components')
	}
	limit := identifier_name_limit(dialect)
	if limit == 0 {
		return
	}
	for part in parts {
		if part.len > limit {
			return error('${dialect_name(dialect)} ${kind} name component `${part}` must not exceed ${limit} bytes')
		}
	}
}

fn identifier_max_components(dialect Dialect) int {
	return match dialect {
		.sqlite, .pg, .mysql { 2 }
	}
}

fn validate_unqualified_identifier(value string, kind string) ! {
	validate_identifier(value, kind)!
	if value.contains('.') {
		return error('${kind} name `${value}` must be unqualified')
	}
}

fn validate_unqualified_identifier_for_dialect(dialect Dialect, value string, kind string) ! {
	validate_identifier_for_dialect(dialect, value, kind)!
	if value.contains('.') {
		return error('${kind} name `${value}` must be unqualified')
	}
}

fn quote_identifier(dialect Dialect, value string) string {
	quote := if dialect == .mysql { '`' } else { '"' }
	return value.split('.').map('${quote}${it}${quote}').join('.')
}

fn escape_literal(value string) string {
	return value.replace("'", "''")
}

fn string_literal_sql(dialect Dialect, value string) string {
	if dialect == .mysql {
		return "X'${value.hex()}'"
	}
	return "'${escape_literal(value)}'"
}
