module migrations

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
pub struct Column {
pub:
	name           string
	kind           ColumnType
	limit          int
	precision      int
	scale          int
	nullable       bool = true
	default_sql    string
	primary_key    bool
	auto_increment bool
	unique         bool
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
	validate_identifier(table.name, 'table')!
	mut definitions := []string{}
	mut column_names := map[string]bool{}
	mut has_primary_key := false
	if table.id {
		validate_identifier(table.id_name, 'primary key')!
		definitions << auto_primary_key_sql(ctx.dialect, table.id_name)
		column_names[table.id_name] = true
		has_primary_key = true
	}
	for column in table.columns {
		if column.name in column_names {
			return error('table `${table.name}` has duplicate column `${column.name}`')
		}
		if has_primary_key && column.primary_key {
			return error('table `${table.name}` cannot have more than one primary key column')
		}
		definitions << column_sql(ctx.dialect, column)!
		column_names[column.name] = true
		has_primary_key = has_primary_key || column.primary_key
	}
	for key in table.foreign_keys {
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
	validate_identifier(name, 'table')!
	ctx.execute('DROP TABLE ${quote_identifier(ctx.dialect, name)};')!
}

// rename_table renames a table.
pub fn (mut ctx Context) rename_table(from string, to string) ! {
	validate_identifier(from, 'table')!
	validate_identifier(to, 'table')!
	ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, from)} RENAME TO ${quote_identifier(ctx.dialect,
		to)};')!
}

// add_column adds a column to an existing table.
pub fn (mut ctx Context) add_column(table string, column Column) ! {
	validate_identifier(table, 'table')!
	definition := column_sql(ctx.dialect, column)!
	ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, table)} ADD COLUMN ${definition};')!
}

// remove_column removes a column from an existing table.
pub fn (mut ctx Context) remove_column(table string, column string) ! {
	validate_identifier(table, 'table')!
	validate_identifier(column, 'column')!
	ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, table)} DROP COLUMN ${quote_identifier(ctx.dialect,
		column)};')!
}

// rename_column renames a column.
pub fn (mut ctx Context) rename_column(table string, from string, to string) ! {
	validate_identifier(table, 'table')!
	validate_identifier(from, 'column')!
	validate_identifier(to, 'column')!
	ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, table)} RENAME COLUMN ${quote_identifier(ctx.dialect,
		from)} TO ${quote_identifier(ctx.dialect, to)};')!
}

// change_column changes a column type and constraints. SQLite requires a table
// rebuild for this operation, so the helper returns an explicit error there.
pub fn (mut ctx Context) change_column(table string, column Column) ! {
	validate_identifier(table, 'table')!
	definition := column_sql(ctx.dialect, column)!
	match ctx.dialect {
		.sqlite {
			return error('change_column is not directly supported by SQLite; create a replacement table in the migration')
		}
		.pg {
			ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, table)} ALTER COLUMN ${quote_identifier(ctx.dialect,
				column.name)} TYPE ${column_type_sql(ctx.dialect, column)!};')!
		}
		.mysql {
			ctx.execute('ALTER TABLE ${quote_identifier(ctx.dialect, table)} MODIFY COLUMN ${definition};')!
		}
	}
}

// add_index adds an index. When name is empty, a deterministic Rails-style
// `index_<table>_on_<columns>` name is used.
pub fn (mut ctx Context) add_index(index Index) ! {
	name := index_name(index)!
	columns := quoted_columns(ctx.dialect, index.columns)!
	unique := if index.unique { 'UNIQUE ' } else { '' }
	ctx.execute('CREATE ${unique}INDEX ${quote_identifier(ctx.dialect, name)} ON ${quote_identifier(ctx.dialect,
		index.table)} (${columns});')!
}

// remove_index removes an index by name.
pub fn (mut ctx Context) remove_index(table string, name string) ! {
	validate_identifier(table, 'table')!
	validate_identifier(name, 'index')!
	if ctx.dialect == .mysql {
		ctx.execute('DROP INDEX ${quote_identifier(ctx.dialect, name)} ON ${quote_identifier(ctx.dialect,
			table)};')!
	} else {
		ctx.execute('DROP INDEX ${quote_identifier(ctx.dialect, name)};')!
	}
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
	validate_identifier(table, 'table')!
	validate_identifier(name, 'foreign key')!
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
	validate_identifier(column.name, 'column')!
	if column.limit < 0 {
		return error('column `${column.name}` limit must not be negative')
	}
	if column.auto_increment && column.kind !in [.integer, .bigint] {
		return error('auto-increment column `${column.name}` must be integer or bigint')
	}
	if dialect == .sqlite && column.auto_increment && !column.primary_key {
		return error('SQLite auto-increment column `${column.name}` must be a primary key')
	}
	mut sql_type := column_type_sql(dialect, column)!
	if column.auto_increment {
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
	if column.primary_key {
		parts << 'PRIMARY KEY'
	}
	if !column.nullable && !column.primary_key {
		parts << 'NOT NULL'
	}
	if column.unique {
		parts << 'UNIQUE'
	}
	if column.default_sql != '' {
		parts << 'DEFAULT ${column.default_sql}'
	}
	if dialect == .sqlite && column.auto_increment && column.primary_key {
		parts << 'AUTOINCREMENT'
	}
	return parts.join(' ')
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

fn index_name(index Index) !string {
	validate_identifier(index.table, 'table')!
	if index.columns.len == 0 {
		return error('index on `${index.table}` must include at least one column')
	}
	for column in index.columns {
		validate_identifier(column, 'column')!
	}
	name := if index.name != '' {
		index.name
	} else {
		'index_${index.table.replace('.', '_')}_on_${index.columns.join('_and_')}'
	}
	validate_identifier(name, 'index')!
	return name
}

fn quoted_columns(dialect Dialect, columns []string) !string {
	if columns.len == 0 {
		return error('at least one column is required')
	}
	mut quoted := []string{cap: columns.len}
	for column in columns {
		validate_identifier(column, 'column')!
		quoted << quote_identifier(dialect, column)
	}
	return quoted.join(', ')
}

fn foreign_key_name(key ForeignKey) !string {
	name := if key.name != '' {
		key.name
	} else {
		'fk_${key.from_table.replace('.', '_')}_${key.column}'
	}
	validate_identifier(name, 'foreign key')!
	return name
}

fn foreign_key_constraint_sql(dialect Dialect, key ForeignKey) !string {
	name := foreign_key_name(key)!
	validate_foreign_key(key)!
	mut query := 'CONSTRAINT ${quote_identifier(dialect, name)} FOREIGN KEY (${quote_identifier(dialect,
		key.column)}) REFERENCES ${quote_identifier(dialect, key.to_table)} (${quote_identifier(dialect,
		key.primary_key)})'
	if key.on_delete != '' {
		query += ' ON DELETE ${foreign_key_action(key.on_delete)!}'
	}
	if key.on_update != '' {
		query += ' ON UPDATE ${foreign_key_action(key.on_update)!}'
	}
	return query
}

fn validate_foreign_key(key ForeignKey) ! {
	validate_identifier(key.from_table, 'table')!
	validate_identifier(key.column, 'column')!
	validate_identifier(key.to_table, 'table')!
	validate_identifier(key.primary_key, 'column')!
}

fn foreign_key_action(action string) !string {
	normalized := action.trim_space().to_upper().replace('_', ' ')
	if normalized !in ['CASCADE', 'RESTRICT', 'NO ACTION', 'SET NULL', 'SET DEFAULT'] {
		return error('unsupported foreign-key action `${action}`')
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

fn quote_identifier(dialect Dialect, value string) string {
	quote := if dialect == .mysql { '`' } else { '"' }
	return value.split('.').map('${quote}${it}${quote}').join('.')
}

fn escape_literal(value string) string {
	return value.replace("'", "''")
}
