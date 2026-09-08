module migrations

import hash.fnv1a
import orm
import strconv

const sqlite_digit_separator_min_version = 3_046_000

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
	kind           ColumnType @[required]
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
	conn                   orm.Connection
	sqlite_runtime_version ?int
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
		column_names[column_name_key(ctx.dialect, table.id_name)] = true
		has_primary_key = true
		if ctx.dialect == .mysql {
			auto_increment_columns = 1
		}
	}
	for column in table.columns {
		column_key := column_name_key(ctx.dialect, column.name)
		if column_key in column_names {
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
		column_names[column_key] = true
		has_primary_key = has_primary_key || is_primary_key
	}
	for key in table.foreign_keys {
		validate_foreign_key(ctx.dialect, key)!
		if key.from_table != table.name {
			return error('foreign key `${key.name}` must use from_table `${table.name}`')
		}
		if column_name_key(ctx.dialect, key.column) !in column_names {
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
		validate_sqlite_add_column(mut ctx, column)!
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
// MySQL is rejected because MODIFY COLUMN replaces server-side attributes that
// the portable Column type cannot represent safely.
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
			validate_unqualified_identifier_for_dialect(.mysql, column.name, 'column')!
			column_type_sql(.mysql, column)!
			return error('MySQL change_column cannot safely preserve attributes outside Column; use ctx.execute() with a complete MODIFY COLUMN definition')
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
	mut create_name_sql := quote_identifier(ctx.dialect, name)
	if ctx.dialect == .sqlite && !name.contains('.') {
		schema := ctx.sqlite_table_schema(index.table)!
		create_name_sql = '${quote_identifier_component(.sqlite, schema)}.${quote_identifier_component(.sqlite, name)}'
	}
	unique := if index.unique { 'UNIQUE ' } else { '' }
	ctx.execute('CREATE ${unique}INDEX ${create_name_sql} ON ${quote_identifier(ctx.dialect,
		index.table)} (${columns});')!
}

// remove_index removes an index by name. PostgreSQL and SQLite derive the index
// schema from a qualified table unless name is already qualified. MySQL index
// names must be unqualified.
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
			drop_name_sql := if name.contains('.') {
				quote_identifier(.pg, name)
			} else {
				schema := ctx.postgresql_table_schema(table)!
				'${quote_identifier_component(.pg, schema)}.${quote_identifier_component(.pg, name)}'
			}
			ctx.execute('DROP INDEX ${drop_name_sql};')!
		}
		.sqlite {
			drop_name := if name.contains('.') {
				name
			} else {
				schema := ctx.sqlite_table_schema(table)!
				'${schema}.${name}'
			}
			ctx.execute('DROP INDEX ${quote_identifier(ctx.dialect, drop_name)};')!
		}
	}
}

fn (mut ctx Context) postgresql_table_schema(table string) !string {
	if table.contains('.') {
		return table.all_before('.')
	}
	rows := ctx.execute("SELECT n.nspname FROM pg_catalog.pg_class AS c JOIN pg_catalog.pg_namespace AS n ON n.oid = c.relnamespace WHERE c.relname = ${string_literal_sql(.pg,
		table)} AND c.relkind IN ('r', 'p', 'v', 'm', 'f') AND pg_catalog.pg_table_is_visible(c.oid) LIMIT 1;")!
	if rows.len != 1 || rows[0].vals.len == 0 || rows[0].vals[0] == '' {
		return error('PostgreSQL remove_index could not resolve table `${table}` to a schema')
	}
	return rows[0].vals[0]
}

fn (mut ctx Context) sqlite_table_schema(table string) !string {
	if table.contains('.') {
		return table.all_before('.')
	}
	database_rows := ctx.execute('PRAGMA database_list;')!
	mut schemas := ['temp', 'main']
	for row in database_rows {
		if row.vals.len < 2 {
			return error('SQLite PRAGMA database_list returned ${row.vals.len} columns; expected at least 2')
		}
		schema := row.vals[1]
		if schema !in schemas {
			schemas << schema
		}
	}
	for schema in schemas {
		rows := ctx.execute("SELECT 1 FROM ${quote_identifier_component(.sqlite, schema)}.sqlite_schema WHERE type = 'table' AND name = ${string_literal_sql(.sqlite,
			table)} COLLATE NOCASE LIMIT 1;")!
		if rows.len > 0 {
			return schema
		}
	}
	return error('SQLite remove_index could not resolve table `${table}` to a database')
}

fn column_name_key(dialect Dialect, name string) string {
	if dialect in [.sqlite, .mysql] {
		return name.to_lower_ascii()
	}
	return name
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

fn validate_sqlite_add_column(mut ctx Context, column Column) ! {
	if column_primary_key(column) || column_unique(column) || column_auto_increment(column) {
		return error('SQLite add_column does not support primary-key, unique, or auto-increment columns; rebuild the table in the migration')
	}
	if default_sql := column.default_sql {
		if sqlite_add_column_default_is_nonconstant(default_sql) {
			return error('SQLite add_column does not support nonconstant default `${default_sql}`; rebuild the table in the migration')
		}
		if sqlite_numeric_default_uses_digit_separators(default_sql)
			&& ctx.sqlite_version_number()! < sqlite_digit_separator_min_version {
			return error('SQLite add_column default `${default_sql}` uses numeric digit separators, which require SQLite 3.46.0 or newer')
		}
	}
	if !column_nullable(column) && !sqlite_has_non_null_default(column) {
		return error('SQLite add_column requires a non-NULL default for a NOT NULL column; rebuild the table in the migration')
	}
}

fn (mut ctx Context) sqlite_version_number() !int {
	if version := ctx.sqlite_runtime_version {
		return version
	}
	rows := ctx.execute('SELECT sqlite_version();')!
	version := sqlite_version_number(rows)!
	ctx.sqlite_runtime_version = version
	return version
}

fn sqlite_version_number(rows []orm.Row) !int {
	if rows.len != 1 || rows[0].vals.len == 0 {
		return error('could not determine SQLite runtime version')
	}
	parts := rows[0].vals[0].split('.')
	if parts.len != 3 {
		return error('unsupported SQLite runtime version `${rows[0].vals[0]}`')
	}
	major := strconv.atoi(parts[0]) or {
		return error('unsupported SQLite runtime version `${rows[0].vals[0]}`')
	}
	minor := strconv.atoi(parts[1]) or {
		return error('unsupported SQLite runtime version `${rows[0].vals[0]}`')
	}
	patch := strconv.atoi(parts[2]) or {
		return error('unsupported SQLite runtime version `${rows[0].vals[0]}`')
	}
	return major * 1_000_000 + minor * 1_000 + patch
}

fn sqlite_numeric_default_uses_digit_separators(default_sql string) bool {
	literal := sqlite_unwrapped_default(sqlite_default_without_comments(default_sql))
	if !literal.contains('_') {
		return false
	}
	if cast_expression := sqlite_cast_expression(literal) {
		return sqlite_numeric_default_uses_digit_separators(cast_expression.value)
	}
	normalized := sqlite_numeric_literal_without_separators(literal) or { return false }
	if normalized.len > 2 && normalized[0] == `0` && normalized[1] in [u8(`x`), `X`] {
		return normalized[2..].bytes().all(it.is_hex_digit())
	}
	return sqlite_is_decimal_numeric_literal(normalized)
}

fn sqlite_add_column_default_is_nonconstant(default_sql string) bool {
	normalized :=
		sqlite_default_without_leading_signs(sqlite_default_without_comments(default_sql)).to_upper()
	if sqlite_default_starts_with_current_time_keyword(normalized) {
		return true
	}
	if core := sqlite_parenthesized_default_core(normalized) {
		return !sqlite_is_constant_default_expression(core)
	}
	return false
}

struct SqliteCastExpression {
	value string
}

fn sqlite_is_constant_default_expression(default_sql string) bool {
	if sqlite_is_literal_default(default_sql) {
		return true
	}
	cast_expression := sqlite_cast_expression(default_sql) or { return false }
	return sqlite_is_constant_default_expression(cast_expression.value)
}

fn sqlite_cast_expression(default_sql string) ?SqliteCastExpression {
	literal := sqlite_default_without_leading_signs(default_sql)
	if literal.len < 6 || literal[..4].to_upper() != 'CAST' {
		return none
	}
	mut open_parenthesis := 4
	for open_parenthesis < literal.len && literal[open_parenthesis].is_space() {
		open_parenthesis++
	}
	if open_parenthesis >= literal.len || literal[open_parenthesis] != `(` {
		return none
	}
	close_parenthesis := sqlite_matching_parenthesis(literal, open_parenthesis) or { return none }
	if literal[close_parenthesis + 1..].trim_space() != '' {
		return none
	}
	inner := literal[open_parenthesis + 1..close_parenthesis]
	as_index := sqlite_top_level_as_index(inner) or { return none }
	value := inner[..as_index].trim_space()
	type_name := inner[as_index + 2..].trim_space()
	if value == '' || type_name == '' {
		return none
	}
	return SqliteCastExpression{
		value: value
	}
}

fn sqlite_matching_parenthesis(expression string, open_parenthesis int) ?int {
	mut depth := 0
	mut quote := u8(0)
	mut index := open_parenthesis
	for index < expression.len {
		character := expression[index]
		if quote != 0 {
			if character == quote {
				if index + 1 < expression.len && expression[index + 1] == quote {
					index += 2
					continue
				}
				quote = 0
			}
			index++
			continue
		}
		if character in [u8(39), 34] {
			quote = character
		} else if character == `(` {
			depth++
		} else if character == `)` {
			depth--
			if depth == 0 {
				return index
			}
		}
		index++
	}
	return none
}

fn sqlite_top_level_as_index(expression string) ?int {
	mut depth := 0
	mut quote := u8(0)
	mut index := 0
	for index + 1 < expression.len {
		character := expression[index]
		if quote != 0 {
			if character == quote {
				if index + 1 < expression.len && expression[index + 1] == quote {
					index += 2
					continue
				}
				quote = 0
			}
			index++
			continue
		}
		if character in [u8(39), 34] {
			quote = character
		} else if character == `(` {
			depth++
		} else if character == `)` {
			depth--
		} else if depth == 0 && character in [u8(`A`), `a`]
			&& expression[index + 1] in [u8(`S`), `s`]
			&& (index == 0 || !sqlite_identifier_character(expression[index - 1]))
			&& (index + 2 == expression.len || !sqlite_identifier_character(expression[index + 2])) {
			return index
		}
		index++
	}
	return none
}

fn sqlite_identifier_character(character u8) bool {
	return character.is_alnum() || character == `_`
}

fn sqlite_default_starts_with_current_time_keyword(default_sql string) bool {
	return sqlite_default_first_identifier(default_sql) in ['CURRENT_TIME', 'CURRENT_DATE',
		'CURRENT_TIMESTAMP']
}

fn sqlite_default_first_identifier(default_sql string) string {
	mut token_end := 0
	for token_end < default_sql.len
		&& (default_sql[token_end].is_alnum() || default_sql[token_end] == `_`) {
		token_end++
	}
	return default_sql[..token_end]
}

fn sqlite_has_non_null_default(column Column) bool {
	if default_sql := column.default_sql {
		normalized := sqlite_default_without_comments(default_sql).trim_space()
		return normalized != '' && !sqlite_default_resolves_to_null(normalized)
	}
	return false
}

fn sqlite_default_resolves_to_null(default_sql string) bool {
	literal := sqlite_unwrapped_default(default_sql)
	if sqlite_default_first_identifier(literal.to_upper()) == 'NULL' {
		return true
	}
	cast_expression := sqlite_cast_expression(literal) or { return false }
	return sqlite_default_resolves_to_null(cast_expression.value)
}

fn sqlite_is_literal_default(default_sql string) bool {
	literal := sqlite_default_without_leading_signs(default_sql)
	if literal == '' {
		return false
	}
	upper := literal.to_upper()
	if upper in ['NULL', 'TRUE', 'FALSE'] {
		return true
	}
	if sqlite_is_single_quoted_literal(literal) {
		return true
	}
	if literal.len > 1 && literal[0] in [u8(`x`), `X`]
		&& sqlite_is_single_quoted_literal(literal[1..]) {
		return true
	}
	if literal.starts_with('(') && literal.ends_with(')') {
		return sqlite_is_literal_default(literal[1..literal.len - 1])
	}
	numeric_literal := sqlite_numeric_literal_without_separators(literal) or { return false }
	if numeric_literal.len > 2 && numeric_literal[0] == `0` && numeric_literal[1] in [u8(`x`), `X`]
		&& numeric_literal[2..].bytes().all(it.is_hex_digit()) {
		return true
	}
	return sqlite_is_decimal_numeric_literal(numeric_literal)
}

fn sqlite_is_decimal_numeric_literal(literal string) bool {
	mut index := 0
	mut mantissa_digits := 0
	for index < literal.len && literal[index].is_digit() {
		index++
		mantissa_digits++
	}
	if index < literal.len && literal[index] == `.` {
		index++
		for index < literal.len && literal[index].is_digit() {
			index++
			mantissa_digits++
		}
	}
	if mantissa_digits == 0 {
		return false
	}
	if index < literal.len && literal[index] in [u8(`e`), `E`] {
		index++
		if index < literal.len && literal[index] in [u8(`+`), `-`] {
			index++
		}
		exponent_start := index
		for index < literal.len && literal[index].is_digit() {
			index++
		}
		if index == exponent_start {
			return false
		}
	}
	return index == literal.len
}

fn sqlite_numeric_literal_without_separators(literal string) ?string {
	if !literal.contains('_') {
		return literal
	}
	is_hex := literal.len > 2 && literal[0] == `0` && literal[1] in [u8(`x`), `X`]
	mut normalized := []u8{cap: literal.len}
	for i, character in literal {
		if character != `_` {
			normalized << character
			continue
		}
		if i == 0 || i == literal.len - 1 {
			return none
		}
		previous_is_digit := if is_hex {
			literal[i - 1].is_hex_digit()
		} else {
			literal[i - 1].is_digit()
		}
		next_is_digit := if is_hex {
			literal[i + 1].is_hex_digit()
		} else {
			literal[i + 1].is_digit()
		}
		if !previous_is_digit || !next_is_digit {
			return none
		}
	}
	return normalized.bytestr()
}

fn sqlite_is_single_quoted_literal(literal string) bool {
	if literal.len < 2 || literal[0] != 39 || literal[literal.len - 1] != 39 {
		return false
	}
	mut i := 1
	for i < literal.len - 1 {
		if literal[i] == 39 {
			if i + 1 >= literal.len - 1 || literal[i + 1] != 39 {
				return false
			}
			i += 2
			continue
		}
		i++
	}
	return true
}

fn sqlite_unwrapped_default(default_sql string) string {
	mut literal := default_sql.trim_space()
	for {
		literal = sqlite_default_without_leading_signs(literal)
		core := sqlite_parenthesized_default_core(literal) or { return literal }
		literal = core
	}
	return literal
}

fn sqlite_parenthesized_default_core(default_sql string) ?string {
	literal := default_sql.trim_space()
	if literal.len < 2 || literal[0] != `(` {
		return none
	}
	mut depth := 0
	mut quote := u8(0)
	mut i := 0
	for i < literal.len {
		ch := literal[i]
		if quote != 0 {
			if ch == quote {
				if i + 1 < literal.len && literal[i + 1] == quote {
					i += 2
					continue
				}
				quote = 0
			}
			i++
			continue
		}
		if ch in [u8(39), 34] {
			quote = ch
		} else if ch == `(` {
			depth++
		} else if ch == `)` {
			depth--
			if depth == 0 {
				return literal[1..i]
			}
		}
		i++
	}
	return none
}

fn sqlite_default_without_leading_signs(default_sql string) string {
	mut literal := default_sql.trim_space()
	for literal.len > 0 && literal[0] in [u8(`+`), `-`] {
		literal = literal[1..].trim_space()
	}
	return literal
}

fn sqlite_default_without_comments(default_sql string) string {
	mut result := []u8{cap: default_sql.len}
	mut i := 0
	mut quote := u8(0)
	for i < default_sql.len {
		ch := default_sql[i]
		if quote != 0 {
			result << ch
			if ch == quote {
				if i + 1 < default_sql.len && default_sql[i + 1] == quote {
					result << default_sql[i + 1]
					i += 2
					continue
				}
				quote = 0
			}
			i++
			continue
		}
		if ch == 39 || ch == 34 {
			quote = ch
			result << ch
			i++
			continue
		}
		if ch == 47 && i + 1 < default_sql.len && default_sql[i + 1] == 42 {
			i += 2
			for i + 1 < default_sql.len && !(default_sql[i] == 42 && default_sql[i + 1] == 47) {
				i++
			}
			if i + 1 < default_sql.len {
				i += 2
			}
			result << u8(32)
			continue
		}
		if ch == 45 && i + 1 < default_sql.len && default_sql[i + 1] == 45 {
			i += 2
			for i < default_sql.len && default_sql[i] !in [u8(10), 13] {
				i++
			}
			result << u8(32)
			continue
		}
		result << ch
		i++
	}
	return result.bytestr()
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
		generated_index_name(index)
	}
	if index.name != '' {
		validate_identifier_for_dialect(dialect, name, 'index')!
	} else {
		validate_identifier(name, 'index')!
	}
	return bounded_identifier_name(dialect, name, index.name == '', 'index')
}

fn generated_index_name(index Index) string {
	base := 'index_${index.table.replace('.', '_')}_on_${index.columns.join('_and_')}'
	mut identity := '${index.table.len}:${index.table}'
	for column in index.columns {
		identity += ':${column.len}:${column}'
	}
	return '${base}_${fnv1a.sum64_string(identity).hex()}'
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
		generated_foreign_key_name(dialect, key)
	}
	validate_unqualified_identifier(name, 'foreign key')!
	return bounded_identifier_name(dialect, name, key.name == '', 'foreign key')
}

fn generated_foreign_key_name(dialect Dialect, key ForeignKey) string {
	base := 'fk_${key.from_table.replace('.', '_')}_${key.column}'
	if dialect in [.pg, .mysql] {
		identity := '${key.from_table.len}:${key.from_table}:${key.column.len}:${key.column}:${key.to_table.len}:${key.to_table}:${key.primary_key.len}:${key.primary_key}'
		return '${base}_${fnv1a.sum64_string(identity).hex()}'
	}
	return base
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
	return value.split('.').map(quote_identifier_component(dialect, it)).join('.')
}

fn quote_identifier_component(dialect Dialect, value string) string {
	quote := if dialect == .mysql { '`' } else { '"' }
	return '${quote}${value.replace(quote, quote + quote)}${quote}'
}

fn escape_literal(value string) string {
	return value.replace("'", "''")
}

fn escape_postgresql_literal(value string) string {
	return value.replace('\\', '\\\\').replace("'", "''")
}

fn string_literal_sql(dialect Dialect, value string) string {
	return match dialect {
		.sqlite { "'${escape_literal(value)}'" }
		.pg { "E'${escape_postgresql_literal(value)}'" }
		.mysql { "X'${value.hex()}'" }
	}
}
