module db

import db.sqlite

$if db_mysql ? {
	import db.mysql
}
$if db_pg ? {
	import db.pg
}
$if db_mssql ? {
	import db.mssql
}

// DriverKind selects the backend used by `open`.
pub enum DriverKind {
	sqlite
	mysql
	pg
	mssql
}

// DriverRow is the normalized row type returned by `Driver` implementations.
pub struct DriverRow {
pub mut:
	vals  []string
	names []string
}

// val returns the value at `index`.
pub fn (row DriverRow) val(index int) string {
	return row.vals[index]
}

// values returns all row values.
pub fn (row DriverRow) values() []string {
	return row.vals.clone()
}

// get_string returns the value for the given column name, or '' if it is missing.
pub fn (row DriverRow) get_string(col_name string) string {
	for i, name in row.names {
		if name == col_name {
			if i < row.vals.len {
				return row.vals[i]
			}
			return ''
		}
	}
	return ''
}

// Driver is the minimal common database connection interface exposed by the
// top-level `db` module. Backend-specific APIs remain available in their
// existing modules.
pub interface Driver {
mut:
	exec(query string) ![]DriverRow
	exec_one(query string) !DriverRow
	exec_param_many(query string, params []string) ![]DriverRow
	validate() !bool
	reset() !
	close() !
}

// DriverConfig contains the common connection fields used by `open`.
//
// SQLite uses `path` or, when empty, `dbname`.
// PostgreSQL/MySQL use the network/user fields. MSSQL also accepts ODBC
// fields such as `conn_str`, `dsn`, `driver`, `server`, `uid`, `pwd`, and
// `options`.
pub struct DriverConfig {
pub:
	kind DriverKind

	path     string
	host     string
	port     int
	user     string
	username string
	password string
	dbname   string

	conn_str string
	dsn      string
	driver   string
	server   string
	uid      string
	pwd      string
	options  map[string]string

	ssl_mode   string
	ssl_key    string
	ssl_cert   string
	ssl_ca     string
	ssl_crl    string
	ssl_capath string
	ssl_cipher string
}

@[heap]
struct SqliteDriver {
mut:
	conn sqlite.DB
}

// open creates a normalized database driver for the selected backend.
//
// SQLite support is available by default. PostgreSQL, MySQL, and MSSQL are
// compiled in only when `-d db_pg`, `-d db_mysql`, or `-d db_mssql` is used,
// avoiding unconditional dependencies on their C client libraries.
pub fn open(config DriverConfig) !&Driver {
	match config.kind {
		.sqlite {
			return open_sqlite(config)!
		}
		.mysql {
			$if db_mysql ? {
				return open_mysql(config)!
			} $else {
				return error('db: mysql driver support is not compiled in; rebuild with `-d db_mysql`')
			}
		}
		.pg {
			$if db_pg ? {
				return open_pg(config)!
			} $else {
				return error('db: pg driver support is not compiled in; rebuild with `-d db_pg`')
			}
		}
		.mssql {
			$if db_mssql ? {
				return open_mssql(config)!
			} $else {
				return error('db: mssql driver support is not compiled in; rebuild with `-d db_mssql`')
			}
		}
	}
}

fn open_sqlite(config DriverConfig) !&Driver {
	path := if config.path != '' { config.path } else { config.dbname }
	if path == '' {
		return error('db: sqlite driver requires DriverConfig.path or DriverConfig.dbname')
	}
	conn := sqlite.connect(path)!
	mut driver := &SqliteDriver{
		conn: conn
	}
	return driver
}

fn sqlite_row_to_row(row sqlite.Row) DriverRow {
	return DriverRow{
		vals:  row.vals.clone()
		names: row.names.clone()
	}
}

fn sqlite_rows_to_rows(rows []sqlite.Row) []DriverRow {
	mut normalized := []DriverRow{cap: rows.len}
	for row in rows {
		normalized << sqlite_row_to_row(row)
	}
	return normalized
}

fn (mut driver SqliteDriver) exec(query string) ![]DriverRow {
	return sqlite_rows_to_rows(driver.conn.exec(query)!)
}

fn (mut driver SqliteDriver) exec_one(query string) !DriverRow {
	return sqlite_row_to_row(driver.conn.exec_one(query)!)
}

fn (mut driver SqliteDriver) exec_param_many(query string, params []string) ![]DriverRow {
	return sqlite_rows_to_rows(driver.conn.exec_param_many(query, params)!)
}

fn (mut driver SqliteDriver) validate() !bool {
	return driver.conn.validate()!
}

fn (mut driver SqliteDriver) reset() ! {
	driver.conn.reset()!
}

fn (mut driver SqliteDriver) close() ! {
	driver.conn.close()!
}

$if db_mysql ? {
	@[heap]
	struct MysqlDriver {
	mut:
		conn mysql.DB
	}

	fn open_mysql(config DriverConfig) !&Driver {
		mut mysql_config := mysql.Config{
			host:       if config.host != '' { config.host } else { '127.0.0.1' }
			port:       if config.port > 0 { u32(config.port) } else { u32(3306) }
			user:       config.user
			username:   config.username
			password:   config.password
			dbname:     config.dbname
			ssl_mode:   mysql_ssl_mode(config.ssl_mode)!
			ssl_key:    config.ssl_key
			ssl_cert:   config.ssl_cert
			ssl_ca:     config.ssl_ca
			ssl_capath: config.ssl_capath
			ssl_cipher: config.ssl_cipher
		}
		if mysql_uses_ssl_files(config) {
			mysql_config.flag = .client_ssl
		}
		conn := mysql.connect(mysql_config)!
		mut driver := &MysqlDriver{
			conn: conn
		}
		return driver
	}

	fn mysql_uses_ssl_files(config DriverConfig) bool {
		return config.ssl_key != '' || config.ssl_cert != '' || config.ssl_ca != ''
			|| config.ssl_capath != '' || config.ssl_cipher != ''
	}

	fn mysql_ssl_mode(value string) !mysql.SslMode {
		normalized := value.to_lower().replace('-', '_')
		return match normalized {
			'', 'unset' { mysql.SslMode.unset }
			'disabled', 'disable' { mysql.SslMode.disabled }
			'preferred', 'prefer' { mysql.SslMode.preferred }
			'required', 'require' { mysql.SslMode.required }
			'verify_ca' { mysql.SslMode.verify_ca }
			'verify_identity', 'verify_full' { mysql.SslMode.verify_identity }
			else { error('db: unsupported mysql ssl_mode `${value}`') }
		}
	}

	fn mysql_row_to_row(row mysql.Row, names []string) DriverRow {
		return DriverRow{
			vals:  row.vals.clone()
			names: names.clone()
		}
	}

	fn mysql_rows_to_rows(rows []mysql.Row, names []string) []DriverRow {
		mut normalized := []DriverRow{cap: rows.len}
		for row in rows {
			normalized << mysql_row_to_row(row, names)
		}
		return normalized
	}

	fn mysql_result_to_rows(result mysql.Result) []DriverRow {
		if result.result == unsafe { nil } {
			return []DriverRow{}
		}
		return mysql_rows_to_rows(result.rows(), result.field_names())
	}

	fn mysql_row_set_to_rows(result mysql.RowSet) []DriverRow {
		return mysql_rows_to_rows(result.rows, result.names)
	}

	fn (mut driver MysqlDriver) exec(query string) ![]DriverRow {
		result := driver.conn.query(query)!
		if result.result == unsafe { nil } {
			return []DriverRow{}
		}
		defer {
			unsafe { result.free() }
		}
		return mysql_result_to_rows(result)
	}

	fn (mut driver MysqlDriver) exec_one(query string) !DriverRow {
		rows := driver.exec(query)!
		if rows.len == 0 {
			return DriverRow{}
		}
		return rows[0]
	}

	fn (mut driver MysqlDriver) exec_param_many(query string, params []string) ![]DriverRow {
		return mysql_row_set_to_rows(driver.conn.exec_param_many_result(query, params)!)
	}

	fn (mut driver MysqlDriver) validate() !bool {
		return driver.conn.validate()!
	}

	fn (mut driver MysqlDriver) reset() ! {
		driver.conn.reset()!
	}

	fn (mut driver MysqlDriver) close() ! {
		driver.conn.close()!
	}
}

$if db_pg ? {
	@[heap]
	struct PgDriver {
	mut:
		conn &pg.Conn = unsafe { nil }
	}

	fn pg_config_from_driver_config(config DriverConfig) !pg.Config {
		return pg.Config{
			host:     config.host
			port:     config.port
			user:     config.user
			username: config.username
			password: config.password
			dbname:   config.dbname
			ssl_mode: pg_ssl_mode(config.ssl_mode)!
			ssl_key:  config.ssl_key
			ssl_cert: config.ssl_cert
			ssl_ca:   config.ssl_ca
			ssl_crl:  config.ssl_crl
		}
	}

	fn open_pg(config DriverConfig) !&Driver {
		conn := pg.connect_direct(pg_config_from_driver_config(config)!)!
		mut driver := &PgDriver{
			conn: conn
		}
		return driver
	}

	fn pg_ssl_mode(value string) !pg.SslMode {
		normalized := value.to_lower().replace('-', '_')
		return match normalized {
			'', 'unset' { pg.SslMode.unset }
			'disable', 'disabled' { pg.SslMode.disable }
			'allow' { pg.SslMode.allow }
			'prefer', 'preferred' { pg.SslMode.prefer }
			'require', 'required' { pg.SslMode.require }
			'verify_ca' { pg.SslMode.verify_ca }
			'verify_full' { pg.SslMode.verify_full }
			else { error('db: unsupported pg ssl_mode `${value}`') }
		}
	}

	fn pg_row_to_row(row pg.Row, names []string) DriverRow {
		return DriverRow{
			vals:  row.values()
			names: names.clone()
		}
	}

	fn pg_result_to_rows(result pg.Result) []DriverRow {
		mut normalized := []DriverRow{cap: result.rows.len}
		for row in result.rows {
			normalized << pg_row_to_row(row, result.names)
		}
		return normalized
	}

	fn (mut driver PgDriver) exec(query string) ![]DriverRow {
		return pg_result_to_rows(driver.conn.exec_result(query)!)
	}

	fn (mut driver PgDriver) exec_one(query string) !DriverRow {
		rows := driver.exec(query)!
		if rows.len == 0 {
			return error('no row')
		}
		return rows[0]
	}

	fn (mut driver PgDriver) exec_param_many(query string, params []string) ![]DriverRow {
		return pg_result_to_rows(driver.conn.exec_param_many_result(query, params)!)
	}

	fn (mut driver PgDriver) validate() !bool {
		return driver.conn.validate()!
	}

	fn (mut driver PgDriver) reset() ! {
		driver.conn.reset()!
	}

	fn (mut driver PgDriver) close() ! {
		driver.conn.close()!
	}
}

$if db_mssql ? {
	@[heap]
	struct MssqlDriver {
	mut:
		conn mssql.Connection
	}

	fn open_mssql(config DriverConfig) !&Driver {
		conn := mssql.connect(mssql.Config{
			conn_str: config.conn_str
			dsn:      config.dsn
			driver:   config.driver
			server:   if config.server != '' { config.server } else { config.host }
			port:     config.port
			uid:      config.uid
			user:     config.user
			pwd:      config.pwd
			password: config.password
			dbname:   config.dbname
			options:  config.options
		})!
		mut driver := &MssqlDriver{
			conn: conn
		}
		return driver
	}

	fn mssql_row_to_row(row mssql.Row, names []string) DriverRow {
		return DriverRow{
			vals:  row.vals.clone()
			names: names.clone()
		}
	}

	fn mssql_rows_to_rows(rows []mssql.Row, names []string) []DriverRow {
		mut normalized := []DriverRow{cap: rows.len}
		for row in rows {
			normalized << mssql_row_to_row(row, names)
		}
		return normalized
	}

	fn mssql_result_to_rows(result mssql.Result) []DriverRow {
		return mssql_rows_to_rows(result.rows, result.names)
	}

	fn (mut driver MssqlDriver) exec(query string) ![]DriverRow {
		res := driver.conn.query(query)!
		return mssql_result_to_rows(res)
	}

	fn (mut driver MssqlDriver) exec_one(query string) !DriverRow {
		rows := driver.exec(query)!
		if rows.len == 0 {
			return error('db: query returned no rows')
		}
		return rows[0]
	}

	fn (mut driver MssqlDriver) exec_param_many(_ string, _ []string) ![]DriverRow {
		return error('db: mssql driver does not support exec_param_many')
	}

	fn (mut driver MssqlDriver) validate() !bool {
		driver.conn.query('SELECT 1')!
		return true
	}

	fn (mut driver MssqlDriver) reset() ! {
	}

	fn (mut driver MssqlDriver) close() ! {
		driver.conn.close()
	}
}
