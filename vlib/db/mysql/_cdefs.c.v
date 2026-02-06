module mysql

@[typedef]
pub struct C.MYSQL {
}

@[typedef]
pub struct C.MYSQL_RES {
}

@[typedef]
pub struct C.MYSQL_FIELD {
	name             &u8 // Name of column
	org_name         &u8 // Original column name, if an alias
	table            &u8 // Table of column if column was a field
	org_table        &u8 // Org table name, if table was an alias
	db               &u8 // Name of the database that the field comes from
	catalog          &u8 // Catalog for table
	def              &u8 // Default value (set by `mysql_list_fields`)
	length           int // Width of column (create length)
	max_length       int // Max width for selected set
	name_length      u32
	org_name_length  u32
	table_length     u32
	org_table_length u32
	db_length        u32
	catalog_length   u32
	def_length       u32
	flags            u32 // Bit-flags that describe the field
	decimals         u32 // Number of decimals in field
	charsetnr        u32 // Character set
	type             int // Type of field. See enums.v for types
}

// C.mysql_init allocates or initializes a MYSQL object suitable for `mysql_real_connect()`.
fn C.mysql_init(mysql &C.MYSQL) &C.MYSQL

// C.mysql_real_connect attempts to establish a connection to a MySQL server running on `host`.
fn C.mysql_real_connect(mysql &C.MYSQL, host &char, user &char, passwd &char, db &char, port u32, unix_socket &char,
	client_flag ConnectionFlag) &C.MYSQL

// C.mysql_query executes the SQL statement pointed to by the null-terminated string `stmt_str`.
fn C.mysql_query(mysql &C.MYSQL, const_q charptr) int

// C.mysql_use_result initiates a result set retrieval but does not actually read
// the result set into the client like `mysql_store_result()` does.
fn C.mysql_use_result(mysql &C.MYSQL)

// C.mysql_real_query executes the SQL statement pointed to by `stmt_str`,
// a string length bytes long.
fn C.mysql_real_query(mysql &C.MYSQL, q &u8, len u32) int

// C.mysql_select_db causes the database specified by `db` to become
// the default (current) database on the connection specified by mysql.
fn C.mysql_select_db(mysql &C.MYSQL, db &u8) int

// C.mysql_change_user changes the user and causes the database specified by `db` to become
// the default (current) database on the connection specified by `mysql`.
fn C.mysql_change_user(mysql &C.MYSQL, user &u8, password &u8, db &u8) bool

// C.mysql_affected_rows returns the number of rows changed, deleted,
// or inserted by the last statement if it was an `UPDATE`, `DELETE`, or `INSERT`.
fn C.mysql_affected_rows(mysql &C.MYSQL) u64

// C.mysql_options sets extra connect options and affects behavior for a connection.
fn C.mysql_options(mysql &C.MYSQL, option int, arg voidptr) int

// C.mysql_get_option returns the current value of an option settable using `mysql_options()`.
fn C.mysql_get_option(mysql &C.MYSQL, option int, arg voidptr) int

// C.mysql_list_tables returns a result set consisting of table names in the current database
// that match the simple regular expression specified by the `wild` parameter.
// `wild` may contain the wildcard characters `%` or `_`,
// or may be a `NULL` pointer to match all tables.
fn C.mysql_list_tables(mysql &C.MYSQL, wild &u8) &C.MYSQL_RES

// C.mysql_num_fields returns the number of columns in a result set.
fn C.mysql_num_fields(res &C.MYSQL_RES) int

// C.mysql_num_rows returns the number of rows in the result set.
fn C.mysql_num_rows(res &C.MYSQL_RES) u64

// C.mysql_autocommit sets autocommit mode on if `mode` is 1, off if `mode` is 0.
fn C.mysql_autocommit(mysql &C.MYSQL, mode bool) int

// C.mysql_commit commits the current transaction.
fn C.mysql_commit(mysql &C.MYSQL) int

// C.mysql_rollback rollback the current transaction.
fn C.mysql_rollback(mysql &C.MYSQL) int

// C.mysql_refresh flush tables or caches, or resets replication server information.
fn C.mysql_refresh(mysql &C.MYSQL, options u32) int

// C.mysql_reset_connection resets the connection to clear the session state.
fn C.mysql_reset_connection(mysql &C.MYSQL) int

// C.mysql_ping checks whether the connection to the server is working.
// Returns zero if the connection to the server is active. Nonzero if an error occurred.
fn C.mysql_ping(mysql &C.MYSQL) int

// C.mysql_store_result reads the entire result of a query to the client,
// allocates a `MYSQL_RES` structure, and places the result into this structure.
// It is a synchronous function.
fn C.mysql_store_result(mysql &C.MYSQL) &C.MYSQL_RES

// C.mysql_fetch_row retrieves the next row of a result set.
fn C.mysql_fetch_row(res &C.MYSQL_RES) &charptr

// C.mysql_fetch_fields returns an array of all `MYSQL_FIELD` structures for a result set.
// Each structure provides the field definition for one column of the result set.
fn C.mysql_fetch_fields(res &C.MYSQL_RES) &C.MYSQL_FIELD

// C.mysql_free_result frees the memory allocated for a result set by `mysql_store_result()`,
// `mysql_use_result()`, `mysql_list_dbs()`, and so forth.
fn C.mysql_free_result(res &C.MYSQL_RES)

// C.mysql_real_escape_string creates a legal SQL string for use in an SQL statement.
fn C.mysql_real_escape_string(mysql &C.MYSQL, to &u8, from &u8, len u64) u64

// C.mysql_close closes a previously opened connection.
fn C.mysql_close(sock &C.MYSQL)

// C.mysql_info retrieves a string providing information about the most recently executed statement.
fn C.mysql_info(mysql &C.MYSQL) &u8

// C.mysql_get_host_info returns a string describing the type of connection in use,
// including the server host name.
fn C.mysql_get_host_info(mysql &C.MYSQL) &u8

// C.mysql_get_server_info returns a string that represents
// the MySQL server version (for example, "8.0.33").
fn C.mysql_get_server_info(mysql &C.MYSQL) &u8

// C.mysql_get_server_version returns an integer that represents the MySQL server version.
// The value has the format `XYYZZ` where `X` is the major version,
// `YY` is the release level (or minor version),
// and `ZZ` is the sub-version within the release level:
// `major_version*10000 + release_level*100 + sub_version`
// For example, "8.0.33" is returned as 80033.
fn C.mysql_get_server_version(mysql &C.MYSQL) u64

// C.mysql_get_client_version returns an integer that represents the MySQL client library version.
// The value has the format `XYYZZ` where `X` is the major version,
// `YY` is the release level (or minor version),
// and `ZZ` is the sub-version within the release level:
// `major_version*10000 + release_level*100 + sub_version`
// For example, "8.0.33" is returned as 80033.
fn C.mysql_get_client_version() u64

// C.mysql_get_client_info returns a string that represents
// the MySQL client library version (for example, "8.0.33").
fn C.mysql_get_client_info() &u8

// C.mysql_error returns a null-terminated string containing the error message
// for the most recently invoked API function that failed.
fn C.mysql_error(mysql &C.MYSQL) &u8

// C.mysql_errno returns the error code for the most recently invoked API function that can succeed or fail.
fn C.mysql_errno(mysql &C.MYSQL) int

// C.mysql_dump_debug_info instructs the server to write debugging information to the error log.
fn C.mysql_dump_debug_info(mysql &C.MYSQL) int

// C.mysql_debug does a `DBUG_PUSH` with the given string.
fn C.mysql_debug(debug &u8)
