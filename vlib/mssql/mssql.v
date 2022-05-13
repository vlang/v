module mssql

pub struct Connection {
mut:
	henv C.SQLHENV = C.SQLHENV(C.SQL_NULL_HENV) // Environment
	hdbc C.SQLHDBC = C.SQLHDBC(C.SQL_NULL_HDBC) // Connection handle
pub mut:
	conn_str string
}

// connect to db
pub fn (mut conn Connection) connect(conn_str string) ?bool {
	conn_str_c := unsafe { &C.SQLCHAR(conn_str.str) }
	mut retcode := C.SQLRETURN(C.SQL_SUCCESS)
	// Allocate environment handle
	retcode = C.SQLAllocHandle(C.SQLSMALLINT(C.SQL_HANDLE_ENV), C.SQLHANDLE(C.SQL_NULL_HANDLE),
		unsafe { &C.SQLHANDLE(&conn.henv) })
	check_error(retcode, 'SQLAllocHandle(SQL_HANDLE_ENV)', C.SQLHANDLE(conn.henv), C.SQLSMALLINT(C.SQL_HANDLE_ENV))?

	// Set the ODBC version environment attribute
	retcode = C.SQLSetEnvAttr(conn.henv, C.SQLINTEGER(C.SQL_ATTR_ODBC_VERSION), &C.SQLPOINTER(C.SQL_OV_ODBC3),
		C.SQLINTEGER(0))
	check_error(retcode, 'SQLSetEnvAttr(SQL_ATTR_ODBC_VERSION)', C.SQLHANDLE(conn.henv),
		C.SQLSMALLINT(C.SQL_HANDLE_ENV))?

	// Allocate connection handle
	retcode = C.SQLAllocHandle(C.SQLSMALLINT(C.SQL_HANDLE_DBC), C.SQLHANDLE(conn.henv),
		unsafe { &C.SQLHANDLE(&conn.hdbc) })
	check_error(retcode, 'SQLAllocHandle(SQL_HANDLE_DBC)', C.SQLHANDLE(conn.hdbc), C.SQLSMALLINT(C.SQL_HANDLE_DBC))?

	// Set login timeout to 5 seconds
	retcode = C.SQLSetConnectAttr(conn.hdbc, C.SQLINTEGER(C.SQL_LOGIN_TIMEOUT), C.SQLPOINTER(5),
		C.SQLINTEGER(0))
	check_error(retcode, 'SQLSetConnectAttr(SQL_LOGIN_TIMEOUT)', C.SQLHANDLE(conn.hdbc),
		C.SQLSMALLINT(C.SQL_HANDLE_DBC))?

	// Connect to data source
	mut outstr := [1024]char{}
	mut outstrlen := C.SQLSMALLINT(0)
	retcode = C.SQLDriverConnect(conn.hdbc, C.SQLHWND(0), conn_str_c, C.SQLSMALLINT(C.SQL_NTS),
		&C.SQLCHAR(&outstr[0]), C.SQLSMALLINT(sizeof(outstr)), &outstrlen, C.SQLUSMALLINT(C.SQL_DRIVER_NOPROMPT))
	check_error(retcode, 'SQLDriverConnect()', C.SQLHANDLE(conn.hdbc), C.SQLSMALLINT(C.SQL_HANDLE_DBC))?
	conn.conn_str = conn_str
	return true
}

// close - closes the connection.
pub fn (mut conn Connection) close() {
	// Connection
	if conn.hdbc != C.SQLHDBC(C.SQL_NULL_HDBC) {
		C.SQLDisconnect(conn.hdbc)
		C.SQLFreeHandle(C.SQLSMALLINT(C.SQL_HANDLE_DBC), C.SQLHANDLE(conn.hdbc))
		conn.hdbc = C.SQLHDBC(C.SQL_NULL_HDBC)
	}
	// Environment
	if conn.henv != C.SQLHENV(C.SQL_NULL_HENV) {
		C.SQLFreeHandle(C.SQLSMALLINT(C.SQL_HANDLE_ENV), C.SQLHANDLE(conn.henv))
		conn.henv = C.SQLHENV(C.SQL_NULL_HENV)
	}
}

// query executes a sql query
pub fn (mut conn Connection) query(q string) ?Result {
	mut hstmt := new_hstmt(conn.hdbc)?
	defer {
		hstmt.close()
	}

	hstmt.exec(q)?

	affected := hstmt.retrieve_affected_rows()?

	hstmt.prepare_read()?
	raw_rows := hstmt.read_rows()?

	mut res := Result{
		rows: []Row{}
		num_rows_affected: affected
	}

	for rr in raw_rows {
		res.rows << Row{
			vals: rr
		}
	}

	return res
}

// check_error checks odbc return code and extract error string if available
fn check_error(e C.SQLRETURN, s string, h C.SQLHANDLE, t C.SQLSMALLINT) ? {
	if e != C.SQLRETURN(C.SQL_SUCCESS) && e != C.SQLRETURN(C.SQL_SUCCESS_WITH_INFO) {
		err_str := extract_error(s, h, t)
		return error(err_str)
	}
}

// extract_error extracts error string from odbc
fn extract_error(fnName string, handle C.SQLHANDLE, tp C.SQLSMALLINT) string {
	mut err_str := fnName
	mut i := 0
	mut native_error := C.SQLINTEGER(0)
	mut sql_state := [7]char{}
	mut message_text := [256]char{}
	mut text_length := C.SQLSMALLINT(0)
	mut ret := C.SQLRETURN(C.SQL_SUCCESS)

	for ret == C.SQLRETURN(C.SQL_SUCCESS) {
		i++
		ret = C.SQLGetDiagRec(tp, handle, C.SQLSMALLINT(i), &C.SQLCHAR(&sql_state[0]),
			&native_error, &C.SQLCHAR(&message_text[0]), C.SQLSMALLINT(sizeof(message_text)),
			&text_length)

		// add driver error string
		if ret == C.SQLRETURN(C.SQL_SUCCESS) || ret == C.SQLRETURN(C.SQL_SUCCESS_WITH_INFO) {
			unsafe {
				state_str := (&sql_state[0]).vstring()
				native_error_code := int(native_error)
				txt_str := (&message_text[0]).vstring()
				err_str += '\n\todbc=$state_str:$i:$native_error_code:$txt_str'
			}
		}
	}
	return err_str
}
