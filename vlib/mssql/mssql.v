module mssql

#include "stdio.h"

pub struct Connection {
mut:
	henv C.SQLHENV   = C.SQLHENV(C.SQL_NULL_HENV)   // Environment
	hdbc C.SQLHDBC   = C.SQLHDBC(C.SQL_NULL_HDBC)   // Connection handle
pub mut:
	conn_str string
}

// connect to db
pub fn (mut conn Connection) connect(conn_str string) ?bool {
	conn_str_c := unsafe{ &C.SQLCHAR(conn_str.str) }
	mut retcode := C.SQLRETURN(C.SQL_SUCCESS)
	// Allocate environment handle
	retcode = C.SQLAllocHandle(C.SQLSMALLINT(C.SQL_HANDLE_ENV), C.SQLHANDLE(C.SQL_NULL_HANDLE), unsafe{&C.SQLHANDLE(&conn.henv)})
	check_error(retcode, "SQLAllocHandle(SQL_HANDLE_ENV)",
	 	C.SQLHANDLE(conn.henv), C.SQLSMALLINT(C.SQL_HANDLE_ENV))?
	
	// Set the ODBC version environment attribute
	retcode = C.SQLSetEnvAttr(conn.henv, C.SQLINTEGER(C.SQL_ATTR_ODBC_VERSION),
		&C.SQLPOINTER(C.SQL_OV_ODBC3), C.SQLINTEGER(0))
	check_error(retcode, "SQLSetEnvAttr(SQL_ATTR_ODBC_VERSION)",
		C.SQLHANDLE(conn.henv), C.SQLSMALLINT(C.SQL_HANDLE_ENV))?

	// Allocate connection handle
	retcode = C.SQLAllocHandle(C.SQLSMALLINT(C.SQL_HANDLE_DBC), C.SQLHANDLE(conn.henv), unsafe{&C.SQLHANDLE(&conn.hdbc)})
	check_error(retcode, "SQLAllocHandle(SQL_HANDLE_DBC)",
		C.SQLHANDLE(conn.hdbc), C.SQLSMALLINT(C.SQL_HANDLE_DBC))?

	// Set login timeout to 5 seconds
	retcode = C.SQLSetConnectAttr(conn.hdbc, C.SQLINTEGER(C.SQL_LOGIN_TIMEOUT), C.SQLPOINTER(5), C.SQLINTEGER(0))
	check_error(retcode, "SQLSetConnectAttr(SQL_LOGIN_TIMEOUT)",
		C.SQLHANDLE(conn.hdbc), C.SQLSMALLINT(C.SQL_HANDLE_DBC))?

	// Connect to data source
	mut outstr := [1024]char{}
	mut outstrlen := C.SQLSMALLINT(0)
	retcode = C.SQLDriverConnect(conn.hdbc, 
								C.SQLHWND(0),
								conn_str_c, 
								C.SQLSMALLINT(C.SQL_NTS), 
								&C.SQLCHAR(&outstr[0]),
								C.SQLSMALLINT(sizeof(outstr)),
								&outstrlen, 
								C.SQLUSMALLINT(C.SQL_DRIVER_NOPROMPT))
	check_error(retcode, "SQLDriverConnect()",
		C.SQLHANDLE(conn.hdbc), C.SQLSMALLINT(C.SQL_HANDLE_DBC))?
	return true
}

// close - closes the connection.
pub fn (mut conn Connection) close() {
	// TODO: check error?
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


pub fn (mut conn Connection) query(q string) ?string {
	mut retcode := C.SQLRETURN(C.SQL_SUCCESS)

	mut hstmt := C.SQLHSTMT(C.SQL_NULL_HSTMT)  // Statement handle
	// Allocate statement handle
	retcode = C.SQLAllocHandle(C.SQLSMALLINT(C.SQL_HANDLE_STMT), C.SQLHANDLE(conn.hdbc), unsafe{&C.SQLHANDLE(&hstmt)})
	check_error(retcode, "SQLAllocHandle(SQL_HANDLE_STMT)",
		C.SQLHANDLE(hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))?

	retcode = C.SQLExecDirect(hstmt,
	 	q.str, C.SQLINTEGER(C.SQL_NTS))
	check_error(retcode, "SQLExecDirect()", C.SQLHANDLE(hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))?

	buff := [256]char{}
	buffer_len := sizeof(buff)
	// Bind columns
	retcode = C.SQLBindCol(hstmt, C.SQLUSMALLINT(1), C.SQLSMALLINT(C.SQL_C_CHAR), C.SQLPOINTER(&buff[0]), C.SQLLEN(buffer_len), 0)
	check_error(retcode, "SQLBindCol()", C.SQLHANDLE(hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))?

	mut cumulate := ''
	// Fetch and print each row of data until SQL_NO_DATA returned.
	for i := 0; ; i++ {
		retcode = C.SQLFetch(hstmt)
		if retcode == C.SQLRETURN(C.SQL_SUCCESS) || retcode == C.SQLRETURN(C.SQL_SUCCESS_WITH_INFO) {
			cumulate += unsafe{ (&buff[0]).vstring() }
		}
		else {
			if retcode != C.SQLRETURN(C.SQL_NO_DATA) {
				check_error(retcode, "SQLFetch()", C.SQLHANDLE(hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))?
			}
			else {
				break
			}
		}
	}

	// Deallocate handle
	if hstmt != C.SQLHSTMT(C.SQL_NULL_HSTMT){
		// check error code?
		C.SQLFreeHandle(C.SQLSMALLINT(C.SQL_HANDLE_STMT), C.SQLHANDLE(hstmt))
	}
	return cumulate
}

fn check_error(e C.SQLRETURN, s string, h C.SQLHANDLE, t C.SQLSMALLINT) ? {
	if e != C.SQLRETURN(C.SQL_SUCCESS) && e != C.SQLRETURN(C.SQL_SUCCESS_WITH_INFO) {
		err_str := extract_error(s, h, t)
		return error(err_str)
	}
}

fn extract_error(fnName string, handle C.SQLHANDLE, tp C.SQLSMALLINT) string
{
	mut err_str := fnName
	mut i := 0
	mut native_error := C.SQLINTEGER(0)
	mut sql_state := [7]char{}
	// mut message_text := [256]C.SQLCHAR{}
	mut message_text := [256]char{}
	mut text_length := C.SQLSMALLINT(0)
	mut ret := C.SQL_SUCCESS

	// println("\nThe driver reported the following error ${fnName}")
	for ret == C.SQL_SUCCESS {
		i++
		ret = C.SQLGetDiagRec(tp, handle, C.SQLSMALLINT(i), &C.SQLCHAR(&sql_state[0]), &native_error,
			&C.SQLCHAR(&message_text[0]), C.SQLSMALLINT(sizeof(message_text)), &text_length)

		// if C.SQL_SUCCEEDED(ret) {
		if (ret)&(~1) == 0 {
			unsafe{
				err_str += ":odbc=${(&sql_state[0]).vstring()}:${i}:${int(native_error)}:${(&message_text[0]).vstring()}\n"
			}
		}
	}
	return err_str
}