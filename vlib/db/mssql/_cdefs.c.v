module mssql

fn C.SQLAllocHandle(handle_type C.SQLSMALLINT, input_handle C.SQLHANDLE, output_handle &C.SQLHANDLE) C.SQLRETURN

fn C.SQLSetEnvAttr(environment_handle C.SQLHENV, attribute C.SQLINTEGER, value C.SQLPOINTER, string_length C.SQLINTEGER) C.SQLRETURN

fn C.SQLGetDiagRec(handle_type C.SQLSMALLINT, handle C.SQLHANDLE, rec_number C.SQLSMALLINT, sql_state &C.SQLCHAR,
	native_error &C.SQLINTEGER, message_text &C.SQLCHAR, buffer_length C.SQLSMALLINT, text_length &C.SQLSMALLINT) C.SQLRETURN

fn C.SQLSetConnectAttr(connection_handle C.SQLHDBC, attribute C.SQLINTEGER, value C.SQLPOINTER, string_length C.SQLINTEGER) C.SQLRETURN

fn C.SQLDriverConnect(hdbc C.SQLHDBC, hwnd C.SQLHWND, sz_conn_str_in &C.SQLCHAR, cb_conn_str_in C.SQLSMALLINT,
	sz_conn_str_out &C.SQLCHAR, cb_conn_str_out_max C.SQLSMALLINT, pcb_conn_str_out &C.SQLSMALLINT, f_driver_completion C.SQLUSMALLINT) C.SQLRETURN

fn C.SQLDisconnect(connection_handle C.SQLHDBC) C.SQLRETURN

fn C.SQLExecDirect(statement_handle C.SQLHSTMT, statement_text &C.SQLCHAR, text_length C.SQLINTEGER) C.SQLRETURN

fn C.SQLBindCol(statement_handle C.SQLHSTMT, column_number C.SQLUSMALLINT, target_type C.SQLSMALLINT, target_value C.SQLPOINTER,
	buffer_length C.SQLLEN, str_len_or_ind &C.SQLLEN) C.SQLRETURN

fn C.SQLFetch(statement_handle C.SQLHSTMT) C.SQLRETURN

fn C.SQLFreeHandle(handle_type C.SQLSMALLINT, handle C.SQLHANDLE) C.SQLRETURN

fn C.SQLNumResultCols(statement_handle C.SQLHSTMT, column_count &C.SQLSMALLINT) C.SQLRETURN

fn C.SQLColAttribute(statement_handle C.SQLHSTMT, column_number C.SQLUSMALLINT, field_identifier C.SQLUSMALLINT,
	character_attribute C.SQLPOINTER, buffer_length C.SQLSMALLINT, string_length C.SQLSMALLINT, numeric_attribute &C.SQLLEN) C.SQLRETURN

fn C.SQLRowCount(statement_handle C.SQLHSTMT, row_count &C.SQLLEN) C.SQLRETURN
