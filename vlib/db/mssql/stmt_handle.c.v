module mssql

// HStmt is handle for sql statement
struct HStmt {
mut:
	// db connection reference. Owner is Connection struct.
	hdbc C.SQLHDBC = C.SQLHDBC(C.SQL_NULL_HDBC)
	// statement handle
	hstmt C.SQLHSTMT = C.SQLHSTMT(C.SQL_NULL_HSTMT)
	// fields used for computation
	column_count int = -1
	// columns
	buffers [][]char
	// indicators for each column. i64 is at least as wide as SQLLEN on both 32- and
	// 64-bit targets, and avoids arrays of C typedefs in generated code.
	indicators []i64
	// length/null indicators for bound parameters. Kept on the handle because ODBC
	// retains the pointers until SQLExecute(), which may happen after
	// bind_parameter returns.
	param_indicators []i64
}

// new_hstmt constructs a new statement handle
fn new_hstmt(hdbc C.SQLHDBC) !HStmt {
	mut retcode := C.SQLRETURN(C.SQL_SUCCESS)
	mut hstmt := C.SQLHSTMT(C.SQL_NULL_HSTMT)
	// Allocate statement handle
	retcode = C.SQLAllocHandle(C.SQLSMALLINT(C.SQL_HANDLE_STMT), C.SQLHANDLE(hdbc),
		unsafe { &C.SQLHANDLE(&hstmt) })
	check_error(retcode, 'SQLAllocHandle(SQL_HANDLE_STMT)', C.SQLHANDLE(hstmt),
		C.SQLSMALLINT(C.SQL_HANDLE_STMT))!

	return HStmt{
		hdbc:  hdbc
		hstmt: hstmt
	}
}

// close the statement handle
fn (mut h HStmt) close() {
	// Deallocate handle
	if h.hstmt != C.SQLHSTMT(C.SQL_NULL_HSTMT) {
		C.SQLFreeStmt(h.hstmt, C.SQLUSMALLINT(C.SQL_RESET_PARAMS))
		// check error code?
		C.SQLFreeHandle(C.SQLSMALLINT(C.SQL_HANDLE_STMT), C.SQLHANDLE(h.hstmt))
		h.hstmt = C.SQLHSTMT(C.SQL_NULL_HSTMT)
	}
}

// exec executes a Sql statement. Result is stored in odbc driver, and not yet read.
fn (h HStmt) exec(sql_ string) ! {
	retcode := C.SQLExecDirect(h.hstmt, sql_.str, C.SQLINTEGER(C.SQL_NTS))
	check_error(retcode, 'SQLExecDirect()', C.SQLHANDLE(h.hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))!
}

// prepare compiles a statement that has parameter markers.
fn (h HStmt) prepare(sql_ string) ! {
	retcode := C.SQLPrepare(h.hstmt, sql_.str, C.SQLINTEGER(C.SQL_NTS))
	check_error(retcode, 'SQLPrepare()', C.SQLHANDLE(h.hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))!
}

// bind_parameter binds one 1-based parameter marker. A none value is bound as SQL NULL.
fn (mut h HStmt) bind_parameter(index int, value ?string) ! {
	if h.param_indicators.len < index {
		h.param_indicators << []i64{len: index - h.param_indicators.len}
	}
	mut value_ptr := C.SQLPOINTER(0)
	mut column_size := C.SQLULEN(0)
	mut buffer_length := C.SQLLEN(0)
	if v := value {
		value_ptr = C.SQLPOINTER(v.str)
		column_size = C.SQLULEN(v.len + 1)
		buffer_length = C.SQLLEN(v.len + 1)
		h.param_indicators[index - 1] = i64(v.len)
	} else {
		// SQL_NULL_DATA tells the driver to ignore value_ptr and store NULL.
		h.param_indicators[index - 1] = i64(C.SQL_NULL_DATA)
	}
	retcode := C.SQLBindParameter(h.hstmt, C.SQLUSMALLINT(index),
		C.SQLSMALLINT(C.SQL_PARAM_INPUT), C.SQLSMALLINT(C.SQL_C_CHAR),
		C.SQLSMALLINT(C.SQL_VARCHAR), column_size, C.SQLSMALLINT(0), value_ptr, buffer_length,
		voidptr(&h.param_indicators[index - 1]))
	check_error(retcode, 'SQLBindParameter()', C.SQLHANDLE(h.hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))!
}

// execute runs a previously prepared statement with its bound parameters.
fn (h HStmt) execute() ! {
	retcode := C.SQLExecute(h.hstmt)
	check_error(retcode, 'SQLExecute()', C.SQLHANDLE(h.hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))!
}

// retrieve_affected_rows returns number of rows affected/modified by the last operation. -1 if not applicable.
fn (h HStmt) retrieve_affected_rows() !int {
	count_ret := C.SQLLEN(0)
	// SQLLEN is pointer-wide; bypass the adapter for 32-bit C int out-parameters.
	retcode := C.SQLRowCount(h.hstmt, voidptr(&count_ret))
	check_error(retcode, 'SQLRowCount()', C.SQLHANDLE(h.hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))!
	return int(count_ret)
}

fn (h HStmt) retrieve_column_count() !int {
	mut retcode := C.SQLRETURN(C.SQL_SUCCESS)
	col_count_buff := C.SQLSMALLINT(0)
	retcode = C.SQLNumResultCols(h.hstmt, &col_count_buff)
	check_error(retcode, 'SQLNumResultCols()', C.SQLHANDLE(h.hstmt),
		C.SQLSMALLINT(C.SQL_HANDLE_STMT))!
	return int(col_count_buff)
}

fn (h HStmt) read_column_names() ![]string {
	if h.column_count <= 0 {
		return []string{}
	}
	mut names := []string{cap: h.column_count}
	for i := 0; i < h.column_count; i++ {
		mut name_buf := [256]char{}
		mut name_len := C.SQLSMALLINT(0)
		mut data_type := C.SQLSMALLINT(0)
		mut column_size := C.SQLULEN(0)
		mut decimal_digits := C.SQLSMALLINT(0)
		mut nullable := C.SQLSMALLINT(0)
		retcode := C.SQLDescribeCol(h.hstmt, C.SQLUSMALLINT(i + 1), &C.SQLCHAR(&name_buf[0]),
			C.SQLSMALLINT(name_buf.len), &name_len, &data_type, &column_size, &decimal_digits,
			&nullable)
		check_error(retcode, 'SQLDescribeCol()', C.SQLHANDLE(h.hstmt),
			C.SQLSMALLINT(C.SQL_HANDLE_STMT))!
		names << unsafe { (&name_buf[0]).vstring() }
	}
	return names
}

// allocate buffers and bind them to drivers
fn (mut h HStmt) prepare_read() ! {
	mut retcode := C.SQLRETURN(C.SQL_SUCCESS)

	column_count := h.retrieve_column_count()!
	h.column_count = column_count // remember the count because read will need it

	h.buffers = [][]char{len: h.column_count}
	h.indicators = []i64{len: h.column_count}

	for i := 0; i < h.column_count; i++ {
		i_col := C.SQLUSMALLINT(i + 1) // col number starts with 1
		size_ret := C.SQLLEN(0)
		// find out buffer size needed to read data in this column
		retcode = C.SQLColAttribute(h.hstmt, i_col, C.SQLUSMALLINT(C.SQL_DESC_LENGTH),
			C.SQLPOINTER(0), C.SQLSMALLINT(0), C.SQLSMALLINT(0), voidptr(&size_ret))
		check_error(retcode, 'SQLColAttribute()', C.SQLHANDLE(h.hstmt),
			C.SQLSMALLINT(C.SQL_HANDLE_STMT))!

		// buffer allocation is the size + 1 to include termination char, since SQL_DESC_LENGTH does not include it.
		allocate_size := size_ret + C.SQLLEN(1)
		allocate_size_int := int(allocate_size)
		buff := []char{len: allocate_size_int}

		// bind the buffer
		retcode = C.SQLBindCol(h.hstmt, C.SQLUSMALLINT(i_col), C.SQLSMALLINT(C.SQL_C_CHAR),
			C.SQLPOINTER(&buff[0]), allocate_size, unsafe { &C.SQLLEN(&h.indicators[i]) })
		check_error(retcode, 'SQLBindCol()', C.SQLHANDLE(h.hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))!

		// record the buffer in HStmt
		h.buffers[i] = buff
	}
}

// fetch all rows
fn (h HStmt) read_rows() ![]Row {
	mut retcode := C.SQLRETURN(C.SQL_SUCCESS)

	mut res := []Row{}

	if h.column_count <= 0 {
		// there is nothing in the driver to read from
		return res
	}

	// Fetch and print each row of data until SQL_NO_DATA returned.
	for {
		retcode = C.SQLFetch(h.hstmt)
		if retcode == C.SQLRETURN(C.SQL_SUCCESS) || retcode == C.SQLRETURN(C.SQL_SUCCESS_WITH_INFO) {
			// copy buffered result to res
			mut row := []string{}
			mut nulls := []bool{len: h.buffers.len}
			mut has_null := false
			for i, content in h.buffers {
				row << unsafe { cstring_to_vstring(content.data) }
				if h.indicators[i] == i64(C.SQL_NULL_DATA) {
					nulls[i] = true
					has_null = true
				}
			}
			mut r := Row{ vals: row }
			if has_null {
				r.nulls = nulls
			}
			res << r
		} else {
			if retcode != C.SQLRETURN(C.SQL_NO_DATA) {
				check_error(retcode, 'SQLFetch()', C.SQLHANDLE(h.hstmt),
					C.SQLSMALLINT(C.SQL_HANDLE_STMT))!
			} else {
				break
			}
		}
	}
	return res
}
