module mssql


// HStmt is handle for sql statement
pub struct HStmt {
mut:
	// db connection reference. Owner is Connection struct.
	hdbc C.SQLHDBC   = C.SQLHDBC(C.SQL_NULL_HDBC)
	// statement handle
	hstmt C.SQLHSTMT = C.SQLHSTMT(C.SQL_NULL_HSTMT)
	
	// fields used for computation
	column_count int = -1
	// columns
	buffers [][]char
}

// new_hstmt constructs a new statement handle
pub fn new_hstmt(hdbc C.SQLHDBC) ?HStmt {
	mut retcode := C.SQLRETURN(C.SQL_SUCCESS)
	mut hstmt := C.SQLHSTMT(C.SQL_NULL_HSTMT)
	// Allocate statement handle
	retcode = C.SQLAllocHandle(C.SQLSMALLINT(C.SQL_HANDLE_STMT), C.SQLHANDLE(hdbc), unsafe{&C.SQLHANDLE(&hstmt)})
	check_error(retcode, "SQLAllocHandle(SQL_HANDLE_STMT)",
		C.SQLHANDLE(hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))?

	return HStmt{
		hdbc: hdbc,
		hstmt: hstmt,
	}
}

// close the statement handle
pub fn (mut h HStmt) close(){
	// Deallocate handle
	if h.hstmt != C.SQLHSTMT(C.SQL_NULL_HSTMT){
		// check error code?
		C.SQLFreeHandle(C.SQLSMALLINT(C.SQL_HANDLE_STMT), C.SQLHANDLE(h.hstmt))
		h.hstmt = C.SQLHSTMT(C.SQL_NULL_HSTMT)
	}
}

// exec executes a Sql statement. Result is stored in odbc driver, and not yet read.
pub fn (mut h HStmt) exec(sql string) ?{
	retcode := C.SQLExecDirect(h.hstmt,
	 	sql.str, C.SQLINTEGER(C.SQL_NTS))
	check_error(retcode, "SQLExecDirect()", C.SQLHANDLE(h.hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))?
}

pub fn (mut h HStmt) retrieve_affected_rows() ?int{
	count_ret := C.SQLLEN(0)
	retcode := C.SQLRowCount(h.hstmt,&count_ret)
	check_error(retcode, "SQLRowCount()", C.SQLHANDLE(h.hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))?
	return int(count_ret)
}

pub fn (mut h HStmt) retrieve_column_count() ?int{
	mut retcode := C.SQLRETURN(C.SQL_SUCCESS)
	col_count_buff := C.SQLSMALLINT(0)
	retcode = C.SQLNumResultCols(h.hstmt,&col_count_buff)
	check_error(retcode, "SQLNumResultCols()", C.SQLHANDLE(h.hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))?
	h.column_count = col_count_buff
	return int(col_count_buff)
}

// allocate buffers and bind them to drivers
pub fn (mut h HStmt) prepare_read() ?{
	mut retcode := C.SQLRETURN(C.SQL_SUCCESS)
	
	h.buffers = [][]char{len: h.column_count, cap: h.column_count}

	for i := 0; i < h.column_count; i++{
		i_col :=  C.SQLUSMALLINT(i + 1) // col number starts with 1
		size_ret := C.SQLLEN(0)
		// find out buffer size needed from column type
		retcode = C.SQLColAttribute(h.hstmt,
                    i_col,
                    C.SQLUSMALLINT(C.SQL_DESC_DISPLAY_SIZE), // TODO: find out the right flag SQL_DESC_COLUMN_LENGTH???
                    C.SQLPOINTER(0),
                    C.SQLSMALLINT(0),
                    C.SQLSMALLINT(0),
                    &size_ret)
		check_error(retcode, "SQLColAttribute()", C.SQLHANDLE(h.hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))?

		// buffer allocation is the size + 1 to include termination char. Is this needed?
		allocate_size := size_ret + C.SQLLEN(1)
		allocate_size_int := int(allocate_size)
		buff := []char{len: allocate_size_int, cap: allocate_size_int}
		
		// bind the buffer
		retcode = C.SQLBindCol(h.hstmt, C.SQLUSMALLINT(i_col), C.SQLSMALLINT(C.SQL_C_CHAR), C.SQLPOINTER(&buff[0]), allocate_size, 0)
		check_error(retcode, "SQLBindCol()", C.SQLHANDLE(h.hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))?

		// record the buffer in HStmt
		h.buffers[i] = buff
	}
}

// fetch all rows 
pub fn (mut h HStmt) read_rows() ?[][]string{
	mut retcode := C.SQLRETURN(C.SQL_SUCCESS)

	mut res := [][]string{}

	if h.column_count <= 0{
		// there is nothing in the driver to read from
		return res
	}

	// Fetch and print each row of data until SQL_NO_DATA returned.
	for i := 0; ; i++ {
		mut row := []string{}
		retcode = C.SQLFetch(h.hstmt)
		if retcode == C.SQLRETURN(C.SQL_SUCCESS) || retcode == C.SQLRETURN(C.SQL_SUCCESS_WITH_INFO) {
			// copy buffered result to res
			for content in h.buffers{
				row << string(content)
			}
		}
		else {
			if retcode != C.SQLRETURN(C.SQL_NO_DATA) {
				check_error(retcode, "SQLFetch()", C.SQLHANDLE(h.hstmt), C.SQLSMALLINT(C.SQL_HANDLE_STMT))?
			}
			else {
				break
			}
		}
		res << row
	}
	return res
}