module oracle

// OCI constants copied from oci.h / ocidfn.h. V declares them here so the
// compiler never reads the Oracle headers.
const oci_success = 0
const oci_success_with_info = 1
const oci_no_data = 100
const oci_default = 0
const oci_threaded = 1
const oci_native_syntax = 1
const oci_fetch_next = 2
const oci_trans_new = 1
const oci_cred_rdbms = 1
const htype_env = 1
const htype_error = 2
const htype_svcctx = 3
const htype_stmt = 4
const htype_server = 8
const htype_session = 9
const dtype_param = 53
const attr_server = 6
const attr_session = 7
const attr_row_count = 9
const attr_param_count = 18
const attr_username = 22
const attr_password = 23
const attr_stmt_type = 24
const attr_name = 4
const attr_data_type = 2
const attr_data_size = 1
const stmt_select = 1
const sqlt_str = 5

// Param is a single bind value for `Connection.exec`.
pub struct Param {
pub:
	value   string
	is_null bool
}

// Row holds the string values of one fetched row.
pub struct Row {
pub mut:
	vals  []string
	nulls []bool
}

// Result is the outcome of `Connection.exec`.
pub struct Result {
pub mut:
	names             []string
	rows              []Row
	num_rows_affected int
}

// Connection owns the OCI handles for one Oracle session.
pub struct Connection {
mut:
	env           voidptr = voidptr(0)
	err           voidptr = voidptr(0)
	srv           voidptr = voidptr(0)
	svc           voidptr = voidptr(0)
	usr           voidptr = voidptr(0)
	is_open       bool
	last_sequence string
pub mut:
	dsn string
}

struct Column {
mut:
	buf    []u8
	define voidptr = voidptr(0)
	ind    i16
	rlen   u16
	rcode  u16
}

// is_connected reports whether this connection currently owns live OCI handles.
pub fn (conn &Connection) is_connected() bool {
	return conn.is_open
}

// connect establishes an Oracle session using the full OCI handle chain.
pub fn (mut conn Connection) connect(cfg Config) ! {
	if conn.is_open {
		return error('oracle: connection is already open')
	}
	dsn := cfg.dsn_string()
	if dsn == '' {
		return error('oracle: config needs a dsn, or host and service/sid')
	}
	user := preferred(cfg.user, cfg.username)
	pass := preferred(cfg.password, cfg.pass)
	if user == '' {
		return error('oracle: config needs a user')
	}

	mut rc := C.OCIEnvCreate(&conn.env, u32(oci_threaded), 0, 0, 0, 0, 0, 0)
	conn.check(rc, 'OCIEnvCreate')!
	conn.is_open = true

	rc = C.OCIHandleAlloc(conn.env, voidptr(&conn.err), u32(htype_error), 0, 0)
	conn.check(rc, 'OCIHandleAlloc(error)')!

	rc = C.OCIHandleAlloc(conn.env, voidptr(&conn.srv), u32(htype_server), 0, 0)
	conn.check(rc, 'OCIHandleAlloc(server)')!

	rc = C.OCIServerAttach(conn.srv, conn.err, voidptr(dsn.str), dsn.len, u32(oci_default))
	conn.check(rc, 'OCIServerAttach')!

	rc = C.OCIHandleAlloc(conn.env, voidptr(&conn.svc), u32(htype_svcctx), 0, 0)
	conn.check(rc, 'OCIHandleAlloc(svcctx)')!

	rc = C.OCIAttrSet(voidptr(&conn.svc), u32(htype_svcctx), voidptr(&conn.srv), 0, u32(attr_server),
		conn.err)
	conn.check(rc, 'OCIAttrSet(server)')!

	rc = C.OCIHandleAlloc(conn.env, voidptr(&conn.usr), u32(htype_session), 0, 0)
	conn.check(rc, 'OCIHandleAlloc(session)')!

	rc = C.OCIAttrSet(voidptr(&conn.usr), u32(htype_session), voidptr(user.str), u32(user.len),
		u32(attr_username), conn.err)
	conn.check(rc, 'OCIAttrSet(username)')!

	rc = C.OCIAttrSet(voidptr(&conn.usr), u32(htype_session), voidptr(pass.str), u32(pass.len),
		u32(attr_password), conn.err)
	conn.check(rc, 'OCIAttrSet(password)')!

	rc = C.OCISessionBegin(conn.svc, conn.err, conn.usr, u32(oci_cred_rdbms), u32(oci_default))
	conn.check(rc, 'OCISessionBegin')!

	rc = C.OCIAttrSet(voidptr(&conn.svc), u32(htype_svcctx), voidptr(&conn.usr), 0, u32(attr_session),
		conn.err)
	conn.check(rc, 'OCIAttrSet(session)')!

	conn.dsn = dsn
}

// close releases every OCI handle owned by this connection. It is safe to call
// more than once.
pub fn (mut conn Connection) close() {
	if !conn.is_open {
		return
	}
	if conn.usr != voidptr(0) {
		C.OCISessionEnd(conn.svc, conn.err, conn.usr, u32(oci_default))
		C.OCIHandleFree(conn.usr, u32(htype_session))
		conn.usr = voidptr(0)
	}
	if conn.srv != voidptr(0) {
		C.OCIServerDetach(conn.srv, conn.err, u32(oci_default))
		C.OCIHandleFree(conn.srv, u32(htype_server))
		conn.srv = voidptr(0)
	}
	if conn.svc != voidptr(0) {
		C.OCIHandleFree(conn.svc, u32(htype_svcctx))
		conn.svc = voidptr(0)
	}
	if conn.err != voidptr(0) {
		C.OCIHandleFree(conn.err, u32(htype_error))
		conn.err = voidptr(0)
	}
	if conn.env != voidptr(0) {
		C.OCIHandleFree(conn.env, u32(htype_env))
		conn.env = voidptr(0)
	}
	conn.is_open = false
}

// exec prepares and executes `query`, binds `params` by position (`:1`, `:2`,
// ...) and returns any result rows. All values are exchanged as strings and
// converted by Oracle, which keeps the binding path simple and predictable.
pub fn (mut conn Connection) exec(query string, params []Param) !Result {
	if !conn.is_open {
		return error('oracle: connection is not open')
	}

	mut stmt := voidptr(0)
	mut rc := C.OCIHandleAlloc(conn.env, voidptr(&stmt), u32(htype_stmt), 0, 0)
	conn.check(rc, 'OCIHandleAlloc(stmt)')!
	defer {
		C.OCIHandleFree(stmt, u32(htype_stmt))
	}

	rc = C.OCIStmtPrepare(stmt, conn.err, voidptr(query.str), u32(query.len), u32(oci_native_syntax),
		u32(oci_default))
	conn.check(rc, 'OCIStmtPrepare')!

	mut stmt_type := u32(0)
	mut out_size := u32(0)
	rc = C.OCIAttrGet(stmt, u32(htype_stmt), voidptr(&stmt_type), &out_size, u32(attr_stmt_type), conn.err)
	conn.check(rc, 'OCIAttrGet(stmt_type)')!
	iters := if int(stmt_type) == stmt_select { 0 } else { 1 }

	// The bind buffers and indicators have to stay alive until Execute returns.
	mut bind_values := []string{len: params.len, init: ''}
	mut inds := []i16{len: params.len, init: 0}
	mut binds := []voidptr{len: params.len, init: voidptr(0)}
	mut alens := []u16{len: params.len, init: 0}
	mut rcodes := []u16{len: params.len, init: 0}
	mut curels := []u32{len: params.len, init: 0}
	for i, p in params {
		if p.is_null {
			inds[i] = -1
		} else {
			bind_values[i] = p.value
		}
		val := bind_values[i]
		// OCI keeps these pointers until OCIStmtExecute returns, so the arrays
		// intentionally live for the whole function.
		unsafe {
			rc = C.OCIBindByPos(stmt, &binds[i], conn.err, u32(i + 1), voidptr(val.str),
				i32(val.len + 1), u16(sqlt_str), voidptr(&inds[i]), voidptr(&alens[i]),
				voidptr(&rcodes[i]), 0, voidptr(&curels[i]), u32(oci_default))
		}
		conn.check(rc, 'OCIBindByPos')!
	}

	rc = C.OCIStmtExecute(conn.svc, stmt, conn.err, u32(iters), 0, voidptr(0), voidptr(0), u32(oci_default))
	conn.check(rc, 'OCIStmtExecute')!

	mut affected := u32(0)
	rc = C.OCIAttrGet(stmt, u32(htype_stmt), voidptr(&affected), &out_size, u32(attr_row_count), conn.err)
	conn.check(rc, 'OCIAttrGet(row_count)')!

	mut res := Result{
		num_rows_affected: int(affected)
	}
	if int(stmt_type) != stmt_select {
		return res
	}

	mut ncols := u32(0)
	rc = C.OCIAttrGet(stmt, u32(htype_stmt), voidptr(&ncols), &out_size, u32(attr_param_count), conn.err)
	conn.check(rc, 'OCIAttrGet(param_count)')!

	// Pre-sized so the addresses handed to OCIDefineByPos stay stable.
	mut cols := []Column{len: int(ncols)}
	for i in 0 .. int(ncols) {
		mut param := voidptr(0)
		rc = C.OCIParamGet(stmt, u32(htype_stmt), conn.err, voidptr(&param), u32(i + 1))
		conn.check(rc, 'OCIParamGet')!

		mut name_buf := [256]u8{}
		mut name_len := u32(0)
		rc = C.OCIAttrGet(param, u32(dtype_param), voidptr(&name_buf[0]), &name_len, u32(attr_name),
			conn.err)
		conn.check(rc, 'OCIAttrGet(name)')!

		mut data_size := u32(0)
		rc = C.OCIAttrGet(param, u32(dtype_param), voidptr(&data_size), &out_size, u32(attr_data_size),
			conn.err)
		conn.check(rc, 'OCIAttrGet(data_size)')!

		mut sz := int(data_size) + 1
		if sz < 4096 {
			sz = 4096
		}
		cols[i].buf = []u8{len: sz}
		unsafe {
			rc = C.OCIDefineByPos(stmt, &cols[i].define, conn.err, u32(i + 1),
				voidptr(cols[i].buf.data), i32(cols[i].buf.len), u16(sqlt_str),
				voidptr(&cols[i].ind), voidptr(&cols[i].rlen), voidptr(&cols[i].rcode), u32(oci_default))
		}
		conn.check(rc, 'OCIDefineByPos')!

		res.names << unsafe { (&name_buf[0]).vstring_with_len(int(name_len)) }.clone()
	}

	for {
		rc = C.OCIStmtFetch2(stmt, conn.err, 1, u16(oci_fetch_next), 0, u32(oci_default))
		if rc == oci_no_data {
			break
		}
		conn.check(rc, 'OCIStmtFetch2')!
		mut row := Row{}
		for col in cols {
			if col.ind == -1 {
				row.vals << ''
				row.nulls << true
				continue
			}
			mut n := int(col.rlen)
			if n > col.buf.len {
				n = col.buf.len
			}
			// SQLT_STR is NUL terminated; trim the terminator and any padding.
			for k in 0 .. n {
				if col.buf[k] == 0 {
					n = k
					break
				}
			}
			val := if n == 0 {
				''
			} else {
				unsafe { (&col.buf[0]).vstring_with_len(n) }.clone()
			}
			row.vals << val
			row.nulls << false
		}
		res.rows << row
	}
	return res
}

// begin starts an explicit transaction with OCITransStart.
pub fn (mut conn Connection) begin() ! {
	if !conn.is_open {
		return error('oracle: connection is not open')
	}
	rc := C.OCITransStart(conn.svc, conn.err, 0, u32(oci_trans_new))
	conn.check(rc, 'OCITransStart')!
}

// commit commits the current transaction.
pub fn (mut conn Connection) commit() ! {
	if !conn.is_open {
		return error('oracle: connection is not open')
	}
	rc := C.OCITransCommit(conn.svc, conn.err, u32(oci_default))
	conn.check(rc, 'OCITransCommit')!
}

// rollback rolls back the current transaction.
pub fn (mut conn Connection) rollback() ! {
	if !conn.is_open {
		return error('oracle: connection is not open')
	}
	rc := C.OCITransRollback(conn.svc, conn.err, u32(oci_default))
	conn.check(rc, 'OCITransRollback')!
}

fn valid_savepoint(name string) bool {
	if name.len == 0 || name.len > 30 {
		return false
	}
	for c in name {
		ok := (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || (c >= 48 && c <= 57) || c == 95
		if !ok {
			return false
		}
	}
	return true
}

// savepoint creates a named savepoint in the current transaction.
pub fn (mut conn Connection) savepoint(name string) ! {
	if !valid_savepoint(name) {
		return error('oracle: invalid savepoint name')
	}
	conn.exec('SAVEPOINT ${name}', [])!
}

// rollback_to rolls the current transaction back to a named savepoint.
pub fn (mut conn Connection) rollback_to(name string) ! {
	if !valid_savepoint(name) {
		return error('oracle: invalid savepoint name')
	}
	conn.exec('ROLLBACK TO SAVEPOINT ${name}', [])!
}

// release_savepoint releases a named savepoint.
pub fn (mut conn Connection) release_savepoint(name string) ! {
	if !valid_savepoint(name) {
		return error('oracle: invalid savepoint name')
	}
	conn.exec('RELEASE SAVEPOINT ${name}', [])!
}

// last_id returns CURRVAL of the sequence created for the most recent
// `create()` call, or 0 when the table had no serial field.
pub fn (mut conn Connection) last_id() int {
	if conn.last_sequence == '' {
		return 0
	}
	res := conn.exec('SELECT "${conn.last_sequence}".CURRVAL FROM DUAL', []) or { return 0 }
	if res.rows.len == 0 || res.rows[0].vals.len == 0 {
		return 0
	}
	return res.rows[0].vals[0].int()
}

fn (conn &Connection) check(rc int, what string) ! {
	if rc == oci_success || rc == oci_success_with_info {
		return
	}
	return error('${what}: ${conn.error_message()}')
}

// error_message formats the first diagnostic record from the OCI error handle.
pub fn (conn &Connection) error_message() string {
	if conn.err == voidptr(0) {
		return 'unknown OCI error (no error handle)'
	}
	mut code := u32(0)
	mut state := u8(0)
	mut msg := [512]u8{}
	rc := C.OCIErrorGet(conn.err, 1, voidptr(&state), &code, voidptr(&msg[0]), u32(msg.len),
		u32(htype_error))
	if rc != oci_success && rc != oci_success_with_info {
		return 'unknown OCI error (OCIErrorGet failed)'
	}
	text := unsafe { (&msg[0]).vstring() }
	return 'ORA-${code}: ${text}'
}
