module h2

import os
import time

@[heap]
struct H2State {
mut:
	ptr voidptr = unsafe { nil }
}

// H2Connection is a JDBC connection to an H2 database driven through the JNI
// Invocation API. It implements orm.Connection and orm.TransactionalConnection.
pub struct H2Connection {
mut:
	state &H2State
}

// connect opens a connection to an H2 database.
//
// Use ':memory:' (or an empty string) for a private in-memory database. Any
// other path is opened as `jdbc:h2:<path>` and becomes a file-backed database.
// The JDBC jar must be provided through the H2_JAR environment variable, and
// JAVA_HOME must point at a JDK containing bin/server/jvm.dll plus the JNI
// headers under include/ and include/win32/.
pub fn connect(path string) !H2Connection {
	jar := os.getenv('H2_JAR')
	if jar == '' {
		return error('h2: H2_JAR environment variable must point to the h2 jar')
	}
	url := if path == '' || path == ':memory:' {
		'jdbc:h2:mem:v_${os.getpid()}_${time.now().unix_milli()};DB_CLOSE_DELAY=-1'
	} else {
		'jdbc:h2:${path}'
	}
	ptr := c_open(url, jar)
	if isnil(ptr) {
		return error('h2: ${c_last_error()}')
	}
	return H2Connection{ state: &H2State{ ptr: ptr } }
}

// close releases the underlying JDBC connection. Calling it more than once is
// harmless. The JVM itself remains alive until the process exits.
pub fn (mut db H2Connection) close() {
	if isnil(db.state.ptr) {
		return
	}
	c_close(db.state.ptr)
	db.state.ptr = unsafe { nil }
}

// is_closed reports whether close() has already been called.
pub fn (db H2Connection) is_closed() bool {
	return isnil(db.state.ptr)
}

// last_id returns the most recently generated identity value on this connection.
pub fn (db H2Connection) last_id() int {
	if isnil(db.state.ptr) {
		return 0
	}
	return int(c_last_id_of(db.state.ptr))
}

// exec runs a raw SQL query with `?` placeholders and returns the result rows.
pub fn (db H2Connection) exec(query string, params []string) !Result {
	return db.exec_nullable(query, params, []bool{len: params.len, init: false})
}

// exec_nullable is like exec, but lets individual parameters bind as SQL NULL.
pub fn (db H2Connection) exec_nullable(query string, params []string, nulls []bool) !Result {
	if isnil(db.state.ptr) {
		return error('h2: connection is closed')
	}
	return c_exec(db.state.ptr, query, params, nulls)
}
