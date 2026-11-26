@[has_globals]
module pg

import dl
import os
import sync

// These types mirror the stable public ABI in PostgreSQL's libpq-fe.h.
// Keep the signatures and enum values in sync with that header; development headers are not
// available at build time, so V cannot import them here.
type FnPQconnectdb = fn (conninfo &char) voidptr
type FnPQstatus = fn (conn voidptr) i32
type FnPQtransactionStatus = fn (conn voidptr) i32
type FnPQerrorMessage = fn (conn voidptr) &char
type FnPQexec = fn (conn voidptr, query &char) voidptr
type FnPQgetisnull = fn (res voidptr, row i32, column i32) i32
type FnPQgetvalue = fn (res voidptr, row i32, column i32) &char
type FnPQresultStatus = fn (res voidptr) i32
type FnPQntuples = fn (res voidptr) i32
type FnPQnfields = fn (res voidptr) i32
type FnPQfname = fn (res voidptr, column i32) &char
type FnPQftype = fn (res voidptr, column i32) u32
type FnPQfmod = fn (res voidptr, column i32) i32
type FnPQfsize = fn (res voidptr, column i32) i32
type FnPQfformat = fn (res voidptr, column i32) i32
type FnPQftable = fn (res voidptr, column i32) u32
type FnPQftablecol = fn (res voidptr, column i32) i32
type FnPQexecParams = fn (conn voidptr, command &char, n_params i32, param_types voidptr, param_values voidptr, param_lengths voidptr, param_formats voidptr, result_format i32) voidptr
type FnPQputCopyData = fn (conn voidptr, buffer &char, nbytes i32) i32
type FnPQputCopyEnd = fn (conn voidptr, errmsg &char) i32
type FnPQgetCopyData = fn (conn voidptr, buffer voidptr, async i32) i32
type FnPQprepare = fn (conn voidptr, stmt_name &char, query &char, n_params i32, param_types voidptr) voidptr
type FnPQexecPrepared = fn (conn voidptr, stmt_name &char, n_params i32, param_values voidptr, param_lengths voidptr, param_formats voidptr, result_format i32) voidptr
type FnPQclear = fn (res voidptr)
type FnPQfreemem = fn (ptr voidptr)
type FnPQfinish = fn (conn voidptr)
type FnPQnotifies = fn (conn voidptr) voidptr
type FnPQconsumeInput = fn (conn voidptr) i32
type FnPQsocket = fn (conn voidptr) i32
type FnPQescapeLiteral = fn (conn voidptr, str &char, length uint) &char

struct PgRuntime {
mut:
	handle             voidptr
	connectdb          FnPQconnectdb         = unsafe { nil }
	status             FnPQstatus            = unsafe { nil }
	transaction_status FnPQtransactionStatus = unsafe { nil }
	error_message      FnPQerrorMessage      = unsafe { nil }
	exec               FnPQexec              = unsafe { nil }
	getisnull          FnPQgetisnull         = unsafe { nil }
	getvalue           FnPQgetvalue          = unsafe { nil }
	result_status      FnPQresultStatus      = unsafe { nil }
	ntuples            FnPQntuples           = unsafe { nil }
	nfields            FnPQnfields           = unsafe { nil }
	fname              FnPQfname             = unsafe { nil }
	ftype              FnPQftype             = unsafe { nil }
	fmod               FnPQfmod              = unsafe { nil }
	fsize              FnPQfsize             = unsafe { nil }
	fformat            FnPQfformat           = unsafe { nil }
	ftable             FnPQftable            = unsafe { nil }
	ftablecol          FnPQftablecol         = unsafe { nil }
	exec_params        FnPQexecParams        = unsafe { nil }
	put_copy_data      FnPQputCopyData       = unsafe { nil }
	put_copy_end       FnPQputCopyEnd        = unsafe { nil }
	get_copy_data      FnPQgetCopyData       = unsafe { nil }
	prepare            FnPQprepare           = unsafe { nil }
	exec_prepared      FnPQexecPrepared      = unsafe { nil }
	clear              FnPQclear             = unsafe { nil }
	freemem            FnPQfreemem           = unsafe { nil }
	finish             FnPQfinish            = unsafe { nil }
	notifies           FnPQnotifies          = unsafe { nil }
	consume_input      FnPQconsumeInput      = unsafe { nil }
	socket             FnPQsocket            = unsafe { nil }
	escape_literal     FnPQescapeLiteral     = unsafe { nil }
}

__global (
	pg_runtime       PgRuntime
	pg_runtime_once  &sync.Once
	pg_runtime_error string
)

fn init() {
	pg_runtime_once = sync.new_once()
}

fn pg_library_paths() []string {
	mut paths := []string{}
	if path := os.getenv_opt('V_PQ_LIB') {
		if path != '' {
			paths << path
		}
	}
	$if windows {
		paths << ['libpq.dll', 'pq.dll']
	} $else $if macos {
		paths << ['libpq.5.dylib', 'libpq.dylib', '/opt/homebrew/opt/libpq/lib/libpq.5.dylib',
			'/opt/homebrew/lib/libpq.5.dylib', '/usr/local/opt/libpq/lib/libpq.5.dylib',
			'/usr/local/lib/libpq.5.dylib', '/opt/local/lib/libpq.5.dylib',
			'/Applications/Postgres.app/Contents/Versions/latest/lib/libpq.5.dylib']
	} $else {
		paths << ['libpq.so.5', 'libpq.so', '/usr/local/lib/libpq.so.5', '/usr/local/lib/libpq.so']
	}
	return paths
}

fn open_libpq() !voidptr {
	mut errors := []string{}
	for path in pg_library_paths() {
		if handle := dl.open_opt(path, dl.rtld_now) {
			return handle
		} else {
			errors << '${path}: ${err}'
		}
	}
	if errors.len == 0 {
		return error('pg: no libpq runtime library paths configured')
	}
	return error('pg: could not load the libpq runtime library:\n${errors.join('\n')}')
}

fn load_pg_runtime() !PgRuntime {
	handle := open_libpq()!
	return PgRuntime{
		handle:             handle
		connectdb:          FnPQconnectdb(dl.sym_opt(handle, 'PQconnectdb')!)
		status:             FnPQstatus(dl.sym_opt(handle, 'PQstatus')!)
		transaction_status: FnPQtransactionStatus(dl.sym_opt(handle,
			'PQtransactionStatus')!)
		error_message:      FnPQerrorMessage(dl.sym_opt(handle, 'PQerrorMessage')!)
		exec:               FnPQexec(dl.sym_opt(handle, 'PQexec')!)
		getisnull:          FnPQgetisnull(dl.sym_opt(handle, 'PQgetisnull')!)
		getvalue:           FnPQgetvalue(dl.sym_opt(handle, 'PQgetvalue')!)
		result_status:      FnPQresultStatus(dl.sym_opt(handle, 'PQresultStatus')!)
		ntuples:            FnPQntuples(dl.sym_opt(handle, 'PQntuples')!)
		nfields:            FnPQnfields(dl.sym_opt(handle, 'PQnfields')!)
		fname:              FnPQfname(dl.sym_opt(handle, 'PQfname')!)
		ftype:              FnPQftype(dl.sym_opt(handle, 'PQftype')!)
		fmod:               FnPQfmod(dl.sym_opt(handle, 'PQfmod')!)
		fsize:              FnPQfsize(dl.sym_opt(handle, 'PQfsize')!)
		fformat:            FnPQfformat(dl.sym_opt(handle, 'PQfformat')!)
		ftable:             FnPQftable(dl.sym_opt(handle, 'PQftable')!)
		ftablecol:          FnPQftablecol(dl.sym_opt(handle, 'PQftablecol')!)
		exec_params:        FnPQexecParams(dl.sym_opt(handle, 'PQexecParams')!)
		put_copy_data:      FnPQputCopyData(dl.sym_opt(handle, 'PQputCopyData')!)
		put_copy_end:       FnPQputCopyEnd(dl.sym_opt(handle, 'PQputCopyEnd')!)
		get_copy_data:      FnPQgetCopyData(dl.sym_opt(handle, 'PQgetCopyData')!)
		prepare:            FnPQprepare(dl.sym_opt(handle, 'PQprepare')!)
		exec_prepared:      FnPQexecPrepared(dl.sym_opt(handle, 'PQexecPrepared')!)
		clear:              FnPQclear(dl.sym_opt(handle, 'PQclear')!)
		freemem:            FnPQfreemem(dl.sym_opt(handle, 'PQfreemem')!)
		finish:             FnPQfinish(dl.sym_opt(handle, 'PQfinish')!)
		notifies:           FnPQnotifies(dl.sym_opt(handle, 'PQnotifies')!)
		consume_input:      FnPQconsumeInput(dl.sym_opt(handle, 'PQconsumeInput')!)
		socket:             FnPQsocket(dl.sym_opt(handle, 'PQsocket')!)
		escape_literal:     FnPQescapeLiteral(dl.sym_opt(handle, 'PQescapeLiteral')!)
	}
}

fn load_pg_runtime_once() {
	pg_runtime = load_pg_runtime() or {
		pg_runtime_error = err.msg()
		return
	}
	pg_runtime_error = ''
}

// ensure_pg_runtime loads the libpq function table once, on first use.
// The library stays loaded because the table contains pointers into it.
// V_PQ_LIB can point at an exact library path when the system loader cannot find libpq.
fn ensure_pg_runtime() ! {
	pg_runtime_once.do(load_pg_runtime_once)
	if pg_runtime_error != '' {
		return error(pg_runtime_error)
	}
}
