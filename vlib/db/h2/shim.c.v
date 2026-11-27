module h2

/*
 * All C/JNI details stay in this .c.v file and in h2_shim.c/h2_shim.h.
 * The V compiler never parses jni.h; it only emits the include below into
 * the generated C source.
 */

#flag -I $env('JAVA_HOME')/include
#flag windows -I $env('JAVA_HOME')/include/win32
#flag -I @VEXEROOT/vlib/db/h2
#flag @VEXEROOT/vlib/db/h2/h2_shim.c
#include "h2_shim.h"

struct C.H2Result {
	nrows     int
	ncols     int
	col_names &&char
	vals      &&char
}

fn C.h2_last_error() &char

fn C.h2_open(url &char, jar_path &char) voidptr

fn C.h2_close(conn voidptr)

fn C.h2_exec(conn voidptr, query &char, params voidptr, nulls voidptr, nparams int) &C.H2Result

fn C.h2_result_free(res &C.H2Result)

fn C.h2_last_id(conn voidptr) i64

fn c_last_error() string {
	return unsafe { cstring_to_vstring(C.h2_last_error()).clone() }
}

fn c_open(url string, jar_path string) voidptr {
	return C.h2_open(url.str, jar_path.str)
}

fn c_close(conn voidptr) {
	C.h2_close(conn)
}

// Result holds the rows returned by a raw query, including per-value NULL flags.
pub struct Result {
pub mut:
	names []string   = []string{}
	vals  [][]string = [][]string{}
	nulls [][]bool   = [][]bool{}
}

fn c_exec(conn voidptr, query string, params []string, null_flags []bool) !Result {
	mut param_ptrs := []&char{len: params.len, init: unsafe { nil }}
	mut null_ints := []i64{len: params.len, init: 0}
	for i, p in params {
		is_null := i < null_flags.len && null_flags[i]
		if !is_null {
			param_ptrs[i] = p.str
		}
		null_ints[i] = if is_null { 1 } else { 0 }
	}
	mut res := C.h2_exec(conn, query.str, param_ptrs.data, null_ints.data, params.len)
	if isnil(res) {
		return error('h2: ${c_last_error()}')
	}
	defer {
		C.h2_result_free(res)
	}
	mut out := Result{}
	for c := 0; c < res.ncols; c++ {
		out.names << unsafe { cstring_to_vstring(res.col_names[c]).clone() }
	}
	for r := 0; r < res.nrows; r++ {
		mut vals := []string{len: res.ncols, init: ''}
		mut nulls_row := []bool{len: res.ncols, init: false}
		for c := 0; c < res.ncols; c++ {
			ptr := unsafe { res.vals[r * res.ncols + c] }
			if isnil(ptr) {
				nulls_row[c] = true
			} else {
				vals[c] = unsafe { cstring_to_vstring(ptr).clone() }
			}
		}
		out.vals << vals
		out.nulls << nulls_row
	}
	return out
}

fn c_last_id_of(conn voidptr) i64 {
	return C.h2_last_id(conn)
}
