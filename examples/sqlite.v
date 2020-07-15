#flag -lsqlite3
#include "sqlite3.h"

// See also the example from https://www.sqlite.org/quickstart.html
struct C.sqlite3{}
struct C.sqlite3_stmt{}

type FnSqlite3Callback fn(voidptr, int, &charptr, &charptr) int

fn C.sqlite3_open(charptr, &&C.sqlite3) int
fn C.sqlite3_close(&C.sqlite3) int

fn C.sqlite3_column_int(stmt &C.sqlite3_stmt, n int) int
// you can also leave out the parameter names and `C.` prefix for C types
fn C.sqlite3_prepare_v2(&sqlite3, charptr, int, &&sqlite3_stmt, &charptr) int
fn C.sqlite3_step(&sqlite3_stmt)
fn C.sqlite3_finalize(&sqlite3_stmt)
fn C.sqlite3_exec(db &sqlite3, sql charptr, FnSqlite3Callback, cb_arg voidptr, emsg &charptr) int
fn C.sqlite3_free(voidptr)

fn my_callback(arg voidptr, howmany int, cvalues &charptr, cnames &charptr) int {
    for i in 0..howmany {
        print('| ${cstring_to_vstring(cnames[i])}: ${cstring_to_vstring(cvalues[i]):20} ')
    }
    println('|')
    return 0
}

fn main() {
    path := 'users.db'
    db := &C.sqlite3(0) // a temporary hack meaning `sqlite3* db = 0`
    C.sqlite3_open(path.str, &db)
    query := 'select count(*) from users'
    stmt := &C.sqlite3_stmt(0)
    C.sqlite3_prepare_v2(db, query.str, -1, &stmt, 0)
    C.sqlite3_step(stmt)
    nr_users := C.sqlite3_column_int(stmt, 0)
    C.sqlite3_finalize(stmt)
    println('There are $nr_users users in the database.')
    // now print all users
    error_msg := charptr(0)
    query_all_users := 'select * from users'
    rc := C.sqlite3_exec(db, query_all_users.str, my_callback, 7, &error_msg)
    if rc != C.SQLITE_OK {
        eprintln( cstring_to_vstring(error_msg) )
        C.sqlite3_free(error_msg)
    }
    C.sqlite3_close(db)
}
