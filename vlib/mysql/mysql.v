module mysql

import orm

#flag linux -I/usr/include/mysql

#flag -lmysqlclient
#include <mysql.h>

struct DB {
	conn &C.MYSQL
}

struct Result {
pub:
	result &C.MYSQL_RES
}

// C

struct C.MYSQL { }
struct C.MYSQL_RES { }
struct C.MYSQL_BIND {
mut:
	buffer_type int
	buffer voidptr
	buffer_length u64
	length_value u64
}
struct C.MYSQL_STMT { }

fn C.mysql_init(mysql &C.MYSQL) &C.MYSQL
fn C.mysql_real_connect(mysql &C.MYSQL, host byteptr, user byteptr, passwd byteptr, db byteptr, port u32, unix_socket byteptr, clientflag u64) &C.MYSQL
fn C.mysql_query(mysql &C.MYSQL, q byteptr) int
fn C.mysql_error(mysql &C.MYSQL) byteptr
fn C.mysql_num_fields(res &C.MYSQL_RES) int
fn C.mysql_store_result(mysql &C.MYSQL) &C.MYSQL_RES
fn C.mysql_fetch_row(res &C.MYSQL_RES) &byteptr
fn C.mysql_free_result(res &C.MYSQL_RES)
fn C.mysql_commit(mysql &C.MYSQL) byte
fn C.mysql_rollback(mysql &C.MYSQL) byte
fn C.mysql_close(sock &C.MYSQL)

fn C.mysql_stmt_init(mysql &C.MYSQL) &C.MYSQL_STMT
fn C.mysql_stmt_prepare(stmt &C.MYSQL_STMT, q byteptr, qsize u64) int
fn C.mysql_stmt_param_count(stmt &C.MYSQL_STMT) u64
fn C.mysql_stmt_bind_param(stmt &C.MYSQL_STMT, binds &C.MYSQL_BIND) byte
fn C.mysql_stmt_error(stmt &C.MYSQL_STMT) byteptr
fn C.mysql_stmt_execute(stmt &C.MYSQL_STMT) u64
fn C.mysql_stmt_affected_rows(stmt &C.MYSQL_STMT) u64
fn C.mysql_stmt_num_rows(stmt &C.MYSQL_STMT) u64
fn C.mysql_stmt_insert_id(stmt &C.MYSQL_STMT) u64
fn C.mysql_stmt_field_count(stmt &C.MYSQL_STMT) u32
fn C.mysql_stmt_close(stmt &C.MYSQL_STMT)
// V


pub fn connect(server, user, passwd, dbname string) DB {
	conn := C.mysql_init(0)
	if isnil(conn) {
		eprintln('mysql_init failed')
		exit(1)
	}
	conn2 := C.mysql_real_connect(conn, server.str, user.str, passwd.str, dbname.str, 0, 0, 0)
	if isnil(conn2) {
		eprintln('mysql_real_connect failed')
		exit(1)
	}
	return DB {conn: conn2}
}

pub fn (db DB) query(q string) Result {
	ret := C.mysql_query(db.conn, q.str)
	if ret != 0 {
		C.fprintf(stderr, '%s\n', mysql_error(db.conn))
		exit(1)
	}
	res := C.mysql_store_result(db.conn)
	return Result {result: res}
}

pub fn (db DB) close() {
	//println('closing conection: $db.conn')
	C.mysql_close(db.conn)
}

pub fn (r Result) fetch_row() &byteptr {
	return C.mysql_fetch_row(r.result)
}

pub fn (r Result) num_fields() int {
	return C.mysql_num_fields(r.result)
}

pub fn (r Result) rows() []orm.Row {
	mut rows := []orm.Row
	nr_cols := r.num_fields()
	for rr := r.fetch_row(); rr; rr = r.fetch_row() {
		mut row := orm.Row{}
		for i := 0; i < nr_cols; i++ {
			if rr[i] == 0 {
				row.vals << ''
			} else {
				row.vals << string(rr[i])
			}
		}
		rows << row
	}
	return rows
}

pub fn (r Result) free() {
	//println('freeing result: $r.result')
	C.mysql_free_result(r.result)
}

//////////////////////////////////////////////////////////////////////////////////////////
//Needed for the ORM to work:
//////////////////////////////////////////////////////////////////////////////////////////

type MBind C.MYSQL_BIND 
//Execute a parametrized query with N parameters. Return array of the result rows:
pub fn (db &DB) pquery(cquery voidptr, nparams int, params voidptr) []orm.Row {
	sparams := &byteptr( params )	
	println( 'pquery cquery: ' + string(byteptr(cquery)) )
	println( 'pquery nparams: $nparams')
	println( 'pquery params: $params')
	println( 'sparams: $sparams')
	//println( 's1: ' + string(sparams[0]))
	//println( 's2: ' + string(sparams[1]))
	bindsize := nparams * sizeof( MBind )
	println( 'bindsize: $bindsize' )
	mut bind := &MBind( calloc( bindsize ) )
	defer { free( bind ) }
	println('bind: $bind')
	for i:=0; i < nparams ; i++ {
		plen := C.strlen( sparams[i] )
		bind[i].buffer_type= C.MYSQL_TYPE_STRING
		bind[i].buffer = sparams[i]
		bind[i].buffer_length = u64(plen)
		bind[i].length_value = u64(plen)
	}
	println('--------------------------------')

	stmt := C.mysql_stmt_init(db.conn)
	defer { C.mysql_stmt_close( stmt ) }
	println('stmt: $stmt')
	cquerylen := C.strlen( cquery )
    println('error is: ' + string( C.mysql_stmt_error( stmt ) ))
	C.mysql_stmt_prepare( stmt, cquery, cquerylen )
    println('error is: ' + string( C.mysql_stmt_error( stmt ) ))
	C.mysql_stmt_bind_param( stmt, bind )
    println('error is: ' + string( C.mysql_stmt_error( stmt ) ))
	C.mysql_stmt_execute( stmt )
    println('error is: ' + string( C.mysql_stmt_error( stmt ) ))
	C.mysql_stmt_store_result(stmt) // buffer the results to client memory
    println('error is: ' + string( C.mysql_stmt_error( stmt ) ))
	/*

	for ; !C.mysql_stmt_fetch(stmt); {
	}

*/
	
	return []orm.Row
}

//Execute a query that returns a single integer result, like for select count(*)
pub fn (db &DB) exec_to_row_to_int(query string) int {
	println('exec_to_row_to_int query: $query')
	mut res := 0
	r := db.query( query )
	nr_cols := r.num_fields()
	if nr_cols == 0 { goto exec_to_row_to_int_end }
	rr := r.fetch_row()
	if !rr { goto exec_to_row_to_int_end }
	if rr[0] == 0 { goto exec_to_row_to_int_end }
	
	res = string(rr[0]).int()

exec_to_row_to_int_end:
	r.free()
	println('exec_to_row_to_int res: $res')
	return res
}
