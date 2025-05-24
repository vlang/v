module main

struct DBFoo {}

struct DBFoo2 {}

struct ConnectionPoolGeneric[T] {
mut:
	connections chan T
}

fn (mut f DBFoo) close() {}

fn (mut f DBFoo2) close() {}

pub fn new_conn_pool[T](size int) !&ConnectionPoolGeneric[T] {
	$if T is DBFoo2 {
		mut pool := &ConnectionPoolGeneric[DBFoo2]{
			connections: chan DBFoo2{cap: size}
		}
		return pool
	} $else {
		return error('')
	}
}

pub fn (mut pool ConnectionPoolGeneric[T]) acquire() !T {
	conn := <-pool.connections or { return error('Failed mysql') }
	return conn
}

pub fn (mut pool ConnectionPoolGeneric[T]) release(conn T) {
	pool.connections <- conn
}

pub fn (mut pool ConnectionPoolGeneric[T]) close() {
	for _ in 0 .. pool.connections.len {
		mut conn := <-pool.connections or { break }
		conn.close()
	}
}

fn test_main() {
	mut pool := new_conn_pool[DBFoo](5) or {
		assert true
		return
	}
	pool.close()
}
