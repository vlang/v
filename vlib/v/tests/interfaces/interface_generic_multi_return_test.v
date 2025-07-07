import pool

pub struct DB {}

fn (mut d DB) validate() !bool {
	return false
}

fn (mut d DB) close() ! {}

fn (mut d DB) reset() ! {}

pub interface DatabaseMysqlPool[T] {
mut:
	acquire() !(T, &pool.ConnectionPoolable)
	release(conn &pool.ConnectionPoolable) !
}

@[heap]
struct DatabasePoolImpl[T] {
mut:
	inner &pool.ConnectionPool
}

pub fn new_mysql_pool[T]() !&DatabaseMysqlPool[T] {
	create_conn := fn () !&pool.ConnectionPoolable {
		return DB{}
	}
	pool_conf := pool.ConnectionPoolConfig{}
	inner_pool := pool.new_connection_pool(create_conn, pool_conf)!
	pool_instance := &DatabasePoolImpl[T]{
		inner: inner_pool
	}
	return pool_instance
}

pub fn (mut p DatabasePoolImpl[T]) acquire() !(T, &pool.ConnectionPoolable) {
	conn := p.inner.get()!
	return conn as DB, conn
}

pub fn (mut p DatabasePoolImpl[T]) release(conn &pool.ConnectionPoolable) ! {
	p.inner.put(conn)!
}

fn test_main() {
	new_mysql_pool[DB]() or { assert err.str().contains('failed') }
}
