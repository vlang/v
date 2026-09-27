// vtest build: !windows // uses pool.ConnectionPool, which can hang on windows
import pool

interface DbConnection {
	name() string
}

interface Poolable {
mut:
	acquire() !(DbConnection, &pool.ConnectionPoolable)
	close()
}

struct MysqlLikeDB {}

fn (db MysqlLikeDB) name() string {
	return 'mysql'
}

fn (mut db MysqlLikeDB) validate() !bool {
	return true
}

fn (mut db MysqlLikeDB) close() ! {}

fn (mut db MysqlLikeDB) reset() ! {}

struct PgInnerPool {}

struct PgLikeDB {
mut:
	pool &PgInnerPool = unsafe { nil }
}

fn (db PgLikeDB) name() string {
	return 'pg'
}

fn (mut db PgLikeDB) validate() !bool {
	return true
}

fn (mut db PgLikeDB) close() ! {}

fn (mut db PgLikeDB) reset() ! {}

@[heap]
struct GenericPool[T] implements Poolable {
mut:
	inner &pool.ConnectionPool
}

fn new_mysql_like_pool() !&GenericPool[MysqlLikeDB] {
	create_conn := fn () !&pool.ConnectionPoolable {
		mut db := MysqlLikeDB{}
		return &db
	}
	inner := pool.new_connection_pool(create_conn, pool.ConnectionPoolConfig{
		max_conns:      1
		min_idle_conns: 0
	})!
	return &GenericPool[MysqlLikeDB]{
		inner: inner
	}
}

fn new_pg_like_pool() !&GenericPool[PgLikeDB] {
	create_conn := fn () !&pool.ConnectionPoolable {
		mut db := PgLikeDB{}
		return &db
	}
	inner := pool.new_connection_pool(create_conn, pool.ConnectionPoolConfig{
		max_conns:      1
		min_idle_conns: 0
	})!
	return &GenericPool[PgLikeDB]{
		inner: inner
	}
}

fn (mut p GenericPool[T]) acquire() !(DbConnection, &pool.ConnectionPoolable) {
	conn := p.inner.get()!
	return conn as T, conn
}

fn (mut p GenericPool[T]) close() {
	p.inner.close()
}

fn test_generic_pool_casts_each_specialization_to_matching_interface() {
	mut mysql_pool := new_mysql_like_pool()!
	mut mysql_db, mysql_conn := mysql_pool.acquire()!
	assert mysql_db.name() == 'mysql'
	mysql_pool.inner.put(mysql_conn)!
	mysql_pool.close()

	mut pg_pool := new_pg_like_pool()!
	mut pg_db, pg_conn := pg_pool.acquire()!
	assert pg_db.name() == 'pg'
	pg_pool.inner.put(pg_conn)!
	pg_pool.close()
}
