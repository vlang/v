import db.redis
import pool

fn create_conn() !&pool.ConnectionPoolable {
	mut redis_client := redis.DB{}
	conn := &redis_client
	return conn
}

fn test_redis_db_implements_connection_poolable() {
	mut conn := create_conn()!
	conn.reset()!
}
