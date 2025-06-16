// vtest build: present_redis?
import db.redis
import time

fn test_redis() {
	mut db := redis.connect() or { panic(err) }
	assert db.ping()! == 'PONG'

	// delete all keys first
	db.cmd('FLUSHALL')!

	// test set[T]
	assert db.set('int', 123)! == 'OK'
	assert db.set('string', 'abc')! == 'OK'
	assert db.set('bin', [u8(0x00), 0x01, 0x02, 0x03])! == 'OK'

	// test get[T]
	assert db.get[i64]('int')! == 123
	assert db.get[string]('string')! == 'abc'
	assert db.get[[]u8]('bin')! == [u8(0x00), 0x01, 0x02, 0x03]

	// test incr/decr
	assert db.incr('int')! == 124
	assert db.decr('int')! == 123

	// test hset/hget/hgetall
	m := {
		'a': '1'
		'b': '2'
		'c': '3'
	}
	assert db.hset('map', m)! == m.len
	assert db.hget[string]('map', 'a')! == '1'
	assert db.hget[string]('map', 'b')! == '2'
	assert db.hget[string]('map', 'c')! == '3'
	assert db.hgetall[string]('map')! == m

	// test expire
	assert db.expire('int', 1)!
	time.sleep(2 * time.second)
	_ := db.get[i64]('int') or {
		assert err.msg().contains('key int not found')
		0
	}

	// test del
	assert db.del('string')! == 1
	_ := db.get[string]('string') or {
		assert err.msg().contains('key string not found')
		''
	}

	// test custom cmd
	assert db.cmd('SET', 'bigint', '123456')! as string == 'OK'
	assert db.cmd('GET', 'bigint')! as []u8 == [u8(49), 50, 51, 52, 53, 54]
	db.close()!
}

fn test_redis_pipeline() {
	mut db := redis.connect() or { panic(err) }
	assert db.ping()! == 'PONG'

	// start pipleline mode
	db.pipeline_start()
	// delete all keys first
	db.cmd('FLUSHALL')!
	a := db.pipeline_execute()!
	assert a == [redis.RedisValue('OK')]

	// restart pipeline again
	db.pipeline_start()
	// test set[T]
	db.set('int', 123)!
	db.set('string', 'abc')!
	db.set('bin', [u8(0x00), 0x01, 0x02, 0x03])!

	// test get[T]
	db.get[i64]('int')!
	db.get[string]('string')!
	db.get[[]u8]('bin')!

	// test incr/decr
	db.incr('int')!
	db.incr('int')!
	db.decr('int')!

	// test hset/hget/hgetall
	m := {
		'a': '1'
		'b': '2'
		'c': '3'
	}
	db.hset('map', m)!
	db.hget[string]('map', 'a')!
	db.hget[string]('map', 'b')!
	db.hget[string]('map', 'c')!
	db.hgetall[string]('map')!

	// test custom cmd
	db.cmd('SET', 'bigint', '123456')!
	db.cmd('GET', 'bigint')!

	b := db.pipeline_execute()!
	assert b == [
		redis.RedisValue('OK'),
		redis.RedisValue('OK'),
		redis.RedisValue('OK'),
		redis.RedisValue([u8(49), 50, 51]),
		redis.RedisValue([u8(97), 98, 99]),
		redis.RedisValue([u8(0), 1, 2, 3]),
		redis.RedisValue(i64(124)),
		redis.RedisValue(i64(125)),
		redis.RedisValue(i64(124)),
		redis.RedisValue(i64(3)),
		redis.RedisValue([u8(49)]),
		redis.RedisValue([u8(50)]),
		redis.RedisValue([u8(51)]),
		redis.RedisValue([redis.RedisValue([u8(97)]), redis.RedisValue([u8(49)]),
			redis.RedisValue([u8(98)]), redis.RedisValue([u8(50)]), redis.RedisValue([u8(99)]),
			redis.RedisValue([u8(51)])]),
		redis.RedisValue('OK'),
		redis.RedisValue([u8(49), 50, 51, 52, 53, 54]),
	]

	db.close()!
}
