// vtest build: started_redis?
import db.redis
import os
import rand

fn data_structure_connection(version int) !redis.DB {
	mut db := redis.connect(password: os.getenv('VREDIS_PASSWORD'))!
	db.cmd('HELLO', version.str())!
	db.version = version
	return db
}

fn sorted_members(values []string) []string {
	mut result := values.clone()
	result.sort()
	return result
}

fn test_list_commands() {
	for version in [2, 3] {
		mut db := data_structure_connection(version)!
		prefix := 'v:redis:lists:${rand.ulid()}'
		keys := ['${prefix}:a', '${prefix}:b', '${prefix}:missing']
		defer {
			for key in keys { db.del(key) or {} }
			db.close() or {}
		}
		binary := [u8(0), 13, 10, 255].bytestr()
		assert db.lpushx(keys[0], 'a')! == 0
		assert db.rpushx(keys[0], 'a')! == 0
		assert db.rpush(keys[0], 'a', 'b', binary)! == 3
		assert db.lpush(keys[0], 'start')! == 4
		assert db.lpushx(keys[0], 'first')! == 5
		assert db.rpushx(keys[0], 'last')! == 6
		assert db.llen(keys[0])! == 6
		assert db.lrange(keys[0], 0, -1)! == ['first', 'start', 'a', 'b', binary, 'last']
		assert db.lindex(keys[0], -2)! == binary
		assert db.lpos(keys[0], binary)! == 4
		assert db.lset(keys[0], 1, 'new')! == 'OK'
		assert db.linsert(keys[0], .before, 'a', 'pivot')! == 7
		assert db.linsert(keys[0], .after, 'a', 'pivot')! == 8
		assert db.lrem(keys[0], 0, 'pivot')! == 2
		assert db.ltrim(keys[0], 1, -2)! == 'OK'
		assert db.lpop(keys[0])! == 'new'
		assert db.rpop(keys[0])! == binary
		assert db.lpop_count(keys[0], 1)! == ['a']
		assert db.rpop_count(keys[0], 2)! == ['b']
		assert db.lpop_count(keys[0], 2)!.len == 0
		assert db.rpop_count(keys[0], 2)!.len == 0
		if value := db.lpop(keys[0]) {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		if value := db.rpop(keys[0]) {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		if value := db.lindex(keys[0], 0) {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		if value := db.lpos(keys[0], 'absent') {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}

		assert db.rpush(keys[0], 'a', binary, 'b')! == 3
		assert db.lmove(keys[0], keys[1], .left, .right)! == 'a'
		assert db.blmove(keys[0], keys[1], .right, .left, 0.1)! == 'b'
		assert db.rpoplpush(keys[0], keys[1])! == binary
		assert db.lrange(keys[1], 0, -1)! == [binary, 'b', 'a']
		left := db.blpop([keys[0], keys[1]], 0.1)!
		assert left.key == keys[1]
		assert left.values == [binary]
		right := db.brpop([keys[0], keys[1]], 0.1)!
		assert right.key == keys[1]
		assert right.values == ['a']
		multi := db.lmpop([keys[0], keys[1]], .left, 2)!
		assert multi.key == keys[1]
		assert multi.values == ['b']
		assert db.rpush(keys[1], 'c', 'd', 'e')! == 3
		blocking := db.blmpop(0.1, [keys[0], keys[1]], .right, 2)!
		assert blocking.key == keys[1]
		assert blocking.values == ['e', 'd']
		assert db.lpop(keys[1])! == 'c'
		if value := db.blpop([keys[2]], 0.01) {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		if value := db.brpop([keys[2]], 0.01) {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		if value := db.lmpop([keys[2]], .left, 1) {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		if value := db.blmpop(0.01, [keys[2]], .left, 1) {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
	}
}

fn test_set_commands() {
	for version in [2, 3] {
		mut db := data_structure_connection(version)!
		prefix := 'v:redis:sets:${rand.ulid()}'
		keys := ['${prefix}:a', '${prefix}:b', '${prefix}:out', '${prefix}:missing']
		defer {
			for key in keys { db.del(key) or {} }
			db.close() or {}
		}
		binary := [u8(0), 13, 10, 255].bytestr()
		assert db.sadd(keys[0], 'a', 'b', binary)! == 3
		assert db.sadd(keys[0], 'a')! == 0
		assert sorted_members(db.smembers(keys[0])!) == sorted_members(['a', 'b', binary])
		assert db.scard(keys[0])! == 3
		assert db.sismember(keys[0], binary)!
		assert !db.sismember(keys[0], 'absent')!
		assert db.smismember(keys[0], binary, 'absent', 'a')! == [true, false, true]
		assert db.smove(keys[0], keys[1], binary)!
		assert !db.smove(keys[0], keys[1], binary)!
		assert db.sadd(keys[1], 'b', 'c')! == 2
		assert db.sinter(keys[0], keys[1])! == ['b']
		assert db.sintercard([keys[0], keys[1]], 0)! == 1
		assert db.sinterstore(keys[2], keys[0], keys[1])! == 1
		assert sorted_members(db.sunion(keys[0], keys[1])!) == sorted_members(['a', 'b', 'c', binary])
		assert db.sunionstore(keys[2], keys[0], keys[1])! == 4
		assert db.sdiff(keys[0], keys[1])! == ['a']
		assert db.sdiffstore(keys[2], keys[0], keys[1])! == 1
		assert db.srandmember(keys[2])! == 'a'
		assert db.srandmember_count(keys[2], -3)! == ['a', 'a', 'a']
		assert db.spop(keys[2])! == 'a'
		assert db.sadd(keys[2], 'a', 'b')! == 2
		assert sorted_members(db.spop_count(keys[2], 3)!) == ['a', 'b']
		assert db.spop_count(keys[2], 1)!.len == 0
		assert db.srandmember_count(keys[2], 2)!.len == 0
		if value := db.spop(keys[2]) {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		if value := db.srandmember(keys[2]) {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		assert db.srem(keys[1], 'c', binary)! == 2
		assert db.smembers(keys[3])!.len == 0
		mut cursor := '0'
		mut scanned := []string{}
		for {
			next, values := db.sscan(keys[0], cursor, count: 1)!
			scanned << values
			cursor = next
			if cursor == '0' { break }
		}
		assert sorted_members(scanned) == ['a', 'b']
	}
}

fn test_sorted_set_commands() {
	for version in [2, 3] {
		mut db := data_structure_connection(version)!
		prefix := 'v:redis:zsets:${rand.ulid()}'
		keys := ['${prefix}:a', '${prefix}:lex', '${prefix}:missing']
		defer {
			for key in keys { db.del(key) or {} }
			db.close() or {}
		}
		binary := [u8(0), 13, 10, 255].bytestr()
		members := [redis.ZMember{ member: 'a', score: 1.25 },
			redis.ZMember{ member: binary, score: 2.5 }, redis.ZMember{ member: 'c', score: 4 }]
		assert db.zadd(keys[0], ...members)! == 3
		assert db.zadd_with_options(keys[0], redis.ZAddOptions{ nx: true }, redis.ZMember{ member: 'a', score: 99 })! == 0
		assert db.zcard(keys[0])! == 3
		assert db.zcount(keys[0], '(1.25', '+inf')! == 2
		assert db.zscore(keys[0], binary)! == 2.5
		scores := db.zmscore(keys[0], 'a', 'missing', binary)!
		assert scores.len == 3
		first_score := scores[0] or { panic('missing first score') }
		assert first_score == 1.25
		if missing := scores[1] {
			assert false, '${missing}'
		}
		last_score := scores[2] or { panic('missing last score') }
		assert last_score == 2.5
		assert db.zrank(keys[0], binary)! == 1
		assert db.zrevrank(keys[0], binary)! == 1
		assert db.zrange(keys[0], 0, -1)! == ['a', binary, 'c']
		assert db.zrange_withscores(keys[0], 0, -1)! == members
		assert db.zrange(keys[0], 0, 1, rev: true)! == ['c', binary]
		assert db.zrevrange(keys[0], 0, -1)! == ['c', binary, 'a']
		assert db.zrevrange_withscores(keys[0], 0, 0)! == [members[2]]
		assert db.zrangebyscore(keys[0], '-inf', '+inf', offset: 1, count: 1)! == [binary]
		assert db.zrangebyscore_withscores(keys[0], '(1.25', '2.5')! == [members[1]]
		assert db.zincrby(keys[0], 0.25, 'a')! == 1.5
		assert db.zrem(keys[0], 'c')! == 1
		assert db.zpopmax(keys[0])! == [members[1]]
		assert db.zpopmin(keys[0], 2)! == [redis.ZMember{ member: 'a', score: 1.5 }]
		assert db.zpopmin(keys[0])!.len == 0
		assert db.zpopmax(keys[0])!.len == 0
		if value := db.zscore(keys[0], 'absent') {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		if value := db.zrank(keys[0], 'absent') {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		if value := db.zrevrank(keys[0], 'absent') {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		assert db.zadd(keys[0], ...members)! == 3
		max := db.bzpopmax(0.1, keys[2], keys[0])!
		assert max.key == keys[0]
		assert max.value == members[2]
		min := db.bzpopmin(0.1, keys[2], keys[0])!
		assert min.value == members[0]
		assert db.zrandmember(keys[0])! == binary
		assert db.zrandmember_count(keys[0], -2)! == [binary, binary]
		assert db.zrandmember_withscores(keys[0], 1)! == [members[1]]
		next, scanned := db.zscan(keys[0], '0', match: binary, count: 10)!
		assert next == '0'
		assert scanned == [members[1]]
		assert db.zremrangebyscore(keys[0], '-inf', '+inf')! == 1
		if value := db.bzpopmax(0.01, keys[2]) {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		if value := db.bzpopmin(0.01, keys[2]) {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		if value := db.zrandmember(keys[2]) {
			assert false, 'expected NilError, received ${value}'
		} else {
			assert err is redis.NilError
		}
		assert db.zadd(keys[1], redis.ZMember{ member: 'a', score: 0 }, redis.ZMember{ member: 'b', score: 0 }, redis.ZMember{ member: 'c', score: 0 })! == 3
		assert db.zrangebylex(keys[1], '[a', '(c')! == ['a', 'b']
		assert db.zlexcount(keys[1], '-', '+')! == 3
		assert db.zremrangebylex(keys[1], '[a', '[b')! == 2
		assert db.zremrangebyrank(keys[1], 0, -1)! == 1
	}
}

fn test_sorted_set_combinations_and_pipeline() {
	for version in [2, 3] {
		mut db := data_structure_connection(version)!
		prefix := 'v:redis:zcombine:${rand.ulid()}'
		keys := ['${prefix}:a', '${prefix}:b', '${prefix}:out', '${prefix}:list', '${prefix}:set']
		defer {
			for key in keys { db.del(key) or {} }
			db.close() or {}
		}
		assert db.zadd(keys[0], redis.ZMember{ member: 'a', score: 1 }, redis.ZMember{ member: 'b', score: 2 })! == 2
		assert db.zadd(keys[1], redis.ZMember{ member: 'b', score: 3 }, redis.ZMember{ member: 'c', score: 4 })! == 2
		weighted := [redis.WeightedKey{ key: keys[0], weight: 2 },
			redis.WeightedKey{ key: keys[1], weight: 3 }]
		assert db.zinter(weighted)! == ['b']
		assert db.zinter_withscores(weighted)! == [redis.ZMember{ member: 'b', score: 13 }]
		assert db.zinterstore(keys[2], weighted, aggregate: .max)! == 1
		assert db.zscore(keys[2], 'b')! == 9
		assert db.zintercard([keys[0], keys[1]], 0)! == 1
		assert db.zunion(weighted)! == ['a', 'c', 'b']
		assert db.zunion_withscores(weighted, aggregate: .min)! == [
			redis.ZMember{ member: 'a', score: 2 },
			redis.ZMember{ member: 'b', score: 4 },
			redis.ZMember{ member: 'c', score: 12 },
		]
		assert db.zunionstore(keys[2], weighted)! == 3
		assert db.zscore(keys[2], 'b')! == 13
		assert db.zdiff(keys[0], keys[1])! == ['a']
		assert db.zdiff_withscores(keys[0], keys[1])! == [redis.ZMember{ member: 'a', score: 1 }]
		assert db.zdiffstore(keys[2], keys[0], keys[1])! == 1
		assert db.zscore(keys[2], 'a')! == 1

		db.pipeline_start()
		assert db.lpush(keys[3], 'a')! == 0
		assert db.lrange(keys[3], 0, -1)!.len == 0
		assert db.sadd(keys[4], 'a')! == 0
		assert db.smembers(keys[4])!.len == 0
		assert db.zrange_withscores(keys[2], 0, -1)!.len == 0
		results := db.pipeline_execute()!
		assert results.len == 5
		assert results[0] as i64 == 1
		assert results[2] as i64 == 1
		assert db.lrange(keys[3], 0, -1)! == ['a']
		assert db.smembers(keys[4])! == ['a']

		assert db.multi()! == 'OK'
		assert db.lpop(keys[3])! == ''
		assert db.spop(keys[4])! == ''
		assert db.zscore(keys[2], 'a')! == 0.0
		assert db.zrange_withscores(keys[2], 0, -1)!.len == 0
		transaction_results := db.exec()!
		assert transaction_results.len == 4
		assert (transaction_results[0] as []u8).bytestr() == 'a'
		assert (transaction_results[1] as []u8).bytestr() == 'a'
		assert db.llen(keys[3])! == 0
		assert db.scard(keys[4])! == 0
	}
}
