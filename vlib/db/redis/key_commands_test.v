// vtest build: started_redis?
import db.redis
import os
import rand
import time

const key_commands_password = os.getenv('VREDIS_PASSWORD')
// MOVE and COPY targets live outside the databases cleared by other Redis test files.
const key_commands_other_database = 13

struct CachedUser {
	name  string
	age   int
	tags  []string
	admin bool
}

fn key_commands_db(version int) !redis.DB {
	mut db := redis.connect(password: key_commands_password)!
	db.hello(version)!
	return db
}

fn key_commands_prefix(version int) string {
	return 'v:keys:${rand.uuid_v4()}:${version}'
}

fn cleanup_key_commands(mut db redis.DB, keys []string) {
	db.unlink(...keys) or {}
	db.close() or {}
}

fn server_has_command(mut db redis.DB, name string) bool {
	info := db.command_info(name) or { return false }
	if info is []redis.RedisValue {
		return info.len == 1 && info[0] !is redis.RedisNull
	}
	return false
}

fn assert_sorted_value[T](value ?T, expected T) {
	actual := value or { panic('expected a sorted value') }
	assert actual == expected
}

fn assert_increx_rejected(mut db redis.DB, key string, options redis.IncrExOptions) {
	db.increx(key, 1, options) or {
		assert err is redis.CommandError
		return
	}
	assert false, 'increx must return an error'
}

fn assert_sorted_missing(value ?string) {
	if actual := value {
		panic('expected none, got ${actual}')
	}
}

fn test_getset_substr_and_lcs_commands() ! {
	for version in [3, 2] {
		mut db := key_commands_db(version)!
		prefix := key_commands_prefix(version)
		keys := ['${prefix}:first', '${prefix}:second', '${prefix}:binary', '${prefix}:missing',
			'${prefix}:list']
		defer { cleanup_key_commands(mut db, keys) }

		if previous := db.getset(keys[0], 'ohmytext') {
			assert false, 'getset must report a missing previous value, got ${previous}'
		} else {
			assert err is redis.NilError
		}
		assert db.get[string](keys[0])! == 'ohmytext'
		assert db.getset(keys[0], 'ohmytext')! == 'ohmytext'
		binary := [u8(0), `\r`, `\n`, 255]
		db.getset(keys[2], binary) or { assert err is redis.NilError }
		assert db.getset(keys[2], []u8{})! == binary
		assert db.get[[]u8](keys[2])! == []u8{}

		assert db.substr[string](keys[0], 2, 3)! == 'my'
		assert db.substr[[]u8](keys[0], -4, -1)! == 'text'.bytes()
		assert db.substr[string](keys[3], 0, -1)! == ''

		assert db.set(keys[1], 'mynewtext')! == 'OK'
		assert db.lcs(keys[0], keys[1])! == 'mytext'
		assert db.lcs_len(keys[0], keys[1])! == 6
		assert db.lcs(keys[0], keys[3])! == ''
		assert db.lcs_len(keys[0], keys[3])! == 0
		result := db.lcs_idx(keys[0], keys[1])!
		assert result.length == 6
		assert result.matches == [
			redis.LcsMatch{
				first_start:  4
				first_end:    7
				second_start: 5
				second_end:   8
				length:       4
			},
			redis.LcsMatch{
				first_start:  2
				first_end:    3
				second_start: 0
				second_end:   1
				length:       2
			},
		]
		long_only := db.lcs_idx(keys[0], keys[1], min_match_len: 4)!
		assert long_only.length == 6
		assert long_only.matches.len == 1
		assert long_only.matches[0].length == 4
		empty := db.lcs_idx(keys[0], keys[3])!
		assert empty.length == 0
		assert empty.matches.len == 0
		if _ := db.lcs_idx(keys[0], keys[1], min_match_len: -1) {
			assert false, 'lcs_idx must reject a negative minimum length'
		} else {
			assert err is redis.CommandError
		}
		assert db.rpush(keys[4], 'not a string')! == 1
		if _ := db.lcs(keys[0], keys[4]) {
			assert false, 'lcs must propagate Redis type errors'
		} else {
			assert err is redis.CommandError
		}
		assert db.ping()! == 'PONG'
	}
}

fn test_renamenx_copy_move_and_touch() ! {
	for version in [3, 2] {
		mut db := key_commands_db(version)!
		prefix := key_commands_prefix(version)
		keys := ['${prefix}:source', '${prefix}:renamed', '${prefix}:copy', '${prefix}:remote']
		defer { cleanup_key_commands(mut db, keys) }
		mut other := redis.connect(
			password: key_commands_password
			database: key_commands_other_database
		)!
		defer { cleanup_key_commands(mut other, keys) }

		assert db.set(keys[0], 'value')! == 'OK'
		assert db.touch(keys[0], keys[1], keys[0])! == 2
		assert db.renamenx(keys[0], keys[1])!
		assert db.exists(keys[0])! == 0
		assert db.set(keys[0], 'other')! == 'OK'
		assert !db.renamenx(keys[0], keys[1])!
		assert db.get[string](keys[1])! == 'value'

		assert db.copy(keys[1], keys[2])!
		assert !db.copy(keys[1], keys[0])!
		assert db.get[string](keys[0])! == 'other'
		assert db.copy(keys[1], keys[0], replace: true)!
		assert db.get[string](keys[0])! == 'value'
		assert !db.copy(keys[3], keys[2])!

		assert db.copy(keys[1], keys[3], database: key_commands_other_database)!
		assert other.get[string](keys[3])! == 'value'
		assert db.exists(keys[3])! == 0
		assert db.move(keys[2], key_commands_other_database)!
		assert db.exists(keys[2])! == 0
		assert other.get[string](keys[2])! == 'value'
		assert !db.move(keys[2], key_commands_other_database)!
		assert db.set(keys[2], 'local')! == 'OK'
		assert !db.move(keys[2], key_commands_other_database)!
		assert db.get[string](keys[2])! == 'local'

		if _ := db.move(keys[2], -1) {
			assert false, 'move must reject a negative database'
		} else {
			assert err is redis.CommandError
		}
		if _ := db.copy(keys[2], keys[3], database: -1) {
			assert false, 'copy must reject a negative database'
		} else {
			assert err is redis.CommandError
		}
		assert db.ping()! == 'PONG'
	}
}

fn test_dump_and_restore() ! {
	for version in [3, 2] {
		mut db := key_commands_db(version)!
		prefix := key_commands_prefix(version)
		keys := ['${prefix}:list', '${prefix}:restored', '${prefix}:binary', '${prefix}:binary_copy',
			'${prefix}:missing']
		defer { cleanup_key_commands(mut db, keys) }

		assert db.rpush(keys[0], 'a', 'b')! == 2
		payload := db.dump(keys[0])!
		assert payload.len > 0
		if _ := db.dump(keys[4]) {
			assert false, 'dump must report a missing key'
		} else {
			assert err is redis.NilError
		}
		assert db.restore(keys[1], 0, payload)! == 'OK'
		assert db.lrange(keys[1], 0, -1)! == ['a', 'b']
		assert db.ttl(keys[1])! == -1
		if _ := db.restore(keys[1], 0, payload) {
			assert false, 'restore must not replace an existing key by default'
		} else {
			assert err.msg().contains('BUSYKEY')
		}
		assert db.restore(keys[1], 120000, payload, replace: true, idletime: 10)! == 'OK'
		assert db.pttl(keys[1])! in 1 .. 120001
		assert db.restore(keys[1], time.now().unix_milli() + 120000, payload,
			replace: true
			absttl:  true
			freq:    5
		)! == 'OK'
		assert db.pttl(keys[1])! in 1 .. 120001

		binary := [u8(0), `\r`, `\n`, 255, `$`, `*`]
		assert db.set(keys[2], binary)! == 'OK'
		assert db.restore(keys[3], 0, db.dump(keys[2])!)! == 'OK'
		assert db.get[[]u8](keys[3])! == binary
		if _ := db.restore(keys[1], 0, binary, replace: true) {
			assert false, 'restore must propagate payload errors'
		} else {
			assert err is redis.CommandError
		}

		for options in [
			redis.RestoreOptions{
				idletime: 1
				freq:     1
			},
			redis.RestoreOptions{
				idletime: -1
			},
			redis.RestoreOptions{
				freq: 256
			},
		] {
			if _ := db.restore(keys[1], 0, payload, options) {
				assert false, 'restore must reject invalid options'
			} else {
				assert err is redis.CommandError
			}
		}
		if _ := db.restore(keys[1], -1, payload) {
			assert false, 'restore must reject a negative ttl'
		} else {
			assert err is redis.CommandError
		}
		assert db.ping()! == 'PONG'
	}
}

fn test_sort_commands() ! {
	for version in [3, 2] {
		mut db := key_commands_db(version)!
		prefix := key_commands_prefix(version)
		keys := ['${prefix}:numbers', '${prefix}:words', '${prefix}:stored', '${prefix}:weight:1',
			'${prefix}:weight:2', '${prefix}:weight:3', '${prefix}:object:1', '${prefix}:object:2']
		defer { cleanup_key_commands(mut db, keys) }

		assert db.rpush(keys[0], '3', '1', '2')! == 3
		numbers := db.sort[int](keys[0])!
		assert numbers.len == 3
		for index, expected in [1, 2, 3] {
			assert_sorted_value(numbers[index], expected)
		}
		page := db.sort[string](keys[0], desc: true, offset: 1, count: 2)!
		assert page.len == 2
		assert_sorted_value(page[0], '2')
		assert_sorted_value(page[1], '1')
		readonly := db.sort_ro[[]u8](keys[0], desc: true)!
		assert readonly.len == 3
		assert_sorted_value(readonly[0], '3'.bytes())

		assert db.rpush(keys[1], 'banana', 'apple', 'cherry')! == 3
		words := db.sort[string](keys[1], alpha: true)!
		assert words.len == 3
		for index, expected in ['apple', 'banana', 'cherry'] {
			assert_sorted_value(words[index], expected)
		}
		if _ := db.sort[string](keys[1]) {
			assert false, 'numeric sort must propagate Redis conversion errors'
		} else {
			assert err is redis.CommandError
		}

		assert db.mset({
			keys[3]: 30
			keys[4]: 10
			keys[5]: 20
		})! == 'OK'
		assert db.hset(keys[6], {
			'name': 'one'
		})! == 1
		assert db.hset(keys[7], {
			'name': 'two'
		})! == 1
		options := redis.SortOptions{
			by:  '${prefix}:weight:*'
			get: ['#', '${prefix}:object:*->name']
		}
		projected := db.sort[string](keys[0], options)!
		assert projected.len == 6
		assert_sorted_value(projected[0], '2')
		assert_sorted_value(projected[1], 'two')
		assert_sorted_value(projected[2], '3')
		assert_sorted_missing(projected[3])
		assert_sorted_value(projected[4], '1')
		assert_sorted_value(projected[5], 'one')
		readonly_projected := db.sort_ro[string](keys[0], options)!
		assert readonly_projected.len == 6
		assert_sorted_missing(readonly_projected[3])

		assert db.sort_store(keys[0], keys[2], desc: true)! == 3
		assert db.lrange(keys[2], 0, -1)! == ['3', '2', '1']
		if _ := db.sort[string](keys[0], offset: 1) {
			assert false, 'sort must reject an offset without a count'
		} else {
			assert err.msg().contains('offset requires count')
		}
		assert db.ping()! == 'PONG'
	}
}

fn test_hrandfield_commands() ! {
	for version in [3, 2] {
		mut db := key_commands_db(version)!
		prefix := key_commands_prefix(version)
		keys := ['${prefix}:hash', '${prefix}:missing']
		defer { cleanup_key_commands(mut db, keys) }
		fields := {
			'a': '1'
			'b': '2'
			'c': '3'
		}

		assert db.hset(keys[0], fields)! == 3
		assert db.hrandfield(keys[0])! in fields
		distinct := db.hrandfield_count(keys[0], 2)!
		assert distinct.len == 2
		assert distinct[0] != distinct[1]
		for field in distinct {
			assert field in fields
		}
		assert db.hrandfield_count(keys[0], 10)!.len == 3
		repeated := db.hrandfield_count(keys[0], -5)!
		assert repeated.len == 5
		for field in repeated {
			assert field in fields
		}
		mut pairs := db.hrandfield_withvalues(keys[0], 3)!
		pairs.sort(a.field < b.field)
		assert pairs == [
			redis.HashField{
				field: 'a'
				value: '1'
			},
			redis.HashField{
				field: 'b'
				value: '2'
			},
			redis.HashField{
				field: 'c'
				value: '3'
			},
		]
		repeated_pairs := db.hrandfield_withvalues(keys[0], -4)!
		assert repeated_pairs.len == 4
		for pair in repeated_pairs {
			assert fields[pair.field] == pair.value
		}

		if field := db.hrandfield(keys[1]) {
			assert false, 'hrandfield must report a missing hash, got ${field}'
		} else {
			assert err is redis.NilError
		}
		assert db.hrandfield_count(keys[1], 2)! == []string{}
		assert db.hrandfield_withvalues(keys[1], 2)! == []redis.HashField{}
		assert db.ping()! == 'PONG'
	}
}

fn test_delex_and_digest() ! {
	for version in [3, 2] {
		mut db := key_commands_db(version)!
		prefix := key_commands_prefix(version)
		keys := ['${prefix}:value', '${prefix}:list', '${prefix}:binary']
		defer { cleanup_key_commands(mut db, keys) }
		if !server_has_command(mut db, 'delex') || !server_has_command(mut db, 'digest') {
			eprintln('skipping delex/digest checks: Redis 8.4 or later is required')
			return
		}

		assert db.set(keys[0], 'Hello world')! == 'OK'
		digest := db.digest(keys[0])!
		assert digest == 'b6acb9d84a38ff74'
		assert !db.delex(keys[0], condition: .ifeq, value: 'other')!
		assert !db.delex(keys[0], condition: .ifne, value: 'Hello world')!
		assert !db.delex(keys[0], condition: .ifdne, value: digest)!
		assert db.exists(keys[0])! == 1
		assert db.delex(keys[0], condition: .ifdeq, value: digest)!
		assert db.exists(keys[0])! == 0
		assert db.set(keys[0], 'value')! == 'OK'
		assert db.delex(keys[0], condition: .ifeq, value: 'value')!
		assert db.set(keys[0], 'value')! == 'OK'
		assert db.delex(keys[0], condition: .ifne, value: 'other')!
		assert db.set(keys[0], 'value')! == 'OK'
		assert db.delex(keys[0])!
		assert !db.delex(keys[0])!
		if _ := db.digest(keys[0]) {
			assert false, 'digest must report a missing key'
		} else {
			assert err is redis.NilError
		}
		if _ := db.delex(keys[0], value: 'value') {
			assert false, 'delex must reject a value without a condition'
		} else {
			assert err is redis.CommandError
		}

		binary := [u8(0), `\r`, `\n`, 255]
		assert db.set(keys[2], binary)! == 'OK'
		assert db.delex(keys[2], condition: .ifeq, value: binary.bytestr())!

		assert db.rpush(keys[1], 'item')! == 1
		if _ := db.delex(keys[1], condition: .ifeq, value: 'item') {
			assert false, 'conditional delex must reject non-string keys'
		} else {
			assert err is redis.CommandError
		}
		if _ := db.digest(keys[1]) {
			assert false, 'digest must reject non-string keys'
		} else {
			assert err is redis.CommandError
		}
		assert db.delex(keys[1])!
		assert db.ping()! == 'PONG'
	}
}

fn test_increx() ! {
	for version in [3, 2] {
		mut db := key_commands_db(version)!
		prefix := key_commands_prefix(version)
		keys := ['${prefix}:counter', '${prefix}:bounded', '${prefix}:window', '${prefix}:float',
			'${prefix}:text']
		defer { cleanup_key_commands(mut db, keys) }
		if !server_has_command(mut db, 'increx') {
			eprintln('skipping increx checks: Redis 8.8 or later is required')
			return
		}

		mut value, mut applied := db.increx(keys[0], 1)!
		assert value == 1 && applied == 1
		value, applied = db.increx(keys[0], -11)!
		assert value == -10 && applied == -11

		assert db.set(keys[1], 99)! == 'OK'
		value, applied = db.increx(keys[1], 5, ubound: 100)!
		assert value == 99 && applied == 0
		value, applied = db.increx(keys[1], 5, ubound: 100, saturate: true)!
		assert value == 100 && applied == 1
		value, applied = db.increx(keys[1], -150, lbound: 0, saturate: true)!
		assert value == 0 && applied == -100

		value, applied = db.increx(keys[2], 1,
			ubound:           100
			expiration:       .ex
			expiration_value: 60
			enx:              true
		)!
		assert value == 1 && applied == 1
		assert db.ttl(keys[2])! in 1 .. 61
		value, applied = db.increx(keys[2], 1, expiration: .ex, expiration_value: 500, enx: true)!
		assert value == 2 && applied == 1
		assert db.ttl(keys[2])! in 1 .. 61
		value, _ = db.increx(keys[2], 1, expiration: .persist)!
		assert value == 3
		assert db.ttl(keys[2])! == -1
		db.increx(keys[2], 1, expiration: .pxat, expiration_value: time.now().unix_milli() + 120000)!
		assert db.pttl(keys[2])! in 1 .. 120001

		assert db.set(keys[3], '1.5')! == 'OK'
		mut float_value, mut float_applied := db.increx_float(keys[3], 0.25)!
		assert float_value == 1.75 && float_applied == 0.25
		float_value, float_applied = db.increx_float(keys[3], 10.0, ubound: 2.0, saturate: true)!
		assert float_value == 2.0 && float_applied == 0.25
		float_value, float_applied = db.increx_float(keys[3], 1.0, ubound: 2.5)!
		assert float_value == 2.0 && float_applied == 0.0
		assert db.get[string](keys[3])! == '2'

		assert_increx_rejected(mut db, keys[0], enx: true)
		assert_increx_rejected(mut db, keys[0], expiration_value: 10)
		assert db.set(keys[4], 'not a number')! == 'OK'
		assert_increx_rejected(mut db, keys[4])
		assert db.ping()! == 'PONG'
	}
}

fn test_json_codec() ! {
	for version in [3, 2] {
		mut db := key_commands_db(version)!
		prefix := key_commands_prefix(version)
		keys := ['${prefix}:user', '${prefix}:map', '${prefix}:list', '${prefix}:invalid',
			'${prefix}:missing']
		defer { cleanup_key_commands(mut db, keys) }
		user := CachedUser{
			name:  'Ada "\r\n'
			age:   36
			tags:  ['math', 'engines']
			admin: true
		}

		assert db.set_json(keys[0], user)! == 'OK'
		assert db.get[string](keys[0])!.starts_with('{"name":')
		assert db.get_json[CachedUser](keys[0])! == user
		assert db.set_json(keys[1], {
			'a': 1
			'b': 2
		})! == 'OK'
		assert db.get_json[map[string]int](keys[1])! == {
			'a': 1
			'b': 2
		}
		assert db.set_json(keys[2], [1, 2, 3])! == 'OK'
		assert db.get_json[[]int](keys[2])! == [1, 2, 3]

		if _ := db.get_json[CachedUser](keys[4]) {
			assert false, 'get_json must report a missing key'
		} else {
			assert err is redis.NilError
		}
		assert db.set(keys[3], 'not json')! == 'OK'
		if _ := db.get_json[CachedUser](keys[3]) {
			assert false, 'get_json must reject invalid JSON'
		} else {
			assert err !is redis.NilError
		}
		assert db.ping()! == 'PONG'
	}
}

fn test_new_commands_in_pipelines() ! {
	for version in [3, 2] {
		mut db := key_commands_db(version)!
		prefix := key_commands_prefix(version)
		keys := ['${prefix}:list', '${prefix}:hash', '${prefix}:first', '${prefix}:second',
			'${prefix}:json']
		defer { cleanup_key_commands(mut db, keys) }

		db.pipeline_start()
		assert db.rpush(keys[0], '2', '1')! == 0
		assert db.sort[string](keys[0])!.len == 0
		assert db.hset(keys[1], {
			'field': 'value'
		})! == 0
		assert db.hrandfield_withvalues(keys[1], 1)! == []redis.HashField{}
		assert db.mset({
			keys[2]: 'ohmytext'
			keys[3]: 'mynewtext'
		})! == ''
		assert db.lcs_idx(keys[2], keys[3])! == redis.LcsResult{}
		assert db.set_json(keys[4], [1, 2])! == ''
		assert db.get_json[[]int](keys[4])! == []int{}
		assert !db.renamenx(keys[2], keys[3])!
		replies := db.pipeline_execute()!
		assert replies.len == 9
		assert replies[1] is []redis.RedisValue
		assert replies[3] is []redis.RedisValue
		assert db.get_json[[]int](keys[4])! == [1, 2]
		assert db.ping()! == 'PONG'
	}
}
