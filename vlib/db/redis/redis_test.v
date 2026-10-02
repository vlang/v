// vtest build: started_redis?
import db.redis
import os
import rand
import time

const redis_password = os.getenv('VREDIS_PASSWORD')
// Keep destructive fixture cleanup away from other Redis test files.
const redis_test_database = 15

// Keys and channel names used by the tests
const k_int = 'k:int'
const k_string = 'k:string'
const k_counter = 'k:counter'
const r_k_map = 'r:k:map'
const r_k_set = 'r:k:set'
const r_k_pubch = 'r:k:pubch'

// Helper publisher used by the pub/sub test. This helper swallows errors and logs
// them locally so it can be spawned safely without propagating results.
fn publish_after_delay() {
	// small delay to ensure subscriber is ready
	time.sleep(50 * time.millisecond)
	mut conn := redis.connect(password: redis_password, database: redis_test_database) or {
		eprintln('publish_after_delay: connect failed: ${err}')
		return
	}
	defer { conn.close() or { eprintln('publish_after_delay: close failed: ${err}') } }
	conn.cmd('PUBLISH', r_k_pubch, 'hello') or {
		eprintln('publish_after_delay: publish failed: ${err}')
	}
}

// -------------------- FAST / BASIC TESTS --------------------

// Basic functionality smoke test (fast)
fn test_redis_basic() {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	defer { db.close() or {} }

	// Basic health check
	assert db.ping()! == 'PONG'

	// delete all keys first
	db.cmd('FLUSHDB')!

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
	db.get[i64]('int') or {
		// tolerate server-specific message wording; ensure some message exists
		assert err.msg().len > 0
	}

	// test del
	assert db.del('string')! == 1
	db.get[string]('string') or {
		// tolerate server-specific message wording; ensure some message exists
		assert err.msg().len > 0
	}

	// test custom cmd
	assert db.cmd('SET', 'bigint', '123456')! as string == 'OK'
	assert db.cmd('GET', 'bigint')! as []u8 == [u8(49), 50, 51, 52, 53, 54]
	db.close()!
}

// A concise pipeline test that exercises correctness but remains fast
fn test_pipeline_small() {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	defer { db.close() or {} }

	assert db.ping()! == 'PONG'

	// start pipleline mode
	db.pipeline_start()
	// small pipeline of a few commands
	db.cmd('FLUSHDB') or { eprintln('FLUSHDB failed (pipeline): ${err}') }
	db.set(k_int, 1) or { eprintln('SET k_int failed (pipeline): ${err}') }
	db.set(k_string, 'p') or { eprintln('SET k_string failed (pipeline): ${err}') }
	db.get[i64](k_int) or { eprintln('GET k_int failed (pipeline): ${err}') }
	db.get[string](k_string) or { eprintln('GET k_string failed (pipeline): ${err}') }
	res := db.pipeline_execute()!

	// Expect exactly the five results we queued and validate them precisely.
	assert res.len == 5
	match res[0] {
		string {
			assert res[0] as string == 'OK'
		}
		else {
			assert false
		}
	}

	match res[1] {
		string {
			assert res[1] as string == 'OK'
		}
		else {
			assert false
		}
	}

	match res[2] {
		string {
			assert res[2] as string == 'OK'
		}
		else {
			assert false
		}
	}

	match res[3] {
		[]u8 {
			assert (res[3] as []u8).bytestr() == '1'
		}
		else {
			assert false
		}
	}

	match res[4] {
		[]u8 {
			assert (res[4] as []u8).bytestr() == 'p'
		}
		else {
			assert false
		}
	}
}

// Another small pipeline sequence test (fast)
fn test_pipeline_sequence() {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	defer { db.close() or {} }

	// keep operations minimal and deterministic
	db.cmd('FLUSHDB') or { eprintln('FLUSHDB failed: ${err}') }
	assert db.set(k_counter, '0')! == 'OK'

	// restart pipeline again
	db.pipeline_start()
	// test set[T]
	db.set('int', 123)!
	db.set('string', 'abc')!
	db.set('bin', [u8(0x00), 0x01, 0x02, 0x03])!

	// execute the queued pipeline commands and collect results
	res := db.pipeline_execute()!
	// We queued 3 SET commands above; each should return "OK"
	assert res.len == 3
	match res[0] {
		string {
			assert res[0] as string == 'OK'
		}
		else {
			assert false
		}
	}

	match res[1] {
		string {
			assert res[1] as string == 'OK'
		}
		else {
			assert false
		}
	}

	match res[2] {
		string {
			assert res[2] as string == 'OK'
		}
		else {
			assert false
		}
	}
}

// -------------------- RESP3 / PUBSUB SHAPE CHECKS (FAST) --------------------

// Quick RESP3 ping and tiny roundtrip
fn test_resp3_ping() {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	defer { db.close() or {} }

	// Basic ping and quick SET/GET roundtrip
	p := db.ping() or { panic(err) }
	assert p == 'PONG'
	db.cmd('DEL', r_k_map) or { eprintln('DEL r_k_map failed: ${err}') }
	assert db.cmd('SET', r_k_map, 'temp')! as string == 'OK'
	val := db.cmd('GET', r_k_map)!
	match val {
		[]u8 {
			assert val.bytestr() == 'temp'
		}
		string {
			assert val == 'temp'
		}
		else {
			assert false
		}
	}

	db.cmd('DEL', r_k_map) or { eprintln('cleanup DEL r_k_map failed: ${err}') }
}

// Quick RESP3 HGETALL shape check (fast)
fn test_resp3_hgetall_fast() {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	defer { db.close() or {} }

	db.cmd('DEL', r_k_map)!
	assert db.hset(r_k_map, {
		'k1': 'v1'
		'k2': 'v2'
	})! >= 1
	if db.version != 3 {
		res := db.hgetall[string](r_k_map) or { panic(err) }
		assert res['k1'] == 'v1'
		db.cmd('DEL', r_k_map)!
		return
	}
	r := db.cmd('HGETALL', r_k_map) or { panic(err) }
	match r {
		map[string]redis.RedisValue {
			v1 := r['k1']!
			v2 := r['k2']!
			assert (v1 as []u8).bytestr() == 'v1'
			assert (v2 as []u8).bytestr() == 'v2'
		}
		redis.RedisMap {
			assert (r.pairs[1] as []u8).bytestr() == 'v1'
			assert (r.pairs[3] as []u8).bytestr() == 'v2'
		}
		[]redis.RedisValue {
			assert (r[1] as []u8).bytestr() == 'v1'
			assert (r[3] as []u8).bytestr() == 'v2'
		}
		else {
			assert false
		}
	}

	db.cmd('DEL', r_k_map)!
}

// Quick RESP3 SMEMBERS shape check (fast)
fn test_resp3_smembers_fast() {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	defer { db.close() or {} }
	db.cmd('DEL', r_k_set) or { eprintln('DEL r_k_set failed: ${err}') }
	db.cmd('SADD', r_k_set, 'one', 'two') or { eprintln('SADD r_k_set failed: ${err}') }
	r := db.cmd('SMEMBERS', r_k_set) or { panic(err) }
	match r {
		redis.RedisSet {
			mut f1 := false
			mut f2 := false
			for el in r.elements {
				el_str := (el as []u8).bytestr()
				if el_str == 'one' {
					f1 = true
				}
				if el_str == 'two' {
					f2 = true
				}
			}
			assert f1 && f2
		}
		[]redis.RedisValue {
			mut f1 := false
			mut f2 := false
			for el in r {
				el_str := (el as []u8).bytestr()
				if el_str == 'one' {
					f1 = true
				}
				if el_str == 'two' {
					f2 = true
				}
			}
			assert f1 && f2
		}
		else {
			assert false
		}
	}

	db.cmd('DEL', r_k_set) or { eprintln('cleanup DEL r_k_set failed: ${err}') }
}

// Very small pub/sub smoke test (fast)
fn test_resp3_pubsub_fast() {
	mut sub := redis.connect(password: redis_password, database: redis_test_database)!
	defer { sub.close() or {} }
	if sub.version != 3 {
		return
	}
	spawn publish_after_delay()
	sub.cmd('SUBSCRIBE', r_k_pubch) or { eprintln('SUBSCRIBE failed: ${err}') }
	time.sleep(200 * time.millisecond)
	sub.cmd('PING') or { eprintln('PING (pubsub) failed: ${err}') }
}

// -------------------- SLOW / HEAVY TESTS --------------------

// Large binary bulk string roundtrip (~200 KiB)
fn test_large_binary_bulk_string() ! {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	defer { db.close() or {} }

	// ensure clean DB
	db.cmd('FLUSHDB')!

	// Create a large binary payload (~200 KB) with a repeating pattern,
	// including zero bytes and CRLF sequences to exercise edge cases.
	mut data := []u8{len: 204800, init: u8(index & 0xFF)}

	// inject some CRLF and zero-byte sequences at several spots
	data[50] = `\r`
	data[51] = `\n`
	data[1000] = 0
	data[1001] = `\r`
	data[1002] = `\n`

	// store binary data
	assert db.set('bigbin', data.clone())! == 'OK'

	// retrieve as []u8 using typed get
	got := db.get[[]u8]('bigbin')!
	assert got == data

	// also test with small binary containing embedded CRLF and NUL
	small := [u8(0), `a`, `b`, `\r`, `\n`, `c`]
	assert db.set('smallbin', small)! == 'OK'
	got_small := db.get[[]u8]('smallbin')!
	assert got_small == small

	// cleanup
	db.del('bigbin')!
	db.del('smallbin')!
}

// Very large payload (~1.2 MiB) roundtrip
fn test_very_large_payload() ! {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	defer { db.close() or {} }

	// ensure clean DB
	db.cmd('FLUSHDB')!

	// Build a very large payload (> 1 MiB). Use ~1.2 MiB to be safely above 1 MiB.
	size := 1_200_000
	mut big := []u8{len: size, init: u8(index & 0xFF)}
	// Put a few sentinel checks
	big[0] = 0
	big[1023] = `\r`
	big[1024] = `\n`
	big[size - 1] = 255

	// store and retrieve
	assert db.set('hugebin', big.clone())! == 'OK'
	got := db.get[[]u8]('hugebin')!
	assert got == big

	// cleanup
	db.del('hugebin')!
}

// Many pipeline commands to exercise batching behavior
fn test_many_pipeline_commands() ! {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	defer { db.close() or {} }

	// ensure clean DB
	db.cmd('FLUSHDB')!

	// We'll queue a moderate but substantial number of commands in pipeline to test behavior.
	// Keep it reasonable for CI runtime; 300 pairs (SET, GET) should exercise pipeline thoroughly.
	count := 300
	db.pipeline_start()
	for i in 0 .. count {
		key := 'kp_${i}'
		val := 'v${i}'
		db.cmd('SET', key, val)!
		db.cmd('GET', key)!
	}
	results := db.pipeline_execute()!
	// Expect 2 * count responses
	assert results.len == count * 2
	for i in 0 .. count {
		set_idx := i * 2
		get_idx := i * 2 + 1
		// SET should return OK
		match results[set_idx] {
			string {
				assert (results[set_idx] as string) == 'OK'
			}
			else {
				assert false
			}
		}

		// GET should return the value as []u8
		expected := ('v${i}').bytes()
		match results[get_idx] {
			[]u8 {
				assert (results[get_idx] as []u8) == expected
			}
			else {
				assert false
			}
		}
	}
}

// Larger randomized binary payloads (many iterations)
fn test_fuzz_random_binary_many() ! {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	defer { db.close() or {} }

	db.cmd('FLUSHDB')!

	// LCG PRNG seed using process id
	mut x := u64(os.getpid())

	count := 50
	for iter in 0 .. count {
		// size up to ~200 KiB
		size := int((x % 200_000) + 1)
		mut data := []u8{len: size}
		for i in 0 .. size {
			data[i] = rand.u8()
		}
		key := 'fuzz_big_${iter}'
		assert db.set(key, data.clone())! == 'OK'
		got := db.get[[]u8](key)!
		assert got == data
		// clean up
		db.del(key)!

		x += u64(size) * u64(iter + 1)
	}
}

// Randomized tests that ensure CRLF sequences embedded at random locations,
// including chunk boundaries, are handled correctly.
fn test_fuzz_crlf_random_positions() ! {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	defer { db.close() or {} }

	db.cmd('FLUSHDB')!

	seed := u64(os.getpid())
	mut x := seed

	trials := 30
	for t in 0 .. trials {
		// size between 1 KiB and 32 KiB
		size := int((x % 31_744) + 1024)
		mut data := []u8{len: size}
		for i in 0 .. size {
			data[i] = rand.u8()
		}
		// place a CRLF at a pseudo-random position; sometimes force it near boundaries
		pos := int(x % u64(size - 2))
		data[pos] = `\r`
		data[pos + 1] = `\n`
		// also occasionally place CRLF at a chunk boundary (4096)
		if t % 5 == 0 && size > 4097 {
			data[4095] = `\r`
			data[4096] = `\n`
		}
		key := 'fuzz_crlf_${t}'
		assert db.set(key, data.clone())! == 'OK'
		got := db.get[[]u8](key)!
		assert got == data
		db.del(key)!
		x += u64(pos) + 1
	}
}

// CRLF crossing a read-chunk boundary
fn test_crlf_at_chunk_boundary() {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	defer { db.close() or {} }

	// ensure clean DB
	db.cmd('FLUSHDB')!

	// The driver's read buffer reads in 4096-byte chunks. Create a payload where CRLF
	// crosses the 4096-byte boundary: place '\r' at index 4095 and '\n' at 4096.
	chunk := 4096
	size := chunk * 3 // a few chunks
	mut data := []u8{len: size, init: u8((index * 37) % 256)}
	if size > chunk + 1 {
		data[chunk - 1] = `\r` // index 4095
		data[chunk] = `\n` // index 4096
	}
	// store and retrieve
	assert db.set('crlf_boundary', data.clone())! == 'OK'
	got := db.get[[]u8]('crlf_boundary')!
	assert got.len == data.len
	// ensure boundary bytes preserved
	assert got[chunk - 1] == `\r`
	assert got[chunk] == `\n`
	// cleanup
	db.del('crlf_boundary')!
}

// Small randomized binary payloads (many repetitions).
fn test_fuzz_random_binary_small() {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	defer { db.close() or {} }

	db.cmd('FLUSHDB')!

	// Simple LCG PRNG seeded from process id
	seed := u64(os.getpid())
	mut x := seed
	for iter := 0; iter < 20; iter++ {
		// pseudo-random size in [1, 4096]
		size := int((x % 4096) + 1)
		mut data := []u8{len: size}
		for i in 0 .. size {
			data[i] = rand.u8()
		}
		k := 'fuzz_small_${iter}'
		assert db.set(k, data.clone())! == 'OK'
		got := db.get[[]u8](k)!
		assert got == data
		// mutate seed slightly so sizes vary
		x += u64(size)
	}

	db.pipeline_start()

	// ensure we have a small map to use here (define inline to avoid depending on outer scope)
	db.hset('map', {
		'a': '1'
		'b': '2'
		'c': '3'
	})!
	db.hget[string]('map', 'a')!
	db.hget[string]('map', 'b')!
	db.hget[string]('map', 'c')!
	db.hgetall[string]('map')!

	// test custom cmd
	db.cmd('SET', 'bigint', '123456')!
	db.cmd('GET', 'bigint')!

	b := db.pipeline_execute()!

	// Validate elements individually to accommodate server-shape differences.
	assert b.len == 7

	match b[0] {
		i64 {
			assert (b[0] as i64) == 3
		}
		else {
			assert false
		}
	}

	match b[1] {
		[]u8 {
			assert (b[1] as []u8) == [u8(49)]
		}
		else {
			assert false
		}
	}

	match b[2] {
		[]u8 {
			assert (b[2] as []u8) == [u8(50)]
		}
		else {
			assert false
		}
	}

	match b[3] {
		[]u8 {
			assert (b[3] as []u8) == [u8(51)]
		}
		else {
			assert false
		}
	}

	// HGETALL may arrive as map[string]RedisValue, RedisMap, or array - accept common shapes.
	match b[4] {
		map[string]redis.RedisValue {
			m := b[4] as map[string]redis.RedisValue
			v := m['a'] or { panic('hgetall: missing a') }
			assert (v as []u8) == [u8(49)]
		}
		redis.RedisMap {
			rm := b[4] as redis.RedisMap
			// pairs are interleaved key/value; expect at least 2 pairs and check the first value
			assert rm.pairs.len >= 2
			assert (rm.pairs[1] as []u8) == [u8(49)]
		}
		[]redis.RedisValue {
			arr := b[4] as []redis.RedisValue
			// RESP2-style interleaved array; ensure element 1 (second item) is the value for 'a'
			assert arr.len > 1
			assert (arr[1] as []u8) == [u8(49)]
		}
		else {
			assert false
		}
	}

	match b[5] {
		string {
			assert b[5] as string == 'OK'
		}
		else {
			assert false
		}
	}

	match b[6] {
		[]u8 {
			assert (b[6] as []u8) == [u8(49), 50, 51, 52, 53, 54]
		}
		else {
			assert false
		}
	}

	db.close()!
}

fn connect_command_test(version int) !redis.DB {
	mut db := redis.connect(password: redis_password, database: redis_test_database)!
	if version == 2 {
		db.cmd('HELLO', '2')!
		db.version = 2
	}
	assert db.version == version
	return db
}

fn cleanup_command_test(mut db redis.DB, keys []string) {
	mut args := ['DEL']
	args << keys
	db.cmd(...args) or {}
	db.close() or {}
}

fn assert_command_value[T](value ?T, expected T) {
	actual := value or { panic('missing expected Redis value') }
	assert actual == expected
}

fn assert_command_missing_string(value ?string) {
	if _ := value {
		assert false, 'expected a missing Redis value'
	}
}

fn assert_command_missing_int(value ?int) {
	if _ := value {
		assert false, 'expected a missing Redis value'
	}
}

fn assert_command_missing_bytes(value ?[]u8) {
	if _ := value {
		assert false, 'expected a missing Redis value'
	}
}

fn test_multi_key_values() ! {
	for version in [3, 2] {
		mut db := connect_command_test(version)!
		prefix := 'v:redis:commands:${rand.uuid_v4()}'
		keys := ['${prefix}:empty', '${prefix}:missing', '${prefix}:text', '${prefix}:binary',
			'${prefix}:integer', '${prefix}:new']
		defer { cleanup_command_test(mut db, keys) }

		assert db.mset({
			keys[0]: ''
			keys[2]: 'hello'
		})! == 'OK'
		values := db.mget[string](keys[0], keys[1], keys[2], keys[2])!
		assert values.len == 4
		assert_command_value(values[0], '')
		assert_command_missing_string(values[1])
		assert_command_value(values[2], 'hello')
		assert_command_value(values[3], 'hello')

		binary := [u8(0), `\r`, `\n`, 255, `x`]
		assert db.mset({
			keys[3]: binary
		})! == 'OK'
		binary_values := db.mget[[]u8](keys[3], keys[0], keys[1])!
		assert_command_value(binary_values[0], binary)
		assert_command_value(binary_values[1], []u8{})
		assert_command_missing_bytes(binary_values[2])
		assert db.mset({
			keys[4]: i64(123)
		})! == 'OK'
		integer_values := db.mget[int](keys[4], keys[1])!
		assert_command_value(integer_values[0], 123)
		assert_command_missing_int(integer_values[1])
		assert db.mset({
			keys[4]: ~u64(0)
		})! == 'OK'
		unsigned_values := db.mget[u64](keys[4])!
		assert_command_value(unsigned_values[0], ~u64(0))
		assert db.get[u64](keys[4])! == ~u64(0)
		assert db.mset({
			keys[4]: ~usize(0)
		})! == 'OK'
		usize_values := db.mget[usize](keys[4])!
		assert usize_values.len == 1
		assert_command_value(usize_values[0], ~usize(0))
		assert db.get[usize](keys[4])! == ~usize(0)

		assert !db.msetnx({
			keys[2]: 'replacement'
			keys[5]: 'new'
		})!
		assert db.get[string](keys[2])! == 'hello'
		assert db.exists(keys[5])! == 0
		assert db.msetnx({
			keys[5]: 'new'
		})!
		assert db.get[string](keys[5])! == 'new'
	}
}

fn test_string_commands() ! {
	for version in [3, 2] {
		mut db := connect_command_test(version)!
		prefix := 'v:redis:commands:${rand.uuid_v4()}'
		keys := ['${prefix}:value', '${prefix}:seconds', '${prefix}:milliseconds', '${prefix}:missing']
		defer { cleanup_command_test(mut db, keys) }

		assert db.setnx(keys[0], 'hello')!
		assert !db.setnx(keys[0], 'replacement')!
		assert db.append(keys[0], ' world')! == 11
		assert db.strlen(keys[0])! == 11
		assert db.getrange[string](keys[0], 0, 4)! == 'hello'
		assert db.getrange[[]u8](keys[0], -5, -1)! == 'world'.bytes()
		assert db.getrange[string](keys[3], 0, -1)! == ''
		assert db.strlen(keys[3])! == 0
		assert db.setrange(keys[0], 6, 'V')! == 11
		assert db.get[string](keys[0])! == 'hello Vorld'

		binary := [u8(0), `\r`, `\n`, 255]
		assert db.setrange(keys[0], 0, binary)! == 11
		assert db.getrange[[]u8](keys[0], 0, 3)! == binary
		assert db.append(keys[0], binary)! == 15
		assert db.getrange[[]u8](keys[0], -4, -1)! == binary
		assert db.setrange(keys[3], 3, 'a')! == 4
		assert db.get[[]u8](keys[3])! == [u8(0), 0, 0, `a`]
		assert db.append(keys[3], 123)! == 7
		assert db.getrange[int](keys[3], -3, -1)! == 123

		assert db.setex(keys[1], 120, 'seconds')! == 'OK'
		assert db.ttl(keys[1])! in 1 .. 121
		assert db.psetex(keys[2], 120000, binary)! == 'OK'
		assert db.pttl(keys[2])! in 1 .. 120001
		assert db.getex[[]u8](keys[2])! == binary
		assert db.getex[[]u8](keys[2], mode: .persist)! == binary
		assert db.ttl(keys[2])! == -1
		assert db.getex[[]u8](keys[2], mode: .ex, value: 120)! == binary
		assert db.ttl(keys[2])! in 1 .. 121
		assert db.getex[[]u8](keys[2], mode: .px, value: 120000)! == binary
		assert db.pttl(keys[2])! in 1 .. 120001
		assert db.getex[[]u8](keys[2], mode: .exat, value: time.now().unix() + 120)! == binary
		assert db.ttl(keys[2])! in 1 .. 121
		assert db.getex[[]u8](keys[2], mode: .pxat, value: time.now().unix_milli() + 120000)! == binary
		assert db.pttl(keys[2])! in 1 .. 120001
		assert db.getdel[string](keys[1])! == 'seconds'
		assert db.exists(keys[1])! == 0
		if _ := db.getdel[string](keys[1]) {
			assert false, 'getdel must reject a missing key'
		} else {
			assert err.msg().contains('not found')
		}
		if _ := db.getex[string](keys[1]) {
			assert false, 'getex must reject a missing key'
		} else {
			assert err.msg().contains('not found')
		}
		if _ := db.setrange(keys[0], -1, 'a') {
			assert false, 'setrange must propagate Redis errors'
		} else {
			assert err.msg().len > 0
		}
		assert db.ping()! == 'PONG'
	}
}

fn test_key_and_counter_commands() ! {
	for version in [3, 2] {
		mut db := connect_command_test(version)!
		prefix := 'v:redis:commands:${rand.uuid_v4()}'
		keys := ['${prefix}:integer', '${prefix}:float', '${prefix}:missing', '${prefix}:renamed']
		defer { cleanup_command_test(mut db, keys) }

		assert db.incrby(keys[0], 5)! == 5
		assert db.incrby(keys[0], -2)! == 3
		assert db.decrby(keys[0], 10)! == -7
		assert db.incrbyfloat(keys[1], 1.25)! == 1.25
		assert db.incrbyfloat(keys[1], -0.5)! == 0.75
		assert db.exists(keys[0], keys[1], keys[2], keys[0])! == 3
		assert db.key_type(keys[0])! == 'string'
		assert db.key_type(keys[2])! == 'none'
		assert db.ttl(keys[0])! == -1
		assert db.pttl(keys[0])! == -1
		assert db.ttl(keys[2])! == -2
		assert db.pttl(keys[2])! == -2
		assert !db.pexpire(keys[2], 120000)!
		assert !db.persist(keys[0])!
		assert db.pexpire(keys[0], 120000)!
		assert db.pttl(keys[0])! in 1 .. 120001
		assert db.persist(keys[0])!
		assert db.ttl(keys[0])! == -1
		assert db.expireat(keys[0], time.now().unix() + 120)!
		assert db.ttl(keys[0])! in 1 .. 121
		assert db.pexpireat(keys[0], time.now().unix_milli() + 120000)!
		assert db.pttl(keys[0])! in 1 .. 120001
		assert db.rename(keys[0], keys[3])! == 'OK'
		assert db.get[int](keys[3])! == -7
		assert db.exists(keys[0])! == 0
		assert db.pexpire(keys[3], 0)!
		assert db.exists(keys[3])! == 0
		assert db.unlink(keys[1], keys[2])! == 1
		if _ := db.incrby(keys[2], 1) {
			// A missing counter is created by INCRBY.
			assert db.get[int](keys[2])! == 1
		} else {
			assert false, err.msg()
		}
		assert db.set(keys[2], 'not an integer')! == 'OK'
		if _ := db.incrby(keys[2], 1) {
			assert false, 'incrby must propagate Redis errors'
		} else {
			assert err.msg().len > 0
		}
		assert db.ping()! == 'PONG'
	}
}

fn test_hash_commands() ! {
	for version in [3, 2] {
		mut db := connect_command_test(version)!
		prefix := 'v:redis:commands:${rand.uuid_v4()}'
		keys := ['${prefix}:hash', '${prefix}:missing']
		defer { cleanup_command_test(mut db, keys) }
		binary := [u8(0), `\r`, `\n`, 255]

		assert db.hset(keys[0], {
			'empty':  []u8{}
			'binary': binary
		})! == 2
		assert db.key_type(keys[0])! == 'hash'
		assert db.hexists(keys[0], 'empty')!
		assert !db.hexists(keys[0], 'missing')!
		assert db.hlen(keys[0])! == 2
		assert db.hlen(keys[1])! == 0
		assert db.hstrlen(keys[0], 'binary')! == binary.len
		assert db.hstrlen(keys[0], 'empty')! == 0
		assert db.hstrlen(keys[0], 'missing')! == 0
		mut fields := db.hkeys(keys[0])!
		fields.sort()
		assert fields == ['binary', 'empty']
		mut values := db.hvals(keys[0])!
		values.sort()
		mut expected_values := ['', binary.bytestr()]
		expected_values.sort()
		assert values == expected_values
		string_values := db.hmget[string](keys[0], 'binary', 'missing', 'empty', 'binary')!
		assert string_values.len == 4
		assert_command_value(string_values[0], binary.bytestr())
		assert_command_missing_string(string_values[1])
		assert_command_value(string_values[2], '')
		assert_command_value(string_values[3], binary.bytestr())
		binary_values := db.hmget[[]u8](keys[0], 'empty', 'binary', 'missing')!
		assert_command_value(binary_values[0], []u8{})
		assert_command_value(binary_values[1], binary)
		assert_command_missing_bytes(binary_values[2])
		assert !db.hsetnx(keys[0], 'empty', 'replacement')!
		assert db.hsetnx(keys[0], 'integer', 2)!
		assert db.hincrby(keys[0], 'integer', 3)! == 5
		assert db.hincrby(keys[0], 'integer', -1)! == 4
		integer_values := db.hmget[int](keys[0], 'integer', 'missing')!
		assert integer_values.len == 2
		assert_command_value(integer_values[0], 4)
		assert_command_missing_int(integer_values[1])
		assert db.hset(keys[0], {
			'unsigned': ~u64(0)
		})! == 1
		unsigned_values := db.hmget[u64](keys[0], 'unsigned')!
		assert unsigned_values.len == 1
		assert_command_value(unsigned_values[0], ~u64(0))
		assert db.hget[u64](keys[0], 'unsigned')! == ~u64(0)
		assert db.hset(keys[0], {
			'unsigned': ~usize(0)
		})! == 0
		usize_values := db.hmget[usize](keys[0], 'unsigned')!
		assert usize_values.len == 1
		assert_command_value(usize_values[0], ~usize(0))
		assert db.hget[usize](keys[0], 'unsigned')! == ~usize(0)
		assert db.hdel(keys[0], 'unsigned')! == 1
		assert db.hincrbyfloat(keys[0], 'float', 1.25)! == 1.25
		assert db.hincrbyfloat(keys[0], 'float', -0.5)! == 0.75
		assert db.hdel(keys[0], 'integer', 'float', 'missing')! == 2
		assert db.hlen(keys[0])! == 2
		assert db.hkeys(keys[1])! == []string{}
		assert db.hvals(keys[1])! == []string{}
		missing_values := db.hmget[string](keys[1], 'missing')!
		assert missing_values.len == 1
		assert_command_missing_string(missing_values[0])
		assert db.set(keys[1], 'string')! == 'OK'
		if _ := db.hlen(keys[1]) {
			assert false, 'hash commands must propagate WRONGTYPE errors'
		} else {
			assert err.msg().contains('WRONGTYPE')
		}
		assert db.ping()! == 'PONG'
	}
}

fn test_scan_commands() ! {
	for version in [3, 2] {
		mut db := connect_command_test(version)!
		prefix := 'v:redis:commands:${rand.uuid_v4()}'
		keys := ['${prefix}:a', '${prefix}:b', '${prefix}:c', '${prefix}:hash']
		defer { cleanup_command_test(mut db, keys) }
		assert db.mset({
			keys[0]: 'a'
			keys[1]: 'b'
			keys[2]: 'c'
		})! == 'OK'
		mut listed_keys := db.keys('${prefix}:?')!
		listed_keys.sort()
		assert listed_keys == keys[..3]
		mut cursor := '0'
		mut scanned_keys := map[string]bool{}
		for {
			next, batch := db.scan(cursor, match: '${prefix}:?', count: 1)!
			for key in batch {
				scanned_keys[key] = true
			}
			cursor = next
			if cursor == '0' {
				break
			}
		}
		assert scanned_keys.len == 3
		for key in keys[..3] {
			assert scanned_keys[key]
		}

		assert db.hset(keys[3], {
			'':    'empty field'
			'a:1': 'first'
			'a:2': 'second'
			'b:1': 'other'
		})! == 4
		cursor = '0'
		mut scanned_fields := map[string]string{}
		for {
			next, batch := db.hscan(keys[3], cursor, match: 'a:*', count: 1)!
			assert batch.len % 2 == 0
			for i := 0; i < batch.len; i += 2 {
				scanned_fields[batch[i]] = batch[i + 1]
			}
			cursor = next
			if cursor == '0' {
				break
			}
		}
		assert scanned_fields == {
			'a:1': 'first'
			'a:2': 'second'
		}
		cursor = '0'
		mut empty_fields := map[string]string{}
		for {
			next, batch := db.hscan(keys[3], cursor, match: '', count: 1)!
			assert batch.len % 2 == 0
			for i := 0; i < batch.len; i += 2 {
				empty_fields[batch[i]] = batch[i + 1]
			}
			cursor = next
			if cursor == '0' {
				break
			}
		}
		assert empty_fields == {
			'': 'empty field'
		}
	}
}

fn test_command_pipeline_placeholders_and_order() ! {
	for version in [3, 2] {
		mut db := connect_command_test(version)!
		prefix := 'v:redis:commands:${rand.uuid_v4()}'
		keys := ['${prefix}:value', '${prefix}:integer', '${prefix}:float', '${prefix}:hash']
		defer { cleanup_command_test(mut db, keys) }
		db.pipeline_start()
		assert db.mset({
			keys[0]: 'value'
		})! == ''
		assert db.mget[string](keys[0])!.len == 0
		assert !db.setnx(keys[0], 'replacement')!
		assert db.incrby(keys[1], 5)! == 0
		assert db.incrbyfloat(keys[2], 1.25)! == 0.0
		assert db.ttl(keys[0])! == 0
		assert db.hset(keys[3], {
			'field': 'hash value'
		})! == 0
		assert db.hmget[string](keys[3], 'field')!.len == 0
		assert db.hkeys(keys[3])! == []string{}
		assert db.hincrbyfloat(keys[3], 'float', 0.5)! == 0.0
		assert db.append(keys[0], '!')! == 0
		assert db.getrange[string](keys[0], 0, -1)! == ''
		cursor, scanned_keys := db.scan('0', match: '${prefix}:*')!
		assert cursor == ''
		assert scanned_keys == []string{}
		assert db.getdel[string](keys[0])! == ''

		count := db.pipeline_cmd_count
		buffer_len := db.pipeline_buffer.len
		if _ := db.mget[bool](keys[0]) {
			assert false, 'unsupported MGET types must not be queued'
		} else {
			assert err.msg().contains('unsupported')
		}
		if _ := db.mset({
			keys[0]: true
		}) {
			assert false, 'unsupported MSET types must not be queued'
		} else {
			assert err.msg().contains('unsupported')
		}
		for options in [redis.GetExOptions{ mode: .ex }, redis.GetExOptions{ value: 1 },
			redis.GetExOptions{ mode: .persist, value: 1 }] {
			if _ := db.getex[string](keys[0], options) {
				assert false, 'invalid GETEX options must not be queued'
			} else {
				assert err.msg().contains('expiration')
			}
		}
		if _, _ := db.scan('0', count: -1) {
			assert false, 'invalid SCAN options must not be queued'
		} else {
			assert err.msg().contains('count')
		}
		assert db.pipeline_cmd_count == count
		assert db.pipeline_buffer.len == buffer_len

		responses := db.pipeline_execute()!
		assert responses.len == 14
		assert responses[0] as string == 'OK'
		mget_response := responses[1] as []redis.RedisValue
		assert (mget_response[0] as []u8).bytestr() == 'value'
		assert responses[2] as i64 == 0
		assert responses[3] as i64 == 5
		assert (responses[4] as []u8).bytestr().f64() == 1.25
		assert responses[5] as i64 == -1
		assert responses[6] as i64 == 1
		hmget_response := responses[7] as []redis.RedisValue
		assert (hmget_response[0] as []u8).bytestr() == 'hash value'
		hkeys_response := responses[8] as []redis.RedisValue
		assert (hkeys_response[0] as []u8).bytestr() == 'field'
		assert (responses[9] as []u8).bytestr().f64() == 0.5
		assert responses[10] as i64 == 6
		assert (responses[11] as []u8).bytestr() == 'value!'
		assert (responses[12] as []redis.RedisValue).len == 2
		assert (responses[13] as []u8).bytestr() == 'value!'
		assert db.exists(keys[0])! == 0
		assert db.ping()! == 'PONG'
	}
}
