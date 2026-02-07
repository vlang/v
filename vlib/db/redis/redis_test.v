// vtest build: started_redis?

module main

import db.redis
import os
import time

// FAST tests only: small, quick operations that complete rapidly.
// Heavy/slow tests (large payloads, extensive fuzzing, long pipelines)
// are included below but intended to be the slower portion of the test suite.

// Top-level keys for the tests
const prefix = 'fast_${os.getpid()}_'
const k_int = '${prefix}int'
const k_string = '${prefix}string'
const k_bin = '${prefix}bin'
const k_map = '${prefix}map'
const k_bigint = '${prefix}bigint'
const k_counter = '${prefix}counter'
const k_p_a = '${prefix}p_a'
const k_p_b = '${prefix}p_b'

// RESP3 quick checks use a distinct prefix to avoid collisions with other tests
const resp3_prefix = 'resp3_fast_${os.getpid()}_'
const r_k_map = '${resp3_prefix}map'
const r_k_set = '${resp3_prefix}set'
const r_k_pubch = '${resp3_prefix}pubch'

// Optional Redis password (kept for compatibility with environments that set it)
const redis_password = os.getenv('VREDIS_PASSWORD')

// Helper publisher used by the pub/sub test. This helper swallows errors and logs
// them locally so it can be spawned safely without propagating results.
fn publish_after_delay() {
	// small delay to ensure subscriber is ready
	time.sleep(50 * time.millisecond)
	mut conn := redis.connect() or {
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
	mut db := redis.connect()!
	defer { db.close() or { eprintln('test_redis_basic: close failed: ${err}') } }

	// Basic health check
	assert db.ping()! == 'PONG'

	// Keep DB small and local for fast execution
	db.cmd('FLUSHALL') or { eprintln('FLUSHALL failed: ${err}') }

	// small set/get checks
	assert db.set(k_int, 42)! == 'OK'
	assert db.set(k_string, 'hello')! == 'OK'
	assert db.set(k_bin, [u8(0x00), 0x01])! == 'OK'

	assert db.get[i64](k_int)! == 42
	assert db.get[string](k_string)! == 'hello'
	assert db.get[[]u8](k_bin)! == [u8(0x00), 0x01]

	// small hash checks
	mut hm := map[string]string{}
	hm['a'] = '1'
	hm['b'] = '2'
	assert db.hset(k_map, hm)! == hm.len
	assert db.hget[string](k_map, 'a')! == '1'
	res_map := db.hgetall[string](k_map)!
	assert res_map['b'] == '2'

	// tiny expire check
	assert db.expire(k_int, 1)!
	time.sleep(1100 * time.millisecond)
	db.get[i64](k_int) or {}
	// cleanup
	db.del(k_string) or { eprintln('cleanup del k_string failed: ${err}') }
	db.del(k_map) or { eprintln('cleanup del k_map failed: ${err}') }
}

// A concise pipeline test that exercises correctness but remains fast
fn test_pipeline_small() {
	mut db := redis.connect()!
	defer { db.close() or { eprintln('test_pipeline_small: close failed: ${err}') } }

	assert db.ping()! == 'PONG'

	db.pipeline_start()
	// small pipeline of a few commands
	db.cmd('FLUSHALL') or { eprintln('FLUSHALL failed (pipeline): ${err}') }
	db.set(k_int, 1) or { eprintln('SET k_int failed (pipeline): ${err}') }
	db.set(k_string, 'p') or { eprintln('SET k_string failed (pipeline): ${err}') }
	db.get[i64](k_int) or { eprintln('GET k_int failed (pipeline): ${err}') }
	db.get[string](k_string) or { eprintln('GET k_string failed (pipeline): ${err}') }
	res := db.pipeline_execute()!

	// Expect exactly the five results we queued and validate them precisely.
	assert res.len == 5
	match res[0] {
		string { assert res[0] as string == 'OK' }
		else { assert false }
	}
	match res[1] {
		string { assert res[1] as string == 'OK' }
		else { assert false }
	}
	match res[2] {
		string { assert res[2] as string == 'OK' }
		else { assert false }
	}
	match res[3] {
		[]u8 { assert (res[3] as []u8).bytestr() == '1' }
		else { assert false }
	}
	match res[4] {
		[]u8 { assert (res[4] as []u8).bytestr() == 'p' }
		else { assert false }
	}
}

// Another small pipeline sequence test (fast)
fn test_pipeline_sequence() {
	mut db := redis.connect()!
	defer { db.close() or { eprintln('test_pipeline_sequence: close failed: ${err}') } }

	// keep operations minimal and deterministic
	db.cmd('FLUSHALL') or { eprintln('FLUSHALL failed: ${err}') }
	assert db.set(k_counter, '0')! == 'OK'

	db.pipeline_start()
	db.cmd('SET', k_p_a, 'alpha') or { eprintln('SET p_a failed: ${err}') }
	db.cmd('SET', k_p_b, 'beta') or { eprintln('SET p_b failed: ${err}') }
	db.cmd('GET', k_p_a) or { eprintln('GET p_a failed: ${err}') }
	db.cmd('INCR', k_counter) or { eprintln('INCR k_counter failed: ${err}') }
	db.cmd('GET', k_counter) or { eprintln('GET k_counter failed: ${err}') }
	res := db.pipeline_execute()!

	assert res.len == 5
	match res[0] {
		string { assert res[0] as string == 'OK' }
		else { assert false }
	}
	match res[2] {
		[]u8 { assert (res[2] as []u8).bytestr() == 'alpha' }
		else { assert false }
	}
	match res[4] {
		[]u8 { assert (res[4] as []u8).bytestr() == '1' }
		else { assert false }
	}
}

// -------------------- RESP3 / PUBSUB SHAPE CHECKS (FAST) --------------------

// Quick RESP3 ping and tiny roundtrip
fn test_resp3_ping() {
	mut db := redis.connect()!
	defer { db.close() or { eprintln('test_resp3_ping: close failed: ${err}') } }

	// Basic ping and quick SET/GET roundtrip
	p := db.ping() or { panic(err) }
	assert p == 'PONG'
	db.cmd('DEL', r_k_map) or { eprintln('DEL r_k_map failed: ${err}') }
	assert db.cmd('SET', r_k_map, 'temp')! as string == 'OK'
	val := db.cmd('GET', r_k_map) or { panic(err) }
	match val {
		[]u8 { assert (val as []u8).bytestr() == 'temp' }
		string { assert val as string == 'temp' }
		else { assert false }
	}
	db.cmd('DEL', r_k_map) or { eprintln('cleanup DEL r_k_map failed: ${err}') }
}

// Quick RESP3 HGETALL shape check (fast)
fn test_resp3_hgetall_fast() {
	mut db := redis.connect()!
	defer { db.close() or { eprintln('test_resp3_hgetall_fast: close failed: ${err}') } }
	db.cmd('DEL', r_k_map) or { eprintln('DEL r_k_map failed: ${err}') }
	assert db.hset(r_k_map, {
		'k1': 'v1'
		'k2': 'v2'
	})! >= 1
	if db.version != 3 {
		res := db.hgetall[string](r_k_map) or { panic(err) }
		assert res['k1'] == 'v1'
		db.cmd('DEL', r_k_map) or { eprintln('cleanup DEL r_k_map failed: ${err}') }
		return
	}
	r := db.cmd('HGETALL', r_k_map) or { panic(err) }
	match r {
		map[string]redis.RedisValue {
			m := r as map[string]redis.RedisValue
			assert (m['k1'] as []u8).bytestr() == 'v1'
			assert (m['k2'] as []u8).bytestr() == 'v2'
		}
		redis.RedisMap {
			rm := r as redis.RedisMap
			assert (rm.pairs[1] as []u8).bytestr() == 'v1'
			assert (rm.pairs[3] as []u8).bytestr() == 'v2'
		}
		[]redis.RedisValue {
			arr := r as []redis.RedisValue
			assert (arr[1] as []u8).bytestr() == 'v1'
			assert (arr[3] as []u8).bytestr() == 'v2'
		}
		else {
			assert false
		}
	}
	db.cmd('DEL', r_k_map) or { eprintln('cleanup DEL r_k_map failed: ${err}') }
}

// Quick RESP3 SMEMBERS shape check (fast)
fn test_resp3_smembers_fast() {
	mut db := redis.connect()!
	defer { db.close() or { eprintln('test_resp3_smembers_fast: close failed: ${err}') } }
	db.cmd('DEL', r_k_set) or { eprintln('DEL r_k_set failed: ${err}') }
	db.cmd('SADD', r_k_set, 'one', 'two') or { eprintln('SADD r_k_set failed: ${err}') }
	r := db.cmd('SMEMBERS', r_k_set) or { panic(err) }
	match r {
		redis.RedisSet {
			s := r as redis.RedisSet
			mut f1 := false
			mut f2 := false
			for el in s.elements {
				if (el as []u8).bytestr() == 'one' {
					f1 = true
				}
				if (el as []u8).bytestr() == 'two' {
					f2 = true
				}
			}
			assert f1 && f2
		}
		[]redis.RedisValue {
			arr := r as []redis.RedisValue
			mut f1 := false
			mut f2 := false
			for el in arr {
				if (el as []u8).bytestr() == 'one' {
					f1 = true
				}
				if (el as []u8).bytestr() == 'two' {
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
	mut sub := redis.connect()!
	defer { sub.close() or { eprintln('test_resp3_pubsub_fast: close failed: ${err}') } }
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
	mut db := redis.connect()!
	defer { db.close() or {} }

	// ensure clean DB
	db.cmd('FLUSHALL')!

	// Create a large binary payload (~200 KB) with a repeating pattern,
	// including zero bytes and CRLF sequences to exercise edge cases.
	size := 200 * 1024 // 200 KiB
	mut data := []u8{len: size}
	for i in 0 .. size {
		// pattern: 0..255 repeated
		data[i] = u8(i % 256)
	}
	// inject some CRLF and zero-byte sequences at several spots
	if size > 100 {
		data[50] = `\r`
		data[51] = `\n`
		data[1000] = 0
		data[1001] = `\r`
		data[1002] = `\n`
	}

	// store binary data
	assert db.set('bigbin', data.clone())! == 'OK'

	// retrieve as []u8 using typed get
	got := db.get[[]u8]('bigbin')!
	assert got.len == data.len
	// spot-check a few offsets to ensure exact match
	assert got[0] == data[0]
	assert got[50] == data[50]
	assert got[51] == data[51]
	assert got[1000] == data[1000]
	assert got[1001] == data[1001]
	assert got[1002] == data[1002]

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
	mut db := redis.connect()!
	defer { db.close() or {} }

	// ensure clean DB
	db.cmd('FLUSHALL')!

	// Build a very large payload (> 1 MiB). Use ~1.2 MiB to be safely above 1 MiB.
	size := 1_200_000
	mut big := []u8{len: size}
	for i in 0 .. size {
		big[i] = u8(i % 256)
	}
	// Put a few sentinel checks
	big[0] = 0
	big[1023] = `\r`
	big[1024] = `\n`
	big[size - 1] = 255

	// store and retrieve
	assert db.set('hugebin', big.clone())! == 'OK'
	got := db.get[[]u8]('hugebin')!
	assert got.len == big.len
	// spot checks
	assert got[0] == big[0]
	assert got[1023] == big[1023]
	assert got[1024] == big[1024]
	assert got[size - 1] == big[size - 1]

	// cleanup
	db.del('hugebin')!
}

// Many pipeline commands to exercise batching behavior
fn test_many_pipeline_commands() ! {
	mut db := redis.connect()!
	defer { db.close() or {} }

	// ensure clean DB
	db.cmd('FLUSHALL')!

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
			string { assert (results[set_idx] as string) == 'OK' }
			else { assert false }
		}
		// GET should return the value as []u8
		expected := ('v${i}').bytes()
		match results[get_idx] {
			[]u8 { assert (results[get_idx] as []u8) == expected }
			else { assert false }
		}
	}
}

// Larger randomized binary payloads (many iterations)
fn test_fuzz_random_binary_many() ! {
	// Larger randomized binary payloads to exercise high-memory/IO paths.
	mut db := redis.connect()!
	defer { db.close() or {} }

	db.cmd('FLUSHALL')!

	// LCG PRNG seed using process id
	seed := u64(os.getpid())
	mut x := seed

	count := 50
	for iter := 0; iter < count; iter++ {
		// size up to ~200 KiB
		size := int((x % 200_000) + 1)
		mut data := []u8{len: size}
		for i in 0 .. size {
			x = x * 6364136223846793005 + 1442695040888963407
			data[i] = u8((x >> 16) & 0xFF)
		}
		key := 'fuzz_big_${iter}'
		assert db.set(key, data.clone())! == 'OK'
		got := db.get[[]u8](key)!
		assert got.len == data.len
		// quick spot-checks for integrity
		if data.len > 3 {
			assert got[0] == data[0]
			assert got[data.len / 2] == data[data.len / 2]
			assert got[data.len - 1] == data[data.len - 1]
		}
		// clean up
		db.del(key)!
		// mix seed
		x += u64(size) * u64(iter + 1)
	}
}

// Randomized CRLF placement tests
fn test_fuzz_crlf_random_positions() ! {
	// Randomized tests that ensure CRLF sequences embedded at random locations,
	// including chunk boundaries, are handled correctly.
	mut db := redis.connect()!
	defer { db.close() or {} }

	db.cmd('FLUSHALL')!

	seed := u64(os.getpid())
	mut x := seed

	trials := 30
	for t in 0 .. trials {
		// size between 1 KiB and 32 KiB
		size := int((x % 31_744) + 1024)
		mut data := []u8{len: size}
		for i in 0 .. size {
			x = x * 6364136223846793005 + 1442695040888963407
			data[i] = u8((x >> 8) & 0xFF)
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
		assert got.len == data.len
		assert got[pos] == `\r`
		assert got[pos + 1] == `\n`
		if t % 5 == 0 && size > 4097 {
			assert got[4095] == `\r`
			assert got[4096] == `\n`
		}
		db.del(key)!
		x += u64(pos) + 1
	}
}

// CRLF crossing a read-chunk boundary
fn test_crlf_at_chunk_boundary() {
	mut db := redis.connect()!
	defer { db.close() or {} }

	// ensure clean DB
	db.cmd('FLUSHALL')!

	// The driver's read buffer reads in 4096-byte chunks. Create a payload where CRLF
	// crosses the 4096-byte boundary: place '\r' at index 4095 and '\n' at 4096.
	chunk := 4096
	size := chunk * 3 // a few chunks
	mut data := []u8{len: size}
	for i in 0 .. size {
		data[i] = u8((i * 37) % 256)
	}
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
	// Small randomized binary payloads (many repetitions).
	mut db := redis.connect()!
	defer { db.close() or {} }

	db.cmd('FLUSHALL')!

	// Simple LCG PRNG seeded from process id
	seed := u64(os.getpid())
	mut x := seed
	for iter := 0; iter < 20; iter++ {
		// pseudo-random size in [1, 4096]
		size := int((x % 4096) + 1)
		mut data := []u8{len: size}
		for i in 0 .. size {
			// LCG step
			x = x * 6364136223846793005 + 1442695040888963407
			data[i] = u8((x >> 32) & 0xFF)
		}
		k := 'fuzz_small_${iter}'
		assert db.set(k, data.clone())! == 'OK'
		got := db.get[[]u8](k)!
		assert got == data
		// mutate seed slightly so sizes vary
		x += u64(size)
	}
}
