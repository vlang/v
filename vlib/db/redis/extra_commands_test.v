// vtest build: started_redis?
import db.redis
import math
import net
import os
import time

fn extra_db(version int) !redis.DB {
	mut db := redis.connect(password: os.getenv('VREDIS_PASSWORD'))!
	db.hello(version)!
	return db
}

fn assert_extra_integer(value ?i64, expected i64) {
	actual := value or { panic('expected an integer') }
	assert actual == expected
}

fn assert_extra_missing_integer(value ?i64) {
	if actual := value {
		panic('expected none, got ${actual}')
	}
}

fn test_bitmap_and_hyperloglog_commands() {
	for version in [2, 3] {
		mut db := extra_db(version)!
		prefix := 'v:extra:${os.getpid()}:${version}:bits:'
		keys := [prefix + 'one', prefix + 'two', prefix + 'result', prefix + 'fields', prefix + 'hll1',
			prefix + 'hll2', prefix + 'hll3']
		defer {
			db.unlink(...keys) or {}
			db.close() or {}
		}
		assert !db.setbit(keys[0], 0, true)!
		assert !db.setbit(keys[0], 7, true)!
		assert db.getbit(keys[0], 0)!
		assert !db.getbit(keys[0], 1)!
		assert db.bitcount(keys[0])! == 2
		assert db.bitcount(keys[0], start: 0, end: 6, unit: .bit)! == 1
		assert db.bitpos(keys[0], true)! == 0
		assert db.bitpos(keys[0], false, start: 1, end: 7, unit: .bit)! == 1
		assert !db.setbit(keys[1], 1, true)!
		assert db.bitop(.or, keys[2], keys[0], keys[1])! == 1
		assert db.bitcount(keys[2])! == 3
		assert db.bitop(.not, keys[2], keys[0])! == 1
		assert db.bitcount(keys[2])! == 6
		operations := db.bitfield(keys[3], redis.BitFieldOperation{
			kind:     .set
			encoding: 'u4'
			offset:   '0'
			value:    15
		}, redis.BitFieldOperation{
			kind:     .incrby
			encoding: 'u4'
			offset:   '0'
			value:    1
			overflow: .fail
		}, redis.BitFieldOperation{
			kind:     .get
			encoding: 'u4'
			offset:   '0'
		})!
		assert operations.len == 3
		assert_extra_integer(operations[0], 0)
		assert_extra_missing_integer(operations[1])
		assert_extra_integer(operations[2], 15)
		read := db.bitfield_ro(keys[3], redis.BitFieldOperation{ encoding: 'u4', offset: '#0' })!
		assert read.len == 1
		assert_extra_integer(read[0], 15)
		mut rejected_write := false
		db.bitfield_ro(keys[3], redis.BitFieldOperation{ kind: .set, encoding: 'u4', offset: '0' }) or {
			rejected_write = true
			assert err is redis.CommandError
			assert err.msg().contains('only GET')
		}
		assert rejected_write
		assert db.pfadd(keys[4], 'one', 'two')!
		assert !db.pfadd(keys[4], 'one')!
		assert db.pfadd(keys[5], 'two', 'three')!
		assert db.pfcount(keys[4], keys[5])! == 3
		assert db.pfmerge(keys[6], keys[4], keys[5])! == 'OK'
		assert db.pfcount(keys[6])! == 3
		db.pipeline_start()
		db.getbit(keys[0], 0)!
		db.bitfield_ro(keys[3], redis.BitFieldOperation{ encoding: 'u4', offset: '0' })!
		db.pfcount(keys[6])!
		results := db.pipeline_execute()!
		assert results.len == 3
		assert results[0] as i64 == 1
		assert results[2] as i64 == 3
	}
}

fn test_geospatial_commands() {
	for version in [2, 3] {
		mut db := extra_db(version)!
		key := 'v:extra:${os.getpid()}:${version}:geo'
		destination := key + ':stored'
		defer {
			db.unlink(key, destination) or {}
			db.close() or {}
		}
		assert db.geoadd(key, [
			redis.GeoMember{ name: 'Palermo', longitude: 13.361389, latitude: 38.115556 },
			redis.GeoMember{ name: 'Catania', longitude: 15.087269, latitude: 37.502669 },
		])! == 2
		assert db.geoadd(key,
			[redis.GeoMember{ name: 'Palermo', longitude: 13.361389, latitude: 38.115556 }],
			mode: .nx
		)! == 0
		assert math.abs(db.geodist(key, 'Palermo', 'Catania', .km)! - 166.2742) < 0.01
		hashes := db.geohash(key, 'Palermo', 'missing')!
		assert hashes.len == 2
		assert (hashes[0] or { '' }) == 'sqc8b49rny0'
		if hash := hashes[1] {
			panic('unexpected hash: ${hash}')
		}
		positions := db.geopos(key, 'Palermo', 'missing')!
		assert positions.len == 2
		position := positions[0] or { panic('missing position') }
		assert math.abs(position.longitude - 13.361389) < 0.00001
		assert math.abs(position.latitude - 38.115556) < 0.00001
		if unexpected := positions[1] {
			panic('unexpected position: ${unexpected}')
		}
		results := db.geosearch(key,
			from_member: 'Palermo'
			radius:      200
			unit:        .km
			order:       .asc
			with_dist:   true
			with_hash:   true
			with_coord:  true
		)!
		assert results.len == 2
		assert results[0].member == 'Palermo'
		assert results[1].member == 'Catania'
		assert math.abs((results[1].distance or { 0.0 }) - 166.2742) < 0.01
		assert (results[0].hash or { i64(0) }) > 0
		assert (results[0].position or { redis.GeoPosition{} }).longitude > 13
		box := db.geosearch(key,
			longitude: 15
			latitude:  37
			width:     400
			height:    400
			unit:      .km
			count:     1
			any:       true
		)!
		assert box.len == 1
		assert db.geosearchstore(destination, key, redis.GeoSearchOptions{ from_member: 'Palermo', radius: 200, unit: .km }, true)! == 2
		assert db.key_type(destination)! == 'zset'
		legacy := db.georadius(key,
			longitude: 13.361389
			latitude:  38.115556
			radius:    200
			unit:      .km
			order:     .asc
			with_dist: true
		)!
		assert legacy.len == 2
		assert legacy[0].member == 'Palermo'
		legacy_member := db.georadiusbymember(key,
			from_member: 'Palermo'
			radius:      200
			unit:        .km
			order:       .asc
			with_coord:  true
		)!
		assert legacy_member.len == 2
		assert legacy_member[0].member == 'Palermo'
		mut rejected_shape := false
		db.geosearch(key, radius: 1, width: 1, height: 1) or {
			rejected_shape = true
			assert err is redis.CommandError
			assert err.msg().contains('requires a positive radius')
		}
		assert rejected_shape
		mut missing := false
		db.geodist(key, 'Palermo', 'missing', .km) or {
			missing = true
			assert err is redis.NilError
		}
		assert missing
	}
}

fn test_scripting_and_function_commands() {
	for version in [2, 3] {
		mut db := extra_db(version)!
		key := 'v:extra:${os.getpid()}:${version}:script'
		library := 'v_extra_${os.getpid()}_${version}'
		function := library + '_echo'
		defer {
			db.function_delete(library) or {}
			db.unlink(key) or {}
			db.close() or {}
		}
		script := 'return {KEYS[1], ARGV[1], false}'
		sha := db.script_load(script)!
		assert sha.len == 40
		assert db.script_exists(sha, '0000000000000000000000000000000000000000')! == [
			true,
			false,
		]
		for result in [db.eval(script, [key], 'a\x00\r\nb')!, db.evalsha(sha, [key], 'a\x00\r\nb')!,
			db.eval_ro(script, [key], 'a\x00\r\nb')!, db.evalsha_ro(sha, [key], 'a\x00\r\nb')!] {
			values := result as []redis.RedisValue
			assert values.len == 3
			assert (values[0] as []u8).bytestr() == key
			assert (values[1] as []u8).bytestr() == 'a\x00\r\nb'
			assert values[2] is redis.RedisNull
		}
		code := '#!lua name=${library}\nredis.register_function{function_name="${function}", callback=function(keys,args) return args[1] end, flags={"no-writes"}}'
		assert db.function_load(code, false)! == library
		assert (db.fcall(function, []string{}, 'hello')! as []u8).bytestr() == 'hello'
		assert (db.fcall_ro(function, []string{}, 'hello')! as []u8).bytestr() == 'hello'
		libraries := db.function_list('LIBRARYNAME', library, 'WITHCODE')!
		assert (libraries as []redis.RedisValue).len == 1
		assert db.function_dump()!.len > 0
		assert db.function_stats()! !is redis.RedisNull
		db.pipeline_start()
		db.eval('return 42', []string{})!
		db.script_exists(sha)!
		pipeline := db.pipeline_execute()!
		assert pipeline.len == 2
		assert pipeline[0] as i64 == 42
	}
}

fn test_server_admin_and_acl_commands() {
	for version in [2, 3] {
		mut db := extra_db(version)!
		key := 'v:extra:${os.getpid()}:${version}:server'
		user := 'v_extra_${os.getpid()}_${version}'
		defer {
			db.acl_deluser(user) or {}
			db.unlink(key) or {}
			db.close() or {}
		}
		assert db.info('server')!.contains('redis_version:')
		config := db.config_get('maxmemory')!
		assert 'maxmemory' in config
		assert db.client_id()! > 0
		assert db.client_setname(user)! == 'OK'
		assert db.client_getname()! == user
		assert db.client_list()!.contains('name=${user}')
		assert db.client_no_touch(true)! == 'OK'
		assert db.client_no_touch(false)! == 'OK'
		assert db.client_reply('ON')! == 'OK'
		assert db.role()! !is redis.RedisNull
		assert db.set(key, 'value')! == 'OK'
		assert db.dbsize()! > 0
		server_time := db.server_time()!
		assert server_time.seconds > 0
		assert server_time.microseconds >= 0 && server_time.microseconds < 1_000_000
		assert db.lastsave()! > 0
		assert db.memory_usage(key)! > 0
		assert db.memory_stats()! !is redis.RedisNull
		assert db.slowlog_len()! >= 0
		assert db.slowlog_get(1)! !is redis.RedisNull
		who := db.acl_whoami()!
		assert who in db.acl_users()!
		assert db.acl_list()!.len > 0
		assert 'read' in db.acl_cat()!
		assert db.acl_cat('string')!.len > 0
		assert db.acl_setuser(user, 'off', '+get', '~${key}')! == 'OK'
		assert db.acl_getuser(user)! !is redis.RedisNull
		assert db.acl_dryrun(user, 'GET', key)! == 'OK'
		assert db.acl_genpass(128)!.len == 32
		assert db.acl_log('1')! !is redis.RedisNull
		assert db.command_count()! > 0
		assert db.command_info('GET')! !is redis.RedisNull
		assert db.command_getkeys('MGET', key, key + ':other')! == [key, key + ':other']
		db.pipeline_start()
		db.config_get('maxmemory')!
		db.server_time()!
		db.acl_users()!
		assert db.pipeline_execute()!.len == 3
	}
}

fn shutdown_peer(mut connection net.TcpConn, response string, delay time.Duration) {
	defer { connection.close() or {} }
	mut command := []u8{len: 4096}
	connection.read(mut command) or { return }
	if delay > 0 {
		time.sleep(delay)
	}
	if response.len > 0 {
		connection.write_string(response) or {}
	}
}

fn test_shutdown_distinguishes_clean_close_error_and_timeout() {
	for response in ['', '-ERR shutdown denied\r\n', ':1\r\n', 'timeout', '-ERR shutdown denied',
		'-', '+', ':', '$', '*1\r\n', '*1\r\n-ERR denied'] {
		mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
		mut client := net.dial_tcp(listener.addr()!.str())!
		client.set_read_timeout(if response == 'timeout' {
			20 * time.millisecond
		} else {
			time.second
		})
		mut server := listener.accept()!
		listener.close()!
		mut db := redis.DB{ conn: client, version: 2 }
		defer { db.close() or {} }
		delay := if response == 'timeout' { 100 * time.millisecond } else { time.Duration(0) }
		peer := spawn shutdown_peer(mut server, if response == 'timeout' { '' } else { response }, delay)
		mut failed := false
		db.shutdown('NOSAVE') or {
			failed = true
			if response == 'timeout' || !response.ends_with('\r\n') || response == '*1\r\n' {
				assert err is redis.ConnectionError
				assert db.closed
			} else if response == ':1\r\n' {
				assert err is redis.ProtocolError
				assert db.closed
			} else {
				assert err is redis.CommandError, '${response}: ${err}'
				assert err.msg().contains('shutdown denied')
				assert !db.closed
			}
		}
		assert failed == (response != '')
		peer.wait()
		if response == '' {
			mut closed := false
			db.ping() or {
				closed = true
				assert err is redis.ConnectionError
			}
			assert closed
		}
	}
}

fn test_client_reply_no_reply_modes_use_dedicated_connections() {
	for version in [2, 3] {
		for mode in ['OFF', 'SKIP'] {
			mut db := extra_db(version)!
			defer { db.close() or {} }
			assert db.client_reply(mode)! == ''
			db.conn.write_string('*1\r\n$4\r\nPING\r\n')!
			assert db.client_reply('ON')! == 'OK'
			assert db.ping()! == 'PONG'
		}
	}
}

fn test_hello_switches_decoder_before_reading_new_protocol() {
	mut db := extra_db(3)!
	defer { db.close() or {} }
	for version in [2, 3, 2, 3] {
		response := db.hello(version)!
		assert response !is redis.RedisNull
		assert db.version == version
		assert db.ping()! == 'PONG'
	}
	db.hello(2)!
	mut rejected := false
	db.hello(3, 'UNKNOWN') or {
		rejected = true
		assert err is redis.CommandError
	}
	assert rejected
	assert db.version == 2
	assert db.ping()! == 'PONG'
	mut reconnectable := redis.connect(password: os.getenv('VREDIS_PASSWORD'), auto_reconnect: true)!
	defer { reconnectable.close() or {} }
	for version in [2, 3] {
		reconnectable.close()!
		reconnectable.hello(version)!
		assert reconnectable.version == version
		assert reconnectable.ping()! == 'PONG'
	}
}
