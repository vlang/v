// vtest build: started_redis?
module redis

import net
import os
import rand
import time

fn test_hash_slots_and_endpoint_parsing() {
	assert hash_slot('123456789') == 12739
	assert hash_slot('foo') == 12182
	assert hash_slot('{user}:one') == hash_slot('{user}:two')
	assert hash_slot('foo{}{bar}') != hash_slot('bar')
	assert hash_slot('a{') != hash_slot('a')
	config := endpoint_config('[::1]:6380', Config{})!
	assert config.host == '::1'
	assert config.port == 6380
	assert endpoint(config) == '[::1]:6380'
	assert endpoint_config(':6381', Config{ host: 'localhost' })!.host == 'localhost'
	endpoint_config('host:65536', Config{}) or {
		assert err is ProtocolError
		return
	}
	assert false
}

fn test_connect_reconnect_database_and_metrics() {
	key := 'v:connection:${rand.ulid()}'
	mut db := connect(
		password:       os.getenv('VREDIS_PASSWORD')
		database:       12
		auto_reconnect: true
		read_timeout:   time.second
		write_timeout:  time.second
		keep_alive:     true
	)!
	defer { db.close() or {} }
	assert db.conn.read_timeout() == time.second
	assert db.conn.write_timeout() == time.second
	assert db.set(key, 'selected database')! == 'OK'
	id := db.cmd('CLIENT', 'ID')! as i64
	db.close()!
	assert db.validate()!
	assert db.cmd('CLIENT', 'ID')! as i64 != id
	assert db.get[string](key)! == 'selected database'
	assert db.statistics().reconnects == 1
	assert db.statistics().commands >= 4
	assert db.select_db(0)! == 'OK'
	mut missing := false
	db.get[string](key) or {
		missing = true
		assert err is NilError
	}
	assert missing
	assert db.select_db(12)! == 'OK'
	assert db.del(key)! == 1
	assert db.statistics().failures == 0 // A missing value is decoded after successful transport.
	mut abstraction := Redis(db)
	assert abstraction.ping()! == 'PONG'
}

fn test_reset_clears_server_transaction_and_watch_state() {
	key := 'v:reset:${rand.ulid()}'
	mut db := connect(password: os.getenv('VREDIS_PASSWORD'))!
	mut other := connect(password: os.getenv('VREDIS_PASSWORD'))!
	defer {
		db.close() or {}
		other.close() or {}
	}
	assert db.watch(key)! == 'OK'
	db.reset()!
	other.set(key, 'changed')!
	db.multi()!
	db.set(key, 'committed')!
	assert db.exec()!.len == 1
	db.multi()!
	db.set(key, 'discarded')!
	db.reset()!
	assert db.get[string](key)! == 'committed'
	db.del(key)!
}

fn test_async_commands_keep_connections_independent() {
	client := async_client(password: os.getenv('VREDIS_PASSWORD'))
	one := client.cmd_async('PING')
	two := client.cmd_async('PING')
	for channel in [one, two] {
		result := <-channel
		if failure := result.err {
			assert false, failure.msg()
		}
		assert result.value as string == 'PONG'
	}
	bad := client.cmd_async('V_UNKNOWN_COMMAND')
	result := <-bad
	if failure := result.err {
		assert failure is CommandError
	} else {
		assert false
	}
}

fn stalled_handshake(mut listener net.TcpListener, ready chan bool) {
	mut server := listener.accept() or { return }
	ready <- true
	time.sleep(100 * time.millisecond)
	server.close() or {}
	listener.close() or {}
}

fn test_handshake_timeout_is_a_connection_error() {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	address := listener.addr()!
	ready := chan bool{cap: 1}
	worker := spawn stalled_handshake(mut listener, ready)
	started := time.now()
	connect(host: '127.0.0.1', port: address.port()!, read_timeout: 10 * time.millisecond) or {
		assert err is ConnectionError
		assert time.now() - started < time.second
		worker.wait()
		return
	}
	worker.wait()
	assert false, 'handshake succeeded despite a stalled server'
}

fn backpressure_server(mut listener net.TcpListener) {
	mut socket := listener.accept() or { return }
	mut server := DB{ version: 3, conn: socket }
	defer {
		server.close() or {}
		listener.close() or {}
	}
	server.read_response() or { return }
	server.write_data('%0\r\n'.bytes()) or { return }
	time.sleep(500 * time.millisecond)
}

fn test_write_timeout_and_unlimited_connect_use_nonblocking_transport() {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := listener.addr()!.port()!
	worker := spawn backpressure_server(mut listener)
	mut db := connect(
		host:            '127.0.0.1'
		port:            port
		connect_timeout: 0
		write_timeout:   20 * time.millisecond
	)!
	defer { db.close() or {} }
	assert !db.conn.is_blocking
	db.conn.sock.set_option_int(.send_buf_size, 4096)!
	started := time.now()
	mut timed_out := false
	db.set('large', 'x'.repeat(8 * 1024 * 1024)) or {
		timed_out = true
		assert err is ConnectionError
		assert db.closed
	}
	assert timed_out
	assert time.now() - started < 400 * time.millisecond
	worker.wait()
}

fn test_authentication_credentials_survive_reconnect() {
	user := 'v-user-${rand.ulid()}'
	password := 'pw-${rand.ulid()}'
	mut admin := connect(password: os.getenv('VREDIS_PASSWORD'))!
	defer {
		admin.acl_deluser(user) or {}
		admin.close() or {}
	}
	admin.acl_setuser(user, 'on', '>${password}', '~*', '+@all')!
	mut db := connect(password: os.getenv('VREDIS_PASSWORD'), auto_reconnect: true)!
	defer { db.close() or {} }
	db.auth_user(user, password)!
	assert db.config.username == user
	assert db.config.password == password
	db.close()!
	assert db.validate()!
	assert db.acl_whoami()! == user
	db.pipeline_start()
	mut rejected := false
	db.auth_user(user, password) or {
		rejected = true
		assert err is AuthError
	}
	assert rejected
	assert db.pipeline_cmd_count == 0
	db.reset()!
}

fn test_named_nopass_user_authentication_survives_reconnect() {
	user := 'v-nopass-${rand.ulid()}'
	mut admin := connect(password: os.getenv('VREDIS_PASSWORD'))!
	defer {
		admin.acl_deluser(user) or {}
		admin.close() or {}
	}
	admin.acl_setuser(user, 'on', 'nopass', '~*', '+@all')!
	mut db := connect(username: user, auto_reconnect: true)!
	defer { db.close() or {} }
	assert db.acl_whoami()! == user
	db.reconnect()!
	assert db.acl_whoami()! == user
	db.close()!
	assert db.ping()! == 'PONG'
	assert db.acl_whoami()! == user
}

fn test_hello_authentication_credentials_survive_reconnect() {
	for password in ['pw-${rand.ulid()}', ''] {
		user := 'v-hello-${rand.ulid()}'
		mut admin := connect(password: os.getenv('VREDIS_PASSWORD'))!
		defer {
			admin.acl_deluser(user) or {}
			admin.close() or {}
		}
		admin.acl_setuser(user, 'on', if password == '' { 'nopass' } else { '>${password}' },
			'~*', '+@all')!
		for version in [2, 3] {
			mut db := connect(password: os.getenv('VREDIS_PASSWORD'), auto_reconnect: true)!
			defer { db.close() or {} }
			db.hello(version, 'SETNAME', 'AUTH', 'auth', user, password, 'SETNAME', 'AUTH')!
			assert db.config.username == user
			assert db.config.password == password
			assert db.acl_whoami()! == user
			db.reconnect()!
			assert db.acl_whoami()! == user
			db.close()!
			assert db.ping()! == 'PONG'
			assert db.acl_whoami()! == user
			mut rejected := false
			db.hello(version, 'AUTH', 'v-missing-${rand.ulid()}', 'wrong-password') or {
				rejected = true
				assert err is CommandError
			}
			assert rejected
			assert db.config.username == user
			assert db.config.password == password
			assert db.acl_whoami()! == user
		}
	}
}

fn test_metrics_and_trace_count_pipeline_replies() {
	traces := chan CommandTrace{cap: 4}
	mut db := connect(
		password:   os.getenv('VREDIS_PASSWORD')
		trace_hook: fn [traces] (event CommandTrace) {
			traces <- event
		}
	)!
	defer { db.close() or {} }
	assert db.ping()! == 'PONG'
	ping := <-traces
	assert ping.command == 'PING'
	assert !ping.failed
	db.pipeline_start()
	db.ping()!
	db.cmd('V_UNKNOWN_COMMAND')!
	results := db.pipeline_execute()!
	assert results.len == 2
	assert results[1] is RedisBlobError
	batch := <-traces
	assert batch.command == 'PIPELINE'
	assert batch.failed
	statistics := db.statistics()
	assert statistics.commands == 3
	assert statistics.failures == 1
	assert statistics.duration >= 0
}

fn resp2_auth_fallback_server(mut listener net.TcpListener, username string, password string) {
	mut socket := listener.accept() or { panic(err) }
	socket.set_read_timeout(2 * time.second)
	socket.set_write_timeout(2 * time.second)
	mut server := DB{ version: 2, conn: socket }
	defer { server.close() or {} }
	hello := string_values(server.read_response() or { panic(err) }, 'hello') or { panic(err) }
	assert hello == ['HELLO', '3', 'AUTH', if username == '' { 'default' } else { username }, password]
	server.write_data("-ERR unknown command 'HELLO'\r\n".bytes()) or { panic(err) }
	auth := string_values(server.read_response() or { panic(err) }, 'auth') or { panic(err) }
	// Redis before ACL support accepts AUTH password, without an explicit username.
	if username == '' || username == 'default' {
		assert auth == ['AUTH', password]
	} else {
		assert auth == ['AUTH', username, password]
	}
	server.write_data('+OK\r\n'.bytes()) or { panic(err) }
	ping := string_values(server.read_response() or { panic(err) }, 'ping') or { panic(err) }
	assert ping == ['PING']
	server.write_data('+PONG\r\n'.bytes()) or { panic(err) }
}

fn test_resp2_fallback_preserves_authentication() {
	for config in [Config{ password: 'fallback-password' },
		Config{ username: '', password: 'fallback-password' }, Config{ username: 'app' }] {
		mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
		listener.set_accept_timeout(2 * time.second)
		defer { listener.close() or {} }
		port := listener.addr()!.port()!
		worker := spawn resp2_auth_fallback_server(mut listener, config.username, config.password)
		mut db := connect(
			host:          '127.0.0.1'
			port:          port
			username:      config.username
			password:      config.password
			read_timeout:  2 * time.second
			write_timeout: 2 * time.second
		)!
		defer { db.close() or {} }
		assert db.version == 2
		assert db.config.username == if config.username == '' { 'default' } else { config.username }
		assert db.ping()! == 'PONG'
		worker.wait()
	}
}
