// https://redis.io/docs/latest/develop/reference/protocol-spec/

module redis

import io
import math.big
import net
import net.ssl
import time

// RESP3 wrapper types
pub struct RedisBlobError {
pub:
	data []u8
}

pub struct RedisVerbatim {
pub:
	format string
	data   []u8
}

pub struct RedisMap {
pub:
	// interleaved key/value pairs: [k1, v1, k2, v2, ...]
	pairs []RedisValue
}

pub struct RedisSet {
pub:
	elements []RedisValue
}

pub struct RedisPush {
pub:
	elements []RedisValue
}

// RedisValue represents all possible RESP (Redis Serialization Protocol) data types
pub type RedisValue = bool
	| big.Integer
	| f32
	| f64
	| i64
	| []u8
	| RedisBlobError
	| RedisMap
	| RedisNull
	| RedisPush
	| RedisSet
	| map[string]RedisValue
	| []RedisValue
	| RedisVerbatim
	| string

// RedisNull represents the Redis NULL type
pub struct RedisNull {}

const cmd_buf_pre_allocate_len = 4096 // Initial buffer size for command building
const resp_buf_pre_allocate_len = 8192 // Initial buffer size for response reading
const max_skip = 64 // Max non-prefix bytes to skip when resynchronizing

// DB represents a Redis database connection
pub struct DB {
pub mut:
	version  int // RESP protocol version
	conn     &net.TcpConn = unsafe { nil } // TCP connection to Redis
	ssl_conn &ssl.SSLConn = unsafe { nil } // SSL connection to Redis
	tls      bool

	// Pre-allocated buffers to reduce memory allocations
	cmd_buf              []u8 // Buffer for building commands
	resp_buf             []u8 // Buffer for reading responses
	transaction_mode     bool
	buffered_transaction bool
	watched              bool
	closed               bool
	config               Config
	metrics              Metrics
	pipeline_mode        bool
	pipeline_buffer      []u8
	pipeline_cmd_count   int
}

// Config controls connection, authentication, timeouts, and TLS settings.
@[params]
pub struct Config {
pub mut:
	host            string = '127.0.0.1'
	port            u16    = 6379
	username        string = 'default'
	password        string
	database        int
	tls             bool
	tls_validate    bool
	tls_ca          string
	tls_cert        string
	tls_key         string
	tls_server_name string
	tls_in_memory   bool
	connect_timeout time.Duration = 5 * time.second
	read_timeout    time.Duration = 30 * time.second
	write_timeout   time.Duration = 30 * time.second
	keep_alive      bool
	auto_reconnect  bool
	max_retries     int
	retry_delay     time.Duration = 100 * time.millisecond
	trace_hook      ?fn (CommandTrace)
	version         int @[deprecated]
}

// connect establishes a connection and negotiates RESP3, with RESP2 fallback.
pub fn connect(config Config) !DB {
	if config.database < 0 || config.max_retries < 0 || config.retry_delay < 0 {
		return ConnectionError{ message: 'invalid database or retry configuration' }
	}
	mut db := DB{
		tls:      config.tls
		config:   config
		cmd_buf:  []u8{cap: cmd_buf_pre_allocate_len}
		resp_buf: []u8{cap: resp_buf_pre_allocate_len}
	}
	db.conn = dial_connection(config)!
	db.conn.set_read_timeout(config.read_timeout)
	db.conn.set_write_timeout(config.write_timeout)
	if config.keep_alive {
		db.conn.sock.set_option_bool(.keep_alive, true) or {
			db.conn.close() or {}
			return ConnectionError{ message: err.msg() }
		}
	}
	if config.tls {
		if config.connect_timeout > 0 { db.conn.set_read_timeout(config.connect_timeout) }
		// mbedtls uses a blocking BIO during its handshake; command I/O is nonblocking below.
		net.set_blocking(db.conn.sock.handle, true) or {
			db.close() or {}
			return ConnectionError{ message: err.msg() }
		}
		db.conn.is_blocking = true
		mut secure := ssl.new_ssl_conn(ssl.SSLConnectConfig{
			validate:               config.tls_validate
			verify:                 config.tls_ca
			cert:                   config.tls_cert
			cert_key:               config.tls_key
			in_memory_verification: config.tls_in_memory
		}) or {
			db.conn.close() or {}
			return ConnectionError{ message: err.msg() }
		}
		hostname := if config.tls_server_name == '' { config.host } else { config.tls_server_name }
		secure.connect(mut db.conn, hostname) or {
			secure.close() or {}
			db.conn.close() or {}
			return ConnectionError{ message: err.msg() }
		}
		secure.set_read_timeout(config.read_timeout)
		db.ssl_conn = secure
		db.conn.set_read_timeout(config.read_timeout)
		net.set_blocking(db.conn.sock.handle, false) or {
			db.close() or {}
			return ConnectionError{ message: err.msg() }
		}
		db.conn.is_blocking = false
	}
	db.negotiate() or {
		db.close() or {}
		return err
	}
	return db
}

fn (mut db DB) negotiate() ! {
	db.version = 3
	mut hello := ['HELLO', '3']
	authenticate := db.config.password != '' || db.config.username !in ['', 'default']
	if authenticate {
		hello << ['AUTH', if db.config.username == '' { 'default' } else { db.config.username },
			db.config.password]
	}
	db.write_resp_array(hello)
	db.write_data(db.cmd_buf)!
	db.read_response() or {
		if err is CommandError {
			if err.msg().starts_with('WRONGPASS') || err.msg().starts_with('NOAUTH') {
				return AuthError{ message: err.msg() }
			}
			db.version = 2
			if authenticate {
				if db.config.username == '' || db.config.username == 'default' {
					db.auth(db.config.password)!
				} else {
					db.auth_user(db.config.username, db.config.password)!
				}
			}
		} else {
			return err
		}
	}
	if db.config.database != 0 {
		db.select_db(db.config.database)!
	}
}

// close terminates the connection to Redis server
pub fn (mut db DB) close() ! {
	if db.closed { return }
	db.closed = true
	mut failure := ''
	if db.tls && unsafe { db.ssl_conn != nil } {
		db.ssl_conn.close() or { failure = err.msg() }
	}
	if unsafe { db.conn != nil } {
		db.conn.close() or { if failure == '' { failure = err.msg() } }
	}
	if failure != '' { return ConnectionError{ message: failure } }
}

fn (mut db DB) write_data(data []u8) ! {
	if db.closed { return ConnectionError{ message: 'connection is closed' } }
	if db.tls {
		db.ssl_conn.set_read_timeout(db.config.write_timeout)
		db.ssl_conn.write(data) or { return ConnectionError{ message: err.msg() } }
	} else {
		db.conn.write(data) or { return ConnectionError{ message: err.msg() } }
	}
}

fn (mut db DB) read_data(mut buf []u8) !int {
	if db.closed { return ConnectionError{ message: 'connection is closed' } }
	if db.tls {
		db.ssl_conn.set_read_timeout(db.config.read_timeout)
		return db.ssl_conn.read(mut buf) or { return ConnectionError{ message: err.msg(), eof: err is io.Eof } }
	}
	return db.conn.read(mut buf) or { return ConnectionError{ message: err.msg(), eof: err is io.Eof } }
}

fn (mut db DB) read_ptr_data(ptr &u8, len int) !int {
	if db.closed { return ConnectionError{ message: 'connection is closed' } }
	if db.tls {
		db.ssl_conn.set_read_timeout(db.config.read_timeout)
		return db.ssl_conn.socket_read_into_ptr(ptr, len) or { return ConnectionError{ message: err.msg(), eof: err is io.Eof } }
	}
	return db.conn.read_ptr(ptr, len) or { return ConnectionError{ message: err.msg(), eof: err is io.Eof } }
}

// auth sends an AUTH command to the server with the given password.
pub fn (mut db DB) auth(password string) ! {
	db.auth_user('', password)!
}

// auth_user authenticates a Redis ACL user, or the default user when username is empty.
pub fn (mut db DB) auth_user(username string, password string) ! {
	if db.pipeline_mode || db.transaction_mode || db.watched {
		return AuthError{ message: 'authentication requires an idle connection' }
	}
	mut args := ['AUTH']
	if username != '' { args << username }
	args << password
	resp := db.cmd(...args) or {
		if err is CommandError { return AuthError{ message: err.msg() } }
		return err
	}
	if resp !is string || (resp as string) != 'OK' {
		return AuthError{ message: 'unexpected authentication response' }
	}
	db.config.username = if username == '' { 'default' } else { username }
	db.config.password = password
}

// ping sends a PING command to verify server responsiveness
pub fn (mut db DB) ping() !string {
	return db.execute_string(['PING'])
}

// validate checks whether the Redis connection is still responsive.
pub fn (mut db DB) validate() !bool {
	if db.pipeline_mode || db.transaction_mode { return false }
	response := db.ping() or {
		if !db.config.auto_reconnect || db.watched { return err }
		db.reconnect()!
		return db.ping()! == 'PONG'
	}
	return response == 'PONG'
}

// reset discards transactions, removes WATCH state, and clears queued commands before pool reuse.
pub fn (mut db DB) reset() ! {
	if db.transaction_mode && !db.buffered_transaction {
		db.pipeline_mode = false
		db.discard()!
	}
	if db.watched {
		db.pipeline_mode = false
		db.transaction_mode = false
		db.unwatch()!
	}
	db.pipeline_mode = false
	db.transaction_mode = false
	db.buffered_transaction = false
	db.pipeline_cmd_count = 0
	db.cmd_buf.clear()
	db.resp_buf.clear()
	db.pipeline_buffer.clear()
}

// del deletes a `key`
pub fn (mut db DB) del(key string) !i64 {
	return db.execute_i64(['DEL', key])
}

// set stores a key-value pair in Redis. Supported value types: integer, string, []u8.
pub fn (mut db DB) set[T](key string, value T) !string {
	return db.execute_string(['SET', key, value_string(value, 'set')!])
}

// get retrieves the value of a key. Supported return types: string, integer, []u8.
pub fn (mut db DB) get[T](key string) !T {
	validate_bulk_type[T]('get')!
	resp := db.cmd('GET', key)!
	if db.pipeline_mode || db.transaction_mode {
		return T{}
	}
	if resp is RedisNull {
		return NilError{ message: '`get()`: key ${key} not found' }
	}
	return bulk_value[T](resp, 'get')
}

// incr increments the integer value of a `key` by 1
pub fn (mut db DB) incr(key string) !i64 {
	return db.execute_i64(['INCR', key])
}

// decr decrements the integer value of a `key` by 1
pub fn (mut db DB) decr(key string) !i64 {
	return db.execute_i64(['DECR', key])
}

// hset sets multiple fields in a hash. Supported value types: string, integer, []u8.
pub fn (mut db DB) hset[T](key string, m map[string]T) !int {
	validate_bulk_type[T]('hset')!
	mut args := ['HSET', key]
	for field, value in m {
		args << field
		args << value_string(value, 'hset')!
	}
	return int(db.execute_i64(args)!)
}

// hget retrieves a hash field. Supported return types: string, integer, []u8.
pub fn (mut db DB) hget[T](key string, m_key string) !T {
	validate_bulk_type[T]('hget')!
	resp := db.cmd('HGET', key, m_key)!
	if db.pipeline_mode || db.transaction_mode {
		return T{}
	}
	return bulk_value[T](resp, 'hget')
}

// hgetall retrieves all fields and values of a hash. Supported value types: string, int, []u8
pub fn (mut db DB) hgetall[T](key string) !map[string]T {
	// HGETALL user:1
	// *2\r\n$7\r\nHGETALL\r\n$6\r\nuser:1\r\n
	$if T !is string && T !is $int && T !is []u8 {
		return CommandError{ message: '`hgetall()`: unsupported value type. Allowed: number, string, []u8' }
	}
	resp := db.cmd('HGETALL', key)!
	if !db.pipeline_mode && !db.transaction_mode {

		// normalize result into map[string]T regardless of RESP2 array, RESP3 map,
		// or RedisMap interleaved pairs.
		$if T is string {
			mut result := map[string]T{}
			match resp {
				[]RedisValue {
					if resp.len % 2 != 0 {
						return CommandError{ message: '`hgetall()`: invalid HGETALL response format' }
					}
					for i in 0 .. resp.len / 2 {
						// keys and values expected as bulk strings for RESP2
						key_val := resp[2 * i]
						val_val := resp[2 * i + 1]
						// key
						k := match key_val {
							[]u8 { key_val.bytestr() }
							string { key_val }
							else {
								return CommandError{ message: '`hgetall()`: unexpected key type: ${key_val.type_name()}' }
							}
						}

						// value
						v := match val_val {
							[]u8 { val_val.bytestr() }
							string { val_val }
							i64 { val_val.str() }
							else {
								return CommandError{ message: '`hgetall()`: unexpected value type: ${val_val.type_name()}' }
							}
						}

						result[k] = v
					}
					return result
				}
				map[string]RedisValue {
					for k, v in resp {
						val_str := match v {
							[]u8 { v.bytestr() }
							string { v }
							i64 { v.str() }
							else {
								return CommandError{ message: '`hgetall()`: unexpected value type in map: ${v.type_name()}' }
							}
						}

						result[k] = val_str
					}
					return result
				}
				RedisMap {
					pairs := resp.pairs
					if pairs.len % 2 != 0 {
						return CommandError{ message: '`hgetall()`: invalid RedisMap response format' }
					}
					for i := 0; i < pairs.len; i += 2 {
						key_val := pairs[i]
						val_val := pairs[i + 1]
						k := match key_val {
							[]u8 { key_val.bytestr() }
							string { key_val }
							else {
								return CommandError{ message: '`hgetall()`: unexpected key type in RedisMap: ${key_val.type_name()}' }
							}
						}

						v := match val_val {
							[]u8 { val_val.bytestr() }
							string { val_val }
							i64 { val_val.str() }
							else {
								return CommandError{ message: '`hgetall()`: unexpected value type in RedisMap: ${val_val.type_name()}' }
							}
						}

						result[k] = v
					}
					return result
				}
				else {
					return CommandError{ message: '`hgetall()`: unsupported response type: ${resp.type_name()}' }
				}
			}
		} $else $if T is $int {
			mut result := map[string]T{}
			match resp {
				[]RedisValue {
					if resp.len % 2 != 0 {
						return CommandError{ message: '`hgetall()`: invalid HGETALL response format' }
					}
					for i in 0 .. resp.len / 2 {
						key_val := resp[2 * i]
						val_val := resp[2 * i + 1]
						k := match key_val {
							[]u8 { key_val.bytestr() }
							string { key_val }
							else {
								return CommandError{ message: '`hgetall()`: unexpected key type: ${key_val.type_name()}' }
							}
						}

						v := match val_val {
							[]u8 { val_val.bytestr().i64() }
							string { val_val.i64() }
							i64 { val_val }
							else {
								return CommandError{ message: '`hgetall()`: unexpected value type: ${val_val.type_name()}' }
							}
						}

						result[k] = T(v)
					}
					return result
				}
				map[string]RedisValue {
					for k, v in resp {
						n := match v {
							[]u8 { v.bytestr().i64() }
							string { v.i64() }
							i64 { v }
							else {
								return CommandError{ message: '`hgetall()`: unexpected value type in map: ${v.type_name()}' }
							}
						}

						result[k] = T(n)
					}
					return result
				}
				RedisMap {
					pairs := resp.pairs
					if pairs.len % 2 != 0 {
						return CommandError{ message: '`hgetall()`: invalid RedisMap response format' }
					}
					for i := 0; i < pairs.len; i += 2 {
						key_val := pairs[i]
						val_val := pairs[i + 1]
						k := match key_val {
							[]u8 { key_val.bytestr() }
							string { key_val }
							else {
								return CommandError{ message: '`hgetall()`: unexpected key type in RedisMap: ${key_val.type_name()}' }
							}
						}

						n := match val_val {
							[]u8 { val_val.bytestr().i64() }
							string { val_val.i64() }
							i64 { val_val }
							else {
								return CommandError{ message: '`hgetall()`: unexpected value type in RedisMap: ${val_val.type_name()}' }
							}
						}

						result[k] = T(n)
					}
					return result
				}
				else {
					return CommandError{ message: '`hgetall()`: unsupported response type: ${resp.type_name()}' }
				}
			}
		} $else $if T is []u8 {
			mut result := map[string]T{}
			match resp {
				[]RedisValue {
					if resp.len % 2 != 0 {
						return CommandError{ message: '`hgetall()`: invalid HGETALL response format' }
					}
					for i in 0 .. resp.len / 2 {
						key_val := resp[2 * i]
						val_val := resp[2 * i + 1]
						k := match key_val {
							[]u8 { key_val.bytestr() }
							string { key_val }
							else {
								return CommandError{ message: '`hgetall()`: unexpected key type: ${key_val.type_name()}' }
							}
						}

						v := match val_val {
							[]u8 { val_val }
							string { val_val.bytes() }
							i64 { val_val.str().bytes() }
							else {
								return CommandError{ message: '`hgetall()`: unexpected value type: ${val_val.type_name()}' }
							}
						}

						result[k] = v
					}
					return result
				}
				map[string]RedisValue {
					for k, v in resp {
						b := match v {
							[]u8 { v }
							string { v.bytes() }
							i64 { v.str().bytes() }
							else {
								return CommandError{ message: '`hgetall()`: unexpected value type in map: ${v.type_name()}' }
							}
						}

						result[k] = b
					}
					return result
				}
				RedisMap {
					pairs := resp.pairs
					if pairs.len % 2 != 0 {
						return CommandError{ message: '`hgetall()`: invalid RedisMap response format' }
					}
					for i := 0; i < pairs.len; i += 2 {
						key_val := pairs[i]
						val_val := pairs[i + 1]
						k := match key_val {
							[]u8 { key_val.bytestr() }
							string { key_val }
							else {
								return CommandError{ message: '`hgetall()`: unexpected key type in RedisMap: ${key_val.type_name()}' }
							}
						}

						b := match val_val {
							[]u8 { val_val }
							string { val_val.bytes() }
							i64 { val_val.str().bytes() }
							else {
								return CommandError{ message: '`hgetall()`: unexpected value type in RedisMap: ${val_val.type_name()}' }
							}
						}

						result[k] = b
					}
					return result
				}
				else {
					return CommandError{ message: '`hgetall()`: unsupported response type: ${resp.type_name()}' }
				}
			}
		} $else {
			// should not happen due to compile-time check above
			return CommandError{ message: '`hgetall()`: unsupported value type ${T.type_name()}' }
		}
	}
	return map[string]T{}
}

// expire sets a `key`'s time to live in `seconds`
pub fn (mut db DB) expire(key string, seconds int) !bool {
	resp := db.cmd('EXPIRE', key, seconds.str())!
	if db.pipeline_mode || db.transaction_mode {
		return false
	}
	// Normalize the response for servers that encode integer replies as strings.
	match resp {
		i64 { return resp != 0 }
		[]u8 { return resp.bytestr().i64() != 0 }
		string { return resp.i64() != 0 }
		else {
			return ProtocolError{ message: '`expire()`: unexpected response type: ${resp.type_name()}' }
		}
	}
}

// read_response_bulk_string handles Redis bulk string responses (format: $<length>\r\n<data>\r\n)
fn (mut db DB) read_response_bulk_string() !RedisValue {
	mut data_length := i64(-1)
	mut chunk := []u8{len: 1}

	db.resp_buf.clear()
	for {
		bytes_read := db.read_data(mut chunk) or {
			return ConnectionError{ message: '`read_response_bulk_string()`: connection error ${err}' }
		}
		if bytes_read == 0 {
			return ConnectionError{ message: '`read_response_bulk_string()`: connection closed prematurely' }
		}
		db.resp_buf << chunk[0]

		if chunk[0] == `\n` {
			break
		}
		if (chunk[0] < `0` || chunk[0] > `9`) && chunk[0] != `\r` && chunk[0] != `-` {
			return ProtocolError{ message: '`read_response_bulk_string()`: invalid bulk string header' }
		}
	}

	if db.resp_buf.len < 2 {
		return ProtocolError{ message: '`read_response_bulk_string()`: bulk string header too short' }
	}

	data_length = db.resp_buf[0..db.resp_buf.len - 2].bytestr().i64()

	// -1 -> NULL bulk string
	if data_length == -1 {
		return RedisNull{}
	}

	// Read payload of exactly data_length bytes
	mut data_buf := []u8{len: int(data_length)}
	mut total_read := 0
	for total_read < data_buf.len {
		mut ptr := unsafe { &data_buf[total_read] }
		n := db.read_ptr_data(ptr, data_buf.len - total_read)!
		if n == 0 && total_read < data_buf.len {
			return ProtocolError{ message: '`read_response_bulk_string()`: incomplete data: read ${total_read} / ${data_buf.len} bytes' }
		}
		total_read += n
	}

	// Now read the trailing CRLF terminator (2 bytes) reliably
	mut term := []u8{len: 2}
	mut term_read := 0
	for term_read < 2 {
		mut ptr := unsafe { &term[term_read] }
		n := db.read_ptr_data(ptr, 2 - term_read)!
		if n == 0 && term_read < 2 {
			return ProtocolError{ message: '`read_response_bulk_string()`: incomplete terminator after payload' }
		}
		term_read += n
	}
	if term[0] != `\r` || term[1] != `\n` {
		return ProtocolError{ message: '`read_response_bulk_string()`: invalid data terminator' }
	}

	return data_buf
}

// read_header reads a CRLF-terminated header (returns content without trailing CRLF)
fn (mut db DB) read_header() !string {
	mut chunk := []u8{len: 1}
	db.resp_buf.clear()
	for {
		bytes_read := db.read_data(mut chunk) or {
			return ConnectionError{ message: '`read_header()`: connection error ${err}' }
		}
		if bytes_read == 0 {
			return ConnectionError{ message: '`read_header()`: connection closed prematurely' }
		}
		db.resp_buf << chunk[0]
		if chunk[0] == `\n` {
			break
		}
	}
	if db.resp_buf.len < 2 {
		return ProtocolError{ message: '`read_header()`: header too short' }
	}
	return db.resp_buf[0..db.resp_buf.len - 2].bytestr()
}

// read_exact_payload reads exactly n bytes + trailing CRLF and return the data bytes (without CRLF)
fn (mut db DB) read_exact_payload(n int) ![]u8 {
	if n < 0 {
		return ProtocolError{ message: 'invalid payload length ${n}' }
	}
	mut data_buf := []u8{len: n + 2}
	mut total_read := 0
	for total_read < data_buf.len {
		remaining := data_buf.len - total_read
		chunk_size := if remaining > 1 { 1 } else { remaining }
		mut chunk_ptr := unsafe { &data_buf[total_read] }

		bytes_read := db.read_ptr_data(chunk_ptr, chunk_size)!
		total_read += bytes_read

		if bytes_read == 0 && total_read < data_buf.len {
			return ProtocolError{ message: '`read_exact_payload()`: incomplete data: read ${total_read} / ${data_buf.len} bytes' }
		}
	}
	// must ending with CRLF
	if data_buf[n] != `\r` || data_buf[n + 1] != `\n` {
		return ProtocolError{ message: '`read_exact_payload()`: invalid data terminator' }
	}
	return data_buf[0..n].clone()
}

// read_resp3_boolean_payload handles RESP3 boolean (#t or #f)
fn (mut db DB) read_resp3_boolean_payload() !bool {
	s := db.read_header()!
	if s == 't' {
		return true
	}
	if s == 'f' {
		return false
	}
	return ProtocolError{ message: '`read_resp3_boolean_payload()`: invalid boolean: ${s}' }
}

// read_resp3_double_payload handles RESP3 double (,<double>)
fn (mut db DB) read_resp3_double_payload() !f64 {
	s := db.read_header()!
	return s.f64()
}

// read_resp3_bignum_payload handles RESP3 big number ((<number>) -> big.Integer)
fn (mut db DB) read_resp3_bignum_payload() !big.Integer {
	mut s := db.read_header()!
	// RESP3 bignum frames may be wrapped in parentheses, e.g. "(12345)".
	// Trim leading '(' and trailing ')' if present to make the numeric string safe for the parser.
	if s.len > 0 && s[0] == `(` {
		s = s[1..]
	}
	if s.len > 0 && s[s.len - 1] == `)` {
		s = s[0..s.len - 1]
	}
	return big.integer_from_string(s)!
}

// read_resp3_blob_error_payload handles RESP3 blob error (!<len>\r\n<data>\r\n)
fn (mut db DB) read_resp3_blob_error_payload() !RedisBlobError {
	header := db.read_header()!
	length := header.i64()
	if length == -1 {
		return RedisBlobError{
			data: []u8{}
		}
	}
	payload := db.read_exact_payload(int(length))!
	return RedisBlobError{
		data: payload
	}
}

// read_resp3_verbatim_payload handles RESP3 verbatim (=<len>\r\n<fmt>:<data>\r\n)
fn (mut db DB) read_resp3_verbatim_payload() !RedisVerbatim {
	header := db.read_header()!
	length := header.i64()
	if length == -1 {
		return RedisVerbatim{
			format: ''
			data:   []u8{}
		}
	}
	payload := db.read_exact_payload(int(length))!
	// split at first ':'
	idx := payload.bytestr().index(':') or { -1 }
	if idx == -1 {
		return RedisVerbatim{
			format: ''
			data:   payload
		}
	}
	fmt := payload[0..idx].bytestr()
	data := payload[idx + 1..].clone()
	return RedisVerbatim{
		format: fmt
		data:   data
	}
}

// read_resp3_map_payload handles RESP3 map (%) where header is number of key/value pairs
// Try to return map[string]RedisValue when keys are string-like, otherwise return RedisMap
fn (mut db DB) read_resp3_map_payload() !RedisValue {
	header := db.read_header()!
	count := header.i64()
	if count == -1 {
		return RedisNull{}
	}
	if count == 0 {
		return map[string]RedisValue{}
	}
	mut pairs := []RedisValue{cap: int(count) * 2}
	for _ in 0 .. count {
		key := db.read_response_value(true)!
		val := db.read_response_value(true)!
		pairs << key
		pairs << val
	}
	// attempt to convert to map[string]RedisValue
	mut kv := map[string]RedisValue{}
	for i := 0; i < pairs.len; i += 2 {
		k := pairs[i]
		v := pairs[i + 1]
		match k {
			[]u8 {
				kv[k.bytestr()] = v
			}
			string {
				kv[k] = v
			}
			else {
				// fallback: return interleaved pairs preserved as RedisMap
				return RedisMap{
					pairs: pairs
				}
			}
		}
	}
	return kv
}

// read_resp3_attr_payload handles RESP3 attributes/attrs (|) and returns a map[string]RedisValue
// Attributes are map-like and we return a map when keys are string-like. If a non-string
// key is encountered, this treats it as an error (attributes are expected to be string-keyed).
fn (mut db DB) read_resp3_attr_payload() !RedisValue {
	header := db.read_header()!
	count := header.i64()
	if count == -1 {
		return RedisNull{}
	}
	if count == 0 {
		return map[string]RedisValue{}
	}
	mut kv := map[string]RedisValue{}
	for _ in 0 .. count {
		k := db.read_response_value(true)!
		v := db.read_response_value(true)!
		match k {
			[]u8 {
				kv[k.bytestr()] = v
			}
			string {
				kv[k] = v
			}
			else {
				return ProtocolError{ message: '`read_resp3_attr_payload()`: attribute key is not a string-like type' }
			}
		}
	}
	return kv
}

// read_resp3_set_payload handles RESP3 set (~)
fn (mut db DB) read_resp3_set_payload() !RedisSet {
	header := db.read_header()!
	count := header.i64()
	if count == -1 {
		return RedisSet{
			elements: []RedisValue{}
		}
	}
	mut elems := []RedisValue{cap: int(count)}
	for _ in 0 .. count {
		elems << db.read_response_value(true)!
	}
	return RedisSet{
		elements: elems
	}
}

// read_resp3_push_payload handles RESP3 push (>) - array-like
fn (mut db DB) read_resp3_push_payload() !RedisPush {
	header := db.read_header()!
	count := header.i64()
	if count == -1 {
		return RedisPush{
			elements: []RedisValue{}
		}
	}
	mut elems := []RedisValue{cap: int(count)}
	for _ in 0 .. count {
		elems << db.read_response_value(true)!
	}
	return RedisPush{
		elements: elems
	}
}

// read_response_i64 handles Redis integer responses (format: :<number>\r\n)
fn (mut db DB) read_response_i64() !i64 {
	db.resp_buf.clear()
	unsafe { db.resp_buf.grow_len(resp_buf_pre_allocate_len) }
	mut total_read := 0

	for total_read < db.resp_buf.len {
		remaining := db.resp_buf.len - total_read
		chunk_size := if remaining > 1 { 1 } else { remaining }
		mut chunk_ptr := unsafe { &db.resp_buf[total_read] }

		bytes_read := db.read_ptr_data(chunk_ptr, chunk_size)!
		total_read += bytes_read

		if total_read > 2 {
			if db.resp_buf[total_read - 2] == `\r` && db.resp_buf[total_read - 1] == `\n` {
				break
			}
		}
		if bytes_read == 0 {
			return ProtocolError{ message: '`read_response_i64()`: incomplete data: read ${total_read} bytes' }
		}
	}
	ret_val := db.resp_buf[0..total_read - 2].bytestr().i64()
	return ret_val
}

// read_response_simple_string handles Redis simple string responses (format: +<string>\r\n)
fn (mut db DB) read_response_simple_string() !string {
	db.resp_buf.clear()
	unsafe { db.resp_buf.grow_len(resp_buf_pre_allocate_len) }
	mut total_read := 0

	for total_read < db.resp_buf.len {
		remaining := db.resp_buf.len - total_read
		chunk_size := if remaining > 1 { 1 } else { remaining }
		mut chunk_ptr := unsafe { &db.resp_buf[total_read] }

		bytes_read := db.read_ptr_data(chunk_ptr, chunk_size)!
		total_read += bytes_read

		if total_read > 2 {
			if db.resp_buf[total_read - 2] == `\r` && db.resp_buf[total_read - 1] == `\n` {
				break
			}
		}
		if bytes_read == 0 {
			return ProtocolError{ message: '`read_response_simple_string()`: incomplete data: read ${total_read} bytes' }
		}
	}
	return db.resp_buf[0..total_read - 2].bytestr()
}

// read_response_array handles Redis array responses (format: *<length>\r\n<elements>)
fn (mut db DB) read_response_array() !RedisValue {
	mut array_len := i64(-1)
	mut chunk := []u8{len: 1}

	db.resp_buf.clear()
	for {
		bytes_read := db.read_data(mut chunk) or {
			return ConnectionError{ message: '`read_response_array()`: connection error: ${err}' }
		}
		if bytes_read == 0 {
			return ConnectionError{ message: '`read_response_array()`: connection closed prematurely' }
		}
		db.resp_buf << chunk[0]

		if chunk[0] == `\n` {
			break
		}
		if (chunk[0] < `0` || chunk[0] > `9`) && chunk[0] != `\r` && chunk[0] != `-` {
			return ProtocolError{ message: '`read_response_array()`: invalid array header' }
		}
	}

	if db.resp_buf.len < 2 {
		return ProtocolError{ message: '`read_response_array()`: array header too short' }
	}

	array_len = db.resp_buf[0..db.resp_buf.len - 2].bytestr().i64() // 排除\r\n

	if array_len == -1 {
		return RedisNull{}
	}
	if array_len == 0 {
		return []RedisValue{}
	}

	mut elements := []RedisValue{cap: int(array_len)}
	for _ in 0 .. array_len {
		element := db.read_response_value(true) or {
			return err
		}
		elements << element
	}
	return elements
}

// read_response handles all types of Redis responses (RESP2 + RESP3 when enabled)
fn (mut db DB) read_response() !RedisValue {
	return db.read_response_value(false)
}

fn (mut db DB) read_response_value(allow_error bool) !RedisValue {
	prefix := db.read_response_prefix()!
	return db.read_response_payload(prefix, allow_error)
}

fn (mut db DB) read_response_prefix() !u8 {
	db.resp_buf.clear()
	unsafe { db.resp_buf.grow_len(1) }
	// Read the first non-empty, non-CR/LF prefix byte. Some transports or
	// intermediate proxies may emit stray CR/LF bytes; skip them so we parse
	// the actual RESP prefix correctly.
	for {
		read_len := db.read_data(mut db.resp_buf)!
		if read_len != 1 {
			return ConnectionError{ message: '`read_response()`: empty response from server', eof: true }
		}
		// Skip stray CR and LF bytes that may precede the real response prefix.
		if db.resp_buf[0] == `\r` || db.resp_buf[0] == `\n` {
			continue
		}
		break
	}

	// If the first non-CRLF byte is not a valid RESP prefix, attempt a bounded
	// resynchronization: read and discard up to `max_skip` bytes looking for a
	// valid prefix. This helps tolerate transient stray bytes while avoiding
	// silently swallowing large amounts of data.
	mut attempts := 0

	for {
		// If this byte is a known RESP prefix, proceed to parse normally.
		ch := db.resp_buf[0]
		if ch == `+` || ch == `-` || ch == `:` || ch == `$` || ch == `*` || ch == `#` || ch == `,`
			|| ch == `(` || ch == `!` || ch == `=` || ch == `%` || ch == `~` || ch == `>`
			|| ch == `|` || ch == `_` {
			break
		}
		// Give up after bounded attempts and return diagnostics.
		if attempts >= max_skip {
			mut prefix_val := -1
			if db.resp_buf.len > 0 {
				prefix_val = int(db.resp_buf[0])
			}
			mut hex := ''
			for i in 0 .. db.resp_buf.len {
				hex += '${int(db.resp_buf[i]):02x} '
			}
			return ProtocolError{ message: '`read_response()`: unknown response prefix byte=${prefix_val} data_hex="${hex}" data_str="${db.resp_buf.bytestr()}"' }
		}
		// Read and discard one more byte from the socket and treat it as the new candidate.
		mut tmp := []u8{len: 1}
		n := db.read_data(mut tmp) or { 0 }
		if n == 0 {
			return ProtocolError{ message: '`read_response()`: incomplete data during resynchronization' }
		}
		db.resp_buf[0] = tmp[0]
		// skip CRLF if encountered but still count the attempt (we consumed a byte)
		attempts++
		continue
	}

	return db.resp_buf[0]
}

fn (mut db DB) read_response_payload(prefix u8, allow_error bool) !RedisValue {
	match prefix {
		`+` { // Simple string
			return db.read_response_simple_string()!
		}
		`-` { // Error message
			msg := db.read_response_simple_string()!
			if allow_error { return RedisBlobError{ data: msg.bytes() } }
			return CommandError{ message: msg }
		}
		`:` { // Integer
			return db.read_response_i64()!
		}
		`$` { // Bulk string
			return db.read_response_bulk_string()!
		}
		`*` { // Array
			return db.read_response_array()!
		}
		// RESP3-only frames (enabled when db.version >= 3)
		`_` { // Null
			if db.version < 3 {
				return ProtocolError{ message: '`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}' }
			}
			db.read_exact_payload(0)!
			return RedisNull{}
		}
		`#` { // Boolean
			if db.version < 3 {
				return ProtocolError{ message: '`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}' }
			}
			return RedisValue(db.read_resp3_boolean_payload()!)
		}
		`,` { // Double
			if db.version < 3 {
				return ProtocolError{ message: '`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}' }
			}
			return RedisValue(db.read_resp3_double_payload()!)
		}
		`(` { // Big number
			if db.version < 3 {
				return ProtocolError{ message: '`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}' }
			}
			return RedisValue(db.read_resp3_bignum_payload()!)
		}
		`!` { // Blob error
			if db.version < 3 {
				return ProtocolError{ message: '`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}' }
			}
			return db.read_resp3_blob_error_payload()!
		}
		`=` { // Verbatim string
			if db.version < 3 {
				return ProtocolError{ message: '`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}' }
			}
			return db.read_resp3_verbatim_payload()!
		}
		`%` { // Map
			if db.version < 3 {
				return ProtocolError{ message: '`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}' }
			}
			return db.read_resp3_map_payload()!
		}
		`~` { // Set
			if db.version < 3 {
				return ProtocolError{ message: '`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}' }
			}
			return RedisValue(db.read_resp3_set_payload()!)
		}
		`>` { // Push
			if db.version < 3 {
				return ProtocolError{ message: '`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}' }
			}
			return RedisValue(db.read_resp3_push_payload()!)
		}
		`|` { // Attr (map-like)
			if db.version < 3 {
				return ProtocolError{ message: '`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}' }
			}
			// Attributes are parsed like maps; reuse map parsing (attrs preserved as map[string]RedisValue)
			return db.read_resp3_map_payload()!
		}
		else {
			// Fallback: this should be unreachable because we validated prefixes above,
			// but return a helpful diagnostic if it happens.
			mut prefix_val := -1
			if db.resp_buf.len > 0 {
				prefix_val = int(db.resp_buf[0])
			}
			mut hex := ''
			for i in 0 .. db.resp_buf.len {
				hex += '${int(db.resp_buf[i]):02x} '
			}
			return ProtocolError{ message: '`read_response()`: unknown response prefix byte=${prefix_val} data_hex="${hex}" data_str="${db.resp_buf.bytestr()}"' }
		}
	}

	return ProtocolError{ message: '`read_response()`: unreachable code' }
}

// write_resp_array serializes command arguments as binary-safe RESP bulk strings.
fn (mut db DB) write_resp_array(args []string) {
	db.cmd_buf.clear()
	db.cmd_buf << '*${args.len}\r\n'.bytes()
	for arg in args {
		db.cmd_buf << '\$${arg.len}\r\n'.bytes()
		db.cmd_buf << arg.bytes()
		db.cmd_buf << '\r\n'.bytes()
	}
}

// cmd sends a custom command to Redis server.
// For example: db.cmd('SET', 'key', 'value')!
pub fn (mut db DB) cmd(cmd ...string) !RedisValue {
	if cmd.len == 0 { return CommandError{ message: 'command must not be empty' } }
	db.write_resp_array(cmd)
	if db.pipeline_mode {
		db.pipeline_buffer << db.cmd_buf
		db.pipeline_cmd_count++
		return RedisNull{}
	}
	started := time.now()
	if db.closed && db.config.auto_reconnect && !db.transaction_mode && !db.watched {
		db.reconnect()!
		db.write_resp_array(cmd)
	}
	mut attempts := 0
	for {
		db.write_data(db.cmd_buf) or {
			sent := if db.tls { db.ssl_conn.last_write_sent } else { db.conn.last_write_sent }
			if db.config.auto_reconnect && !db.transaction_mode && !db.watched && sent == 0 && attempts < db.config.max_retries {
				attempts++
				time.sleep(db.config.retry_delay)
				db.reconnect()!
				db.write_resp_array(cmd)
				continue
			}
			db.record_command(cmd[0], started, true)
			db.close() or {}
			return err
		}
		break
	}
	resp := db.read_response() or {
		db.record_command(cmd[0], started, true)
		if err is ConnectionError || err is ProtocolError { db.close() or {} }
		return err
	}
	if resp is RedisBlobError {
		db.record_command(cmd[0], started, true)
		return CommandError{ message: resp.data.bytestr() }
	}
	db.record_command(cmd[0], started, false)
	return resp
}

// pipeline_start starts a buffered command pipeline.
pub fn (mut db DB) pipeline_start() {
	db.pipeline_mode = true
	db.pipeline_cmd_count = 0
	db.pipeline_buffer.clear()
}

// pipeline_execute sends queued commands and retrieves all replies, including command error values.
pub fn (mut db DB) pipeline_execute() ![]RedisValue {
	if !db.pipeline_mode {
		return CommandError{ message: '`pipeline_execute()`: pipeline not started' }
	}
	defer {
		db.pipeline_mode = false
		db.pipeline_cmd_count = 0
		db.pipeline_buffer.clear()
	}
	if db.pipeline_buffer.len == 0 {
		return []RedisValue{}
	}

	started := time.now()
	db.write_data(db.pipeline_buffer) or {
		db.close() or {}
		return err
	}

	mut results := []RedisValue{cap: db.pipeline_cmd_count}
	for _ in 0 .. db.pipeline_cmd_count {
		results << db.read_response_value(true) or {
			db.close() or {}
			return err
		}
	}

	db.record_pipeline(results, started)
	// reset to non-pipeline mode
	db.pipeline_mode = false
	db.pipeline_cmd_count = 0
	return results
}
