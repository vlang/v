// https://redis.io/docs/latest/develop/reference/protocol-spec/

module redis

import math.big
import net
import net.ssl
import os
import strings

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
pub type RedisValue = string
	| i64
	| bool
	| f32
	| f64
	| []u8
	| RedisNull
	| []RedisValue
	| map[string]RedisValue
	| big.Integer
	| RedisBlobError
	| RedisVerbatim
	| RedisMap
	| RedisSet
	| RedisPush

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
	cmd_buf            []u8 // Buffer for building commands
	resp_buf           []u8 // Buffer for reading responses
	pipeline_mode      bool
	pipeline_buffer    []u8
	pipeline_cmd_count int
}

// Configuration options for Redis connection
@[params]
pub struct Config {
pub mut:
	host     string = '127.0.0.1' // Redis server host
	port     u16    = 6379        // Redis server port
	password string = os.getenv('VREDIS_PASSWORD') // Redis server password (optional)
	tls      bool // Enable TLS/SSL connection
	version  int = 3 // RESP protocol version, will be changed to 2 if RESP2 server connected
}

// connect establishes a connection to a Redis server
pub fn connect(config Config) !DB {
	mut db := DB{
		version:  config.version
		tls:      config.tls
		cmd_buf:  []u8{cap: cmd_buf_pre_allocate_len}
		resp_buf: []u8{cap: resp_buf_pre_allocate_len}
	}

	if config.tls {
		mut ssl_conn := ssl.new_ssl_conn(ssl.SSLConnectConfig{ validate: false })!
		ssl_conn.dial(config.host, int(config.port))!
		db.ssl_conn = ssl_conn
	} else {
		db.conn = net.dial_tcp('${config.host}:${config.port}')!
	}

	// Always attempt HELLO during connect() to negotiate RESP3 (and include AUTH
	// subcommand when a password is provided). If HELLO fails or the server
	// doesn't support it, fall back to RESP2 and perform AUTH via the AUTH
	// command (if a password was given).

	// build HELLO 3 command; include AUTH subcommand when password present
	db.cmd_buf.clear()
	if config.password.len > 0 {
		// *4\r\n$5\r\nHELLO\r\n$1\r\n3\r\n$4\r\nAUTH\r\n$<pwlen>\r\n<pw>\r\n
		db.cmd_buf << '*4\r\n$5\r\nHELLO\r\n$1\r\n3\r\n$4\r\nAUTH\r\n$${config.password.len}\r\n${config.password}\r\n'.bytes()
	} else {
		// *2\r\n$5\r\nHELLO\r\n$1\r\n3\r\n
		db.cmd_buf << '*2\r\n$5\r\nHELLO\r\n$1\r\n3\r\n'.bytes()
	}

	// send HELLO and attempt to read response. If any step fails, fallback to RESP2 + AUTH.
	db.write_data(db.cmd_buf) or {
		// write failed (connection error?) — fallback to RESP2 and perform AUTH if needed
		db.version = 2
		if config.password.len > 0 {
			db.auth(config.password)!
		}
		return db
	}

	// Try to read and consume HELLO response. If HELLO fails (unknown command / error),
	// treat as not-supported and fall back to RESP2 + AUTH.
	db.read_response() or {
		db.version = 2
		if config.password.len > 0 {
			db.auth(config.password)!
		}
		return db
	}

	return db
}

// close terminates the connection to Redis server
pub fn (mut db DB) close() ! {
	if db.tls {
		db.ssl_conn.close()!
	} else {
		db.conn.close()!
	}
}

// Helper methods for TLS abstraction
fn (mut db DB) write_data(data []u8) ! {
	if db.tls {
		db.ssl_conn.write(data)!
	} else {
		db.conn.write(data)!
	}
}

fn (mut db DB) read_data(mut buf []u8) !int {
	if db.tls {
		return db.ssl_conn.read(mut buf)
	} else {
		return db.conn.read(mut buf)
	}
}

fn (mut db DB) read_ptr_data(ptr &u8, len int) !int {
	if db.tls {
		return db.ssl_conn.socket_read_into_ptr(ptr, len)!
	} else {
		return db.conn.read_ptr(ptr, len)!
	}
}

// auth sends an AUTH command to the server with the given password.
pub fn (mut db DB) auth(password string) ! {
	resp := db.cmd('AUTH', password)!
	match resp {
		string {
			if resp != 'OK' {
				return error('Authentication failed: ${resp}')
			}
		}
		else {
			return error('Authentication failed: unexpected response type')
		}
	}
}

// ping sends a PING command to verify server responsiveness
pub fn (mut db DB) ping() !string {
	return db.cmd('PING')! as string
}

// del deletes a `key`
pub fn (mut db DB) del(key string) !i64 {
	// *2\r\n$3\r\nDEL\r\n$6\r\ncounter\r\n
	// send cmd
	db.cmd_buf.clear()
	db.cmd_buf << '*2\r\n$3\r\nDEL\r\n$${key.len}\r\n${key}\r\n'.bytes()
	if db.pipeline_mode {
		db.pipeline_buffer << db.cmd_buf
		db.pipeline_cmd_count++
	} else {
		db.write_data(db.cmd_buf)!

		// read resp
		return db.read_response()! as i64
	}
	return 0
}

// set stores a `key`-value` pair in Redis. Supported value types: number, string, []u8
pub fn (mut db DB) set[T](key string, value T) !string {
	// *3\r\n$3\r\nSET\r\n$4\r\nname\r\n$5\r\nVlang\r\n
	db.cmd_buf.clear()
	db.cmd_buf << '*3\r\n$3\r\nSET\r\n$${key.len}\r\n${key}\r\n'.bytes()
	$if T is $int {
		val_str := value.str()
		db.cmd_buf << '$${val_str.len}\r\n${val_str}'.bytes()
	} $else $if T is string {
		db.cmd_buf << '$${value.len}\r\n${value}'.bytes()
	} $else $if T is []u8 {
		db.cmd_buf << '$${value.len}\r\n'.bytes()
		db.cmd_buf << value
	} $else {
		return error('`set()`: unsupported value type. Allowed: number, string, []u8')
	}
	db.cmd_buf << '\r\n'.bytes()
	if db.pipeline_mode {
		db.pipeline_buffer << db.cmd_buf
		db.pipeline_cmd_count++
	} else {
		db.write_data(db.cmd_buf)!
		return db.read_response()! as string
	}
	return ''
}

// get retrieves the value of a `key`. Supported return types: string, int, []u8
pub fn (mut db DB) get[T](key string) !T {
	// *2\r\n$3\r\nGET\r\n$4\r\nname\r\n
	// send cmd
	db.cmd_buf.clear()
	db.cmd_buf << '*2\r\n$3\r\nGET\r\n$${key.len}\r\n${key}\r\n'.bytes()
	if db.pipeline_mode {
		db.pipeline_buffer << db.cmd_buf
		db.pipeline_cmd_count++
	} else {
		db.write_data(db.cmd_buf)!
		resp := db.read_response()!
		match resp {
			[]u8 {
				$if T is string {
					return resp.bytestr()
				} $else $if T is $int {
					return T(resp.bytestr().i64())
				} $else $if T is []u8 {
					return resp
				}
			}
			RedisNull {
				return error('`get()`: key ${key} not found')
			}
			else {
				return error('`get()`: unexpected response type')
			}
		}
		return error('`get()`: unsupported data type')
	}
	return T{}
}

// incr increments the integer value of a `key` by 1
pub fn (mut db DB) incr(key string) !i64 {
	// *2\r\n$4\r\nINCR\r\n$6\r\ncounter\r\n
	// send cmd
	db.cmd_buf.clear()
	db.cmd_buf << '*2\r\n$4\r\nINCR\r\n$${key.len}\r\n${key}\r\n'.bytes()
	if db.pipeline_mode {
		db.pipeline_buffer << db.cmd_buf
		db.pipeline_cmd_count++
	} else {
		db.write_data(db.cmd_buf)!

		// read resp
		return db.read_response()! as i64
	}
	return 0
}

// decr decrements the integer value of a `key` by 1
pub fn (mut db DB) decr(key string) !i64 {
	// *2\r\n$4\r\nDECR\r\n$6\r\ncounter\r\n
	// send cmd
	db.cmd_buf.clear()
	db.cmd_buf << '*2\r\n$4\r\nDECR\r\n$${key.len}\r\n${key}\r\n'.bytes()
	if db.pipeline_mode {
		db.pipeline_buffer << db.cmd_buf
		db.pipeline_cmd_count++
	} else {
		db.write_data(db.cmd_buf)!

		// read resp
		return db.read_response()! as i64
	}
	return 0
}

// hset sets multiple `key`-`value` pairs in a hash. Supported value types: string, int, []u8
pub fn (mut db DB) hset[T](key string, m map[string]T) !int {
	// HSET user:1 name "John" age 30
	// *6\r\n$4\r\nHSET\r\n$6\r\nuser:1\r\n$4\r\nname\r\n$4\r\nJohn\r\n$3\r\nage\r\n$2\r\n30\r\n
	db.cmd_buf.clear()
	db.cmd_buf << '*${2 + m.len * 2}\r\n$4\r\nHSET\r\n$${key.len}\r\n${key}\r\n'.bytes()
	for k, v in m {
		db.cmd_buf << '$${k.len}\r\n${k}\r\n'.bytes()
		$if T is string {
			db.cmd_buf << '$${v.len}\r\n${v}\r\n'.bytes()
		} $else $if T is $int {
			v_str := v.str()
			db.cmd_buf << '$${v_str.len}\r\n${v_str}\r\n'.bytes()
		} $else $if T is []u8 {
			// Write bulk string header correctly (no stray '$' after the length)
			db.cmd_buf << '$${v.len}\r\n'.bytes()
			db.cmd_buf << v
			db.cmd_buf << '\r\n'.bytes()
		} $else {
			return error('`hset()`: unsupported value type. Allowed: number, string, []u8')
		}
	}
	if db.pipeline_mode {
		db.pipeline_buffer << db.cmd_buf
		db.pipeline_cmd_count++
	} else {
		db.write_data(db.cmd_buf)!
		return int(db.read_response()! as i64)
	}
	return 0
}

// hget retrieves the value of a hash field. Supported return types: string, int, []u8
pub fn (mut db DB) hget[T](key string, m_key string) !T {
	// HGET user:1 name
	// *3\r\n$4\r\nHGET\r\n$6\r\nuser:1\r\n$4\r\nname\r\n
	db.cmd_buf.clear()
	db.cmd_buf << '*3\r\n$4\r\nHGET\r\n$${key.len}\r\n${key}\r\n'.bytes()
	db.cmd_buf << '$${m_key.len}\r\n${m_key}\r\n'.bytes()
	if db.pipeline_mode {
		db.pipeline_buffer << db.cmd_buf
		db.pipeline_cmd_count++
	} else {
		db.write_data(db.cmd_buf)!
		resp := db.read_response()! as []u8
		$if T is string {
			return resp.bytestr()
		} $else $if T is $int {
			return resp.bytestr().i64()
		} $else $if T is []u8 {
			return resp
		}
		return error('`hget()`: unsupported return type. Allowed: number, string, []u8')
	}
	return T{}
}

// hgetall retrieves all fields and values of a hash. Supported value types: string, int, []u8
pub fn (mut db DB) hgetall[T](key string) !map[string]T {
	// HGETALL user:1
	// *2\r\n$7\r\nHGETALL\r\n$6\r\nuser:1\r\n
	$if T !is string && T !is $int && T !is []u8 {
		return error('`hgetall()`: unsupported value type. Allowed: number, string, []u8')
	}
	db.cmd_buf.clear()
	db.cmd_buf << '*2\r\n$7\r\nHGETALL\r\n$${key.len}\r\n${key}\r\n'.bytes()
	if db.pipeline_mode {
		db.pipeline_buffer << db.cmd_buf
		db.pipeline_cmd_count++
	} else {
		db.write_data(db.cmd_buf)!
		resp := db.read_response()!

		// normalize result into map[string]T regardless of RESP2 array, RESP3 map,
		// or RedisMap interleaved pairs.
		$if T is string {
			mut result := map[string]T{}
			match resp {
				[]RedisValue {
					elements := resp
					if elements.len % 2 != 0 {
						return error('`hgetall()`: invalid HGETALL response format')
					}
					for i in 0 .. elements.len / 2 {
						// keys and values expected as bulk strings for RESP2
						key_val := elements[2 * i]
						val_val := elements[2 * i + 1]
						// key
						k := match key_val {
							[]u8 { (key_val as []u8).bytestr() }
							string { key_val as string }
							else { return error('`hgetall()`: unexpected key type: ${key_val.type_name()}') }
						}
						// value
						v := match val_val {
							[]u8 { (val_val as []u8).bytestr() }
							string { val_val as string }
							i64 { (val_val as i64).str() }
							else { return error('`hgetall()`: unexpected value type: ${val_val.type_name()}') }
						}
						result[k] = v
					}
					return result
				}
				map[string]RedisValue {
					for k, v in resp as map[string]RedisValue {
						val_str := match v {
							[]u8 { (v as []u8).bytestr() }
							string { v as string }
							i64 { (v as i64).str() }
							else { return error('`hgetall()`: unexpected value type in map: ${v.type_name()}') }
						}
						result[k] = val_str
					}
					return result
				}
				RedisMap {
					rm := resp as RedisMap
					pairs := rm.pairs
					if pairs.len % 2 != 0 {
						return error('`hgetall()`: invalid RedisMap response format')
					}
					for i := 0; i < pairs.len; i += 2 {
						key_val := pairs[i]
						val_val := pairs[i + 1]
						k := match key_val {
							[]u8 { (key_val as []u8).bytestr() }
							string { key_val as string }
							else { return error('`hgetall()`: unexpected key type in RedisMap: ${key_val.type_name()}') }
						}
						v := match val_val {
							[]u8 { (val_val as []u8).bytestr() }
							string { val_val as string }
							i64 { (val_val as i64).str() }
							else { return error('`hgetall()`: unexpected value type in RedisMap: ${val_val.type_name()}') }
						}
						result[k] = v
					}
					return result
				}
				else {
					return error('`hgetall()`: unsupported response type: ${resp.type_name()}')
				}
			}
		} $else $if T is $int {
			mut result := map[string]T{}
			match resp {
				[]RedisValue {
					elements := resp
					if elements.len % 2 != 0 {
						return error('`hgetall()`: invalid HGETALL response format')
					}
					for i in 0 .. elements.len / 2 {
						key_val := elements[2 * i]
						val_val := elements[2 * i + 1]
						k := match key_val {
							[]u8 { (key_val as []u8).bytestr() }
							string { key_val as string }
							else { return error('`hgetall()`: unexpected key type: ${key_val.type_name()}') }
						}
						v := match val_val {
							[]u8 { (val_val as []u8).bytestr().i64() }
							string { (val_val as string).i64() }
							i64 { val_val as i64 }
							else { return error('`hgetall()`: unexpected value type: ${val_val.type_name()}') }
						}
						result[k] = T(v)
					}
					return result
				}
				map[string]RedisValue {
					m := resp
					for k, v in m {
						n := match v {
							[]u8 { (v as []u8).bytestr().i64() }
							string { (v as string).i64() }
							i64 { v as i64 }
							else { return error('`hgetall()`: unexpected value type in map: ${v.type_name()}') }
						}
						result[k] = T(n)
					}
					return result
				}
				RedisMap {
					rm := resp as RedisMap
					pairs := rm.pairs
					if pairs.len % 2 != 0 {
						return error('`hgetall()`: invalid RedisMap response format')
					}
					for i := 0; i < pairs.len; i += 2 {
						key_val := pairs[i]
						val_val := pairs[i + 1]
						k := match key_val {
							[]u8 { (key_val as []u8).bytestr() }
							string { key_val as string }
							else { return error('`hgetall()`: unexpected key type in RedisMap: ${key_val.type_name()}') }
						}
						n := match val_val {
							[]u8 { (val_val as []u8).bytestr().i64() }
							string { (val_val as string).i64() }
							i64 { val_val as i64 }
							else { return error('`hgetall()`: unexpected value type in RedisMap: ${val_val.type_name()}') }
						}
						result[k] = T(n)
					}
					return result
				}
				else {
					return error('`hgetall()`: unsupported response type: ${resp.type_name()}')
				}
			}
		} $else $if T is []u8 {
			mut result := map[string]T{}
			match resp {
				[]RedisValue {
					elements := resp
					if elements.len % 2 != 0 {
						return error('`hgetall()`: invalid HGETALL response format')
					}
					for i in 0 .. elements.len / 2 {
						key_val := elements[2 * i]
						val_val := elements[2 * i + 1]
						k := match key_val {
							[]u8 { (key_val as []u8).bytestr() }
							string { key_val as string }
							else { return error('`hgetall()`: unexpected key type: ${key_val.type_name()}') }
						}
						v := match val_val {
							[]u8 { val_val as []u8 }
							string { (val_val as string).bytes() }
							i64 { (val_val as i64).str().bytes() }
							else { return error('`hgetall()`: unexpected value type: ${val_val.type_name()}') }
						}
						result[k] = v
					}
					return result
				}
				map[string]RedisValue {
					m := resp
					for k, v in m {
						b := match v {
							[]u8 { v as []u8 }
							string { (v as string).bytes() }
							i64 { (v as i64).str().bytes() }
							else { return error('`hgetall()`: unexpected value type in map: ${v.type_name()}') }
						}
						result[k] = b
					}
					return result
				}
				RedisMap {
					rm := resp as RedisMap
					pairs := rm.pairs
					if pairs.len % 2 != 0 {
						return error('`hgetall()`: invalid RedisMap response format')
					}
					for i := 0; i < pairs.len; i += 2 {
						key_val := pairs[i]
						val_val := pairs[i + 1]
						k := match key_val {
							[]u8 { (key_val as []u8).bytestr() }
							string { key_val as string }
							else { return error('`hgetall()`: unexpected key type in RedisMap: ${key_val.type_name()}') }
						}
						b := match val_val {
							[]u8 { val_val as []u8 }
							string { (val_val as string).bytes() }
							i64 { (val_val as i64).str().bytes() }
							else { return error('`hgetall()`: unexpected value type in RedisMap: ${val_val.type_name()}') }
						}
						result[k] = b
					}
					return result
				}
				else {
					return error('`hgetall()`: unsupported response type: ${resp.type_name()}')
				}
			}
		} $else {
			// should not happen due to compile-time check above
			return error('`hgetall()`: unsupported value type ${T.type_name()}')
		}
	}
	return map[string]T{}
}

// expire sets a `key`'s time to live in `seconds`
pub fn (mut db DB) expire(key string, seconds int) !bool {
	// *3\r\n$6\r\nEXPIRE\r\n$6\r\ncounter\r\n$3\r\n600\r\n
	// send cmd
	seconds_str := seconds.str()
	db.cmd_buf.clear()
	db.cmd_buf << '*3\r\n$6\r\nEXPIRE\r\n$${key.len}\r\n${key}\r\n'.bytes()
	db.cmd_buf << '$${seconds_str.len}\r\n${seconds_str}\r\n'.bytes()
	if db.pipeline_mode {
		db.pipeline_buffer << db.cmd_buf
		db.pipeline_cmd_count++
	} else {
		db.write_data(db.cmd_buf)!

		// read resp
		rv := db.read_response()!

		// normalize to boolean result as before
		match rv {
			i64 { return (rv as i64) != 0 }
			[]u8 { return (rv as []u8).bytestr().i64() != 0 }
			string { return (rv as string).i64() != 0 }
			else { return error('`expire()`: unexpected response type: ${rv.type_name()}') }
		}
	}
	return false
}

// read_response_bulk_string handles Redis bulk string responses (format: $<length>\r\n<data>\r\n)
fn (mut db DB) read_response_bulk_string() !RedisValue {
	mut data_length := i64(-1)
	mut chunk := []u8{len: 1}

	db.resp_buf.clear()
	for {
		bytes_read := db.read_data(mut chunk) or {
			return error('`read_response_bulk_string()`: connection error ${err}')
		}
		if bytes_read == 0 {
			return error('`read_response_bulk_string()`: connection closed prematurely')
		}
		db.resp_buf << chunk[0]

		if chunk[0] == `\n` {
			break
		}
		if (chunk[0] < `0` || chunk[0] > `9`) && chunk[0] != `\r` && chunk[0] != `-` {
			return error('`read_response_bulk_string()`: invalid bulk string header')
		}
	}

	if db.resp_buf.len < 2 {
		return error('`read_response_bulk_string()`: bulk string header too short')
	}

	data_length = db.resp_buf[0..db.resp_buf.len - 2].bytestr().i64()

	// -1 -> NULL bulk string
	if data_length == -1 {
		return RedisNull{}
	}

	// If zero-length payload, read exactly the 2-byte terminator CRLF reliably
	if data_length == 0 {
		mut term := []u8{len: 2}
		mut total_term := 0
		for total_term < 2 {
			mut ptr := unsafe { &term[total_term] }
			n := db.read_ptr_data(ptr, 2 - total_term)! // read remaining terminator bytes
			if n == 0 && total_term < 2 {
				return error('`read_response_bulk_string()`: incomplete terminator for empty string')
			}
			total_term += n
		}
		if term[0] != `\r` || term[1] != `\n` {
			return error('invalid terminator for empty string')
		}
		return []u8{}
	}

	// Read payload of exactly data_length bytes
	mut data_buf := []u8{len: int(data_length)}
	mut total_read := 0
	for total_read < data_buf.len {
		mut ptr := unsafe { &data_buf[total_read] }
		n := db.read_ptr_data(ptr, data_buf.len - total_read)!
		if n == 0 && total_read < data_buf.len {
			return error('`read_response_bulk_string()`: incomplete data: read ${total_read} / ${data_buf.len} bytes')
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
			return error('`read_response_bulk_string()`: incomplete terminator after payload')
		}
		term_read += n
	}
	if term[0] != `\r` || term[1] != `\n` {
		return error('`read_response_bulk_string()`: invalid data terminator')
	}

	return data_buf.clone()
}

// read_header reads a CRLF-terminated header (returns content without trailing CRLF)
fn (mut db DB) read_header() !string {
	mut chunk := []u8{len: 1}
	db.resp_buf.clear()
	for {
		bytes_read := db.read_data(mut chunk) or {
			return error('`read_header()`: connection error ${err}')
		}
		if bytes_read == 0 {
			return error('`read_header()`: connection closed prematurely')
		}
		db.resp_buf << chunk[0]
		if chunk[0] == `\n` {
			break
		}
	}
	if db.resp_buf.len < 2 {
		return error('`read_header()`: header too short')
	}
	return db.resp_buf[0..db.resp_buf.len - 2].bytestr()
}

// read_exact_payload reads exactly n bytes + trailing CRLF and return the data bytes (without CRLF)
fn (mut db DB) read_exact_payload(n int) ![]u8 {
	if n < 0 {
		return error('invalid payload length ${n}')
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
			return error('`read_exact_payload()`: incomplete data: read ${total_read} / ${data_buf.len} bytes')
		}
	}
	// must ending with CRLF
	if data_buf[n] != `\r` || data_buf[n + 1] != `\n` {
		return error('`read_exact_payload()`: invalid data terminator')
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
	return error('`read_resp3_boolean_payload()`: invalid boolean: ${s}')
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
	for _ in 0 .. int(count) {
		key := db.read_response()!
		val := db.read_response()!
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
				kv[(k as []u8).bytestr()] = v
			}
			string {
				kv[k as string] = v
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
	for _ in 0 .. int(count) {
		k := db.read_response()!
		v := db.read_response()!
		match k {
			[]u8 {
				kv[(k as []u8).bytestr()] = v
			}
			string {
				kv[k as string] = v
			}
			else {
				return error('`read_resp3_attr_payload()`: attribute key is not a string-like type')
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
	for _ in 0 .. int(count) {
		elems << db.read_response()!
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
	for _ in 0 .. int(count) {
		elems << db.read_response()!
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
			return error('`read_response_i64()`: incomplete data: read ${total_read} bytes')
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
			return error('`read_response_simple_string()`: incomplete data: read ${total_read} bytes')
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
			return error('`read_response_array()`: connection error: ${err}')
		}
		if bytes_read == 0 {
			return error('`read_response_array()`: connection closed prematurely')
		}
		db.resp_buf << chunk[0]

		if chunk[0] == `\n` {
			break
		}
		if (chunk[0] < `0` || chunk[0] > `9`) && chunk[0] != `\r` && chunk[0] != `-` {
			return error('`read_response_array()`: invalid array header')
		}
	}

	if db.resp_buf.len < 2 {
		return error('`read_response_array()`: array header too short')
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
		element := db.read_response() or {
			return error('`read_response_array()`: failed to read array element: ${err}')
		}
		elements << element
	}
	return elements
}

// read_response handles all types of Redis responses (RESP2 + RESP3 when enabled)
fn (mut db DB) read_response() !RedisValue {
	db.resp_buf.clear()
	unsafe { db.resp_buf.grow_len(1) }
	// Read the first non-empty, non-CR/LF prefix byte. Some transports or
	// intermediate proxies may emit stray CR/LF bytes; skip them so we parse
	// the actual RESP prefix correctly.
	for {
		read_len := db.read_data(mut db.resp_buf)!
		if read_len != 1 {
			return error('`read_response()`: empty response from server')
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
			|| ch == `|` {
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
			return error("`read_response()`: unknown response prefix byte=${prefix_val} data_hex=\"${hex}\" data_str=\"${db.resp_buf.bytestr()}\"")
		}
		// Read and discard one more byte from the socket and treat it as the new candidate.
		mut tmp := []u8{len: 1}
		n := db.read_data(mut tmp) or { 0 }
		if n == 0 {
			return error('`read_response()`: incomplete data during resynchronization')
		}
		db.resp_buf[0] = tmp[0]
		// skip CRLF if encountered but still count the attempt (we consumed a byte)
		attempts++
		continue
	}

	match db.resp_buf[0] {
		`+` { // Simple string
			return db.read_response_simple_string()!
		}
		`-` { // Error message
			msg := db.read_response_simple_string()!
			return error(msg)
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
		`#` { // Boolean
			if db.version < 3 {
				return error('`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}')
			}
			return RedisValue(db.read_resp3_boolean_payload()!)
		}
		`,` { // Double
			if db.version < 3 {
				return error('`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}')
			}
			return RedisValue(db.read_resp3_double_payload()!)
		}
		`(` { // Big number
			if db.version < 3 {
				return error('`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}')
			}
			return RedisValue(db.read_resp3_bignum_payload()!)
		}
		`!` { // Blob error
			if db.version < 3 {
				return error('`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}')
			}
			return db.read_resp3_blob_error_payload()!
		}
		`=` { // Verbatim string
			if db.version < 3 {
				return error('`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}')
			}
			return db.read_resp3_verbatim_payload()!
		}
		`%` { // Map
			if db.version < 3 {
				return error('`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}')
			}
			return db.read_resp3_map_payload()!
		}
		`~` { // Set
			if db.version < 3 {
				return error('`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}')
			}
			return RedisValue(db.read_resp3_set_payload()!)
		}
		`>` { // Push
			if db.version < 3 {
				return error('`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}')
			}
			return RedisValue(db.read_resp3_push_payload()!)
		}
		`|` { // Attr (map-like)
			if db.version < 3 {
				return error('`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}')
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
			return error("`read_response()`: unknown response prefix byte=${prefix_val} data_hex=\"${hex}\" data_str=\"${db.resp_buf.bytestr()}\"")
		}
	}
	return error('`read_response()`: unreachable code')
}

// cmd sends a custom command to Redis server
// for example: db.cmd('SET', 'key', 'value')!
pub fn (mut db DB) cmd(cmd ...string) !RedisValue {
	mut sb := strings.new_builder(cmd.len * 20)
	sb.write_string('*${cmd.len}\r\n') // Command array header
	for arg in cmd {
		sb.write_string('$${arg.len}\r\n${arg}\r\n')
	}
	if db.pipeline_mode {
		db.pipeline_buffer << unsafe { sb.reuse_as_plain_u8_array() }
		db.pipeline_cmd_count++
	} else {
		db.write_data(unsafe { sb.reuse_as_plain_u8_array() })!
		return db.read_response()!
	}
	return RedisNull{}
}

// pipeline_start start a new pipeline
pub fn (mut db DB) pipeline_start() {
	db.pipeline_mode = true
	db.pipeline_cmd_count = 0
	db.pipeline_buffer.clear()
}

// pipeline_execute executes the cmds in pipeline at once and retrieves all responses
pub fn (mut db DB) pipeline_execute() ![]RedisValue {
	if !db.pipeline_mode {
		return error('`pipeline_execute()`: pipeline not started')
	}
	defer {
		db.pipeline_mode = false
	}
	if db.pipeline_buffer.len == 0 {
		return []RedisValue{}
	}

	db.write_data(db.pipeline_buffer)!

	mut results := []RedisValue{cap: db.pipeline_cmd_count}
	for _ in 0 .. db.pipeline_cmd_count {
		results << db.read_response()!
	}

	// reset to non-pipeline mode
	db.pipeline_mode = false
	db.pipeline_cmd_count = 0
	return results
}
