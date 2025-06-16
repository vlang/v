// https://redis.io/docs/latest/develop/reference/protocol-spec/

module redis

import net
import strings

// RedisValue represents all possible RESP (Redis Serialization Protocol) data types
pub type RedisValue = string | i64 | bool | f32 | f64 | []u8 | RedisNull | []RedisValue

// RedisNull represents the Redis NULL type
pub struct RedisNull {}

const cmd_buf_pre_allocate_len = 4096 // Initial buffer size for command building
const resp_buf_pre_allocate_len = 8192 // Initial buffer size for response reading

// DB represents a Redis database connection
pub struct DB {
pub mut:
	version int // RESP protocol version
mut:
	conn &net.TcpConn = unsafe { nil } // TCP connection to Redis

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
	host    string = '127.0.0.1' // Redis server host
	port    u16    = 6379        // Redis server port
	version int    = 2           // RESP protocol version (default: v2)
}

// connect establishes a connection to a Redis server
pub fn connect(config Config) !DB {
	conn := net.dial_tcp('${config.host}:${config.port}')!
	return DB{
		conn:     conn
		version:  config.version
		cmd_buf:  []u8{cap: cmd_buf_pre_allocate_len}
		resp_buf: []u8{cap: resp_buf_pre_allocate_len}
	}
}

// close terminates the connection to Redis server
pub fn (mut db DB) close() ! {
	db.conn.close()!
}

// ping sends a PING command to verify server responsiveness
pub fn (mut db DB) ping() !string {
	db.conn.write_string('*1\r\n$4\r\nPING\r\n')!
	return db.read_response()! as string
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
		db.conn.write(db.cmd_buf)!

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
		db.conn.write(db.cmd_buf)!
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
		db.conn.write(db.cmd_buf)!
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
		db.conn.write(db.cmd_buf)!

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
		db.conn.write(db.cmd_buf)!

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
			db.cmd_buf << '$${v.len}\r\n$'.bytes()
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
		db.conn.write(db.cmd_buf)!
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
		db.conn.write(db.cmd_buf)!
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
		db.conn.write(db.cmd_buf)!
		resp := db.read_response()!
		match resp {
			[]RedisValue {
				mut result := map[string]T{}
				elements := resp

				if elements.len % 2 != 0 {
					return error('`hgetall()`: invalid HGETALL response format')
				}

				for i in 0 .. elements.len / 2 {
					key_resp := elements[2 * i] as []u8
					val_resp := elements[2 * i + 1] as []u8

					k := key_resp.bytestr()

					$if T is string {
						result[k] = val_resp.bytestr()
					} $else $if T is $int {
						result[k] = val_resp.bytestr().i64()
					} $else $if T is []u8 {
						result[k] = val_resp
					} $else {
						error('`hgetall()`: invalid value type for map: ${T.type_name()}')
					}
				}
				return result
			}
			else {
				return error('`hgetall()`: expected array response, got: ${resp.type_name()}')
			}
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
		db.conn.write(db.cmd_buf)!

		// read resp
		resp := db.read_response()! as i64
		return resp != 0
	}
	return false
}

// read_response_bulk_string handles Redis bulk string responses (format: $<length>\r\n<data>\r\n)
fn (mut db DB) read_response_bulk_string() !RedisValue {
	mut data_length := i64(-1)
	mut chunk := []u8{len: 1}

	db.resp_buf.clear()
	for {
		bytes_read := db.conn.read(mut chunk) or {
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

	if data_length == 0 {
		mut terminator := []u8{len: 2}
		db.conn.read(mut terminator)!
		if terminator[0] != `\r` || terminator[1] != `\n` {
			return error('invalid terminator for empty string')
		}
		return []u8{}
	}
	if data_length == -1 {
		return RedisNull{}
	}

	mut data_buf := []u8{len: int(data_length) + 2} // +2 for ending \r\n
	mut total_read := 0

	for total_read < data_buf.len {
		remaining := data_buf.len - total_read
		chunk_size := if remaining > 1 { 1 } else { remaining }
		mut chunk_ptr := unsafe { &data_buf[total_read] }

		bytes_read := db.conn.read_ptr(chunk_ptr, chunk_size)!
		total_read += bytes_read

		if bytes_read == 0 && total_read < data_buf.len {
			return error('`read_response_bulk_string()`: incomplete data: read ${total_read} / ${data_buf.len} bytes')
		}
	}

	// must ending with CRLF
	if data_buf[data_length] != `\r` || data_buf[data_length + 1] != `\n` {
		return error('`read_response_bulk_string()`: invalid data terminator')
	}

	return data_buf[0..data_length].clone()
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

		bytes_read := db.conn.read_ptr(chunk_ptr, chunk_size)!
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

		bytes_read := db.conn.read_ptr(chunk_ptr, chunk_size)!
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
		bytes_read := db.conn.read(mut chunk) or {
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

// read_response handles all types of Redis responses
fn (mut db DB) read_response() !RedisValue {
	db.resp_buf.clear()
	unsafe { db.resp_buf.grow_len(1) }
	read_len := db.conn.read(mut db.resp_buf)!
	if read_len != 1 {
		return error('`read_response()`: empty response from server')
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
		else {
			return error('`read_response()`: unknown response prefix: ${db.resp_buf.bytestr()}')
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
		unsafe { db.conn.write(sb.reuse_as_plain_u8_array())! }
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

	db.conn.write(db.pipeline_buffer)!

	mut results := []RedisValue{cap: db.pipeline_cmd_count}
	for _ in 0 .. db.pipeline_cmd_count {
		results << db.read_response()!
	}

	// reset to non-pipeline mode
	db.pipeline_mode = false
	db.pipeline_cmd_count = 0
	return results
}
