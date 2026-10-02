module redis

// GetExMode selects the expiration operation performed by getex.
pub enum GetExMode {
	none
	ex
	px
	exat
	pxat
	persist
}

// GetExOptions configures getex expiration in seconds, milliseconds, or Unix timestamps.
@[params]
pub struct GetExOptions {
pub:
	mode  GetExMode
	value i64
}

// ScanOptions configures optional pattern matching and the suggested scan batch size.
@[params]
pub struct ScanOptions {
pub:
	match ?string
	count int
}

fn value_string[T](value T, command string) !string {
	$if T is string {
		return value
	} $else $if T is $int {
		return value.str()
	} $else $if T is []u8 {
		return value.bytestr()
	} $else {
		return error('`${command}()`: unsupported value type. Allowed: number, string, []u8')
	}
}

fn validate_bulk_type[T](command string) ! {
	$if T !is string && T !is $int && T !is []u8 {
		return error('`${command}()`: unsupported return type. Allowed: number, string, []u8')
	}
}

// Keep unsigned parsing outside generic instantiation so its intermediate types stay u64.
fn unsigned_value(data string) u64 {
	return data.u64()
}

fn bulk_value[T](resp RedisValue, command string) !T {
	validate_bulk_type[T](command)!
	$if T is []u8 {
		if resp is []u8 {
			return resp
		}
	}
	data := match resp {
		[]u8 { resp.bytestr() }
		string { resp }
		RedisNull { return error('`${command}()`: value not found') }
		else { return error('`${command}()`: unexpected response type') }
	}
	$if T is string {
		return data
	} $else $if T is u64 || T is usize {
		return T(unsigned_value(data))
	} $else $if T is $int {
		return T(data.i64())
	} $else $if T is []u8 {
		return data.bytes()
	} $else {
		return error('`${command}()`: unsupported return type')
	}
}

fn array_value(resp RedisValue, command string) ![]RedisValue {
	match resp {
		[]RedisValue { return resp }
		else { return error('`${command}()`: unexpected response type') }
	}
}

fn string_values(resp RedisValue, command string) ![]string {
	values := array_value(resp, command)!
	mut result := []string{cap: values.len}
	for value in values {
		result << bulk_value[string](value, command)!
	}
	return result
}

fn nullable_values[T](resp RedisValue, command string) ![]?T {
	values := array_value(resp, command)!
	mut result := []?T{cap: values.len}
	for value in values {
		if value is RedisNull {
			result << none
		} else {
			result << ?T(bulk_value[T](value, command)!)
		}
	}
	return result
}

fn (mut db DB) execute_i64(args []string) !i64 {
	resp := db.cmd(...args)!
	if db.pipeline_mode {
		return 0
	}
	match resp {
		i64 { return resp }
		else { return error('`${args[0].to_lower()}()`: unexpected response type') }
	}
}

fn (mut db DB) execute_string(args []string) !string {
	resp := db.cmd(...args)!
	if db.pipeline_mode {
		return ''
	}
	return bulk_value[string](resp, args[0].to_lower())
}

fn (mut db DB) execute_strings(args []string) ![]string {
	resp := db.cmd(...args)!
	if db.pipeline_mode {
		return []string{}
	}
	return string_values(resp, args[0].to_lower())
}

fn (mut db DB) execute_bulk[T](args []string) !T {
	validate_bulk_type[T](args[0].to_lower())!
	resp := db.cmd(...args)!
	if db.pipeline_mode {
		return T{}
	}
	return bulk_value[T](resp, args[0].to_lower())
}

fn (mut db DB) execute_nullable[T](args []string) ![]?T {
	validate_bulk_type[T](args[0].to_lower())!
	resp := db.cmd(...args)!
	if db.pipeline_mode {
		return []?T{}
	}
	return nullable_values[T](resp, args[0].to_lower())
}

fn (mut db DB) execute_f64(args []string) !f64 {
	resp := db.cmd(...args)!
	if db.pipeline_mode {
		return 0.0
	}
	match resp {
		f64 { return resp }
		else { return bulk_value[string](resp, args[0].to_lower())!.f64() }
	}
}

fn multi_set_args[T](command string, values map[string]T) ![]string {
	$if T !is string && T !is $int && T !is []u8 {
		return error('`${command.to_lower()}()`: unsupported value type. Allowed: number, string, []u8')
	}
	mut args := [command]
	for key, value in values {
		args << key
		args << value_string(value, command.to_lower())!
	}
	return args
}

fn scan_args(args []string, options ScanOptions) ![]string {
	if options.count < 0 {
		return error('`${args[0].to_lower()}()`: count must not be negative')
	}
	mut result := args.clone()
	if pattern := options.match {
		result << 'MATCH'
		result << pattern
	}
	if options.count > 0 {
		result << 'COUNT'
		result << options.count.str()
	}
	return result
}

fn (mut db DB) execute_scan(args []string) !(string, []string) {
	resp := db.cmd(...args)!
	if db.pipeline_mode {
		return '', []string{}
	}
	command := args[0].to_lower()
	values := array_value(resp, command)!
	if values.len != 2 {
		return error('`${command}()`: invalid scan response')
	}
	return bulk_value[string](values[0], command)!, string_values(values[1], command)!
}

// mget retrieves values in key order, preserving missing keys as none.
// Supported return types are string, integer types, and []u8.
pub fn (mut db DB) mget[T](keys ...string) ![]?T {
	mut args := ['MGET']
	args << keys
	return db.execute_nullable[T](args)
}

// mset atomically stores multiple keys. Supported value types are string, integer types, and []u8.
pub fn (mut db DB) mset[T](values map[string]T) !string {
	return db.execute_string(multi_set_args('MSET', values)!)
}

// msetnx atomically stores multiple keys only when none of them exists.
pub fn (mut db DB) msetnx[T](values map[string]T) !bool {
	return db.execute_i64(multi_set_args('MSETNX', values)!)! != 0
}

// setnx stores a key only when it does not exist.
pub fn (mut db DB) setnx[T](key string, value T) !bool {
	return db.execute_i64(['SETNX', key, value_string(value, 'setnx')!])! != 0
}

// setex stores a value with an expiration time in seconds.
pub fn (mut db DB) setex[T](key string, seconds i64, value T) !string {
	return db.execute_string(['SETEX', key, seconds.str(), value_string(value, 'setex')!])
}

// psetex stores a value with an expiration time in milliseconds.
pub fn (mut db DB) psetex[T](key string, milliseconds i64, value T) !string {
	return db.execute_string(['PSETEX', key, milliseconds.str(), value_string(value, 'psetex')!])
}

// getdel retrieves and deletes a string key, returning an error if the key is missing.
pub fn (mut db DB) getdel[T](key string) !T {
	return db.execute_bulk[T](['GETDEL', key])
}

// getex retrieves a string key and optionally changes or removes its expiration.
// Missing keys return an error. Timed modes require a positive value.
pub fn (mut db DB) getex[T](key string, options GetExOptions) !T {
	mut args := ['GETEX', key]
	match options.mode {
		.none, .persist {
			if options.value != 0 {
				return error('`getex()`: expiration value requires a timed mode')
			}
			if options.mode == .persist {
				args << 'PERSIST'
			}
		}
		.ex, .px, .exat, .pxat {
			if options.value <= 0 {
				return error('`getex()`: expiration value must be positive')
			}
			args << options.mode.str().to_upper()
			args << options.value.str()
		}
	}
	return db.execute_bulk[T](args)
}

// incrby increments an integer key by the given amount.
pub fn (mut db DB) incrby(key string, increment i64) !i64 {
	return db.execute_i64(['INCRBY', key, increment.str()])
}

// decrby decrements an integer key by the given amount.
pub fn (mut db DB) decrby(key string, decrement i64) !i64 {
	return db.execute_i64(['DECRBY', key, decrement.str()])
}

// incrbyfloat increments a numeric key by the given floating point amount.
pub fn (mut db DB) incrbyfloat(key string, increment f64) !f64 {
	return db.execute_f64(['INCRBYFLOAT', key, increment.str()])
}

// exists counts the specified keys that exist, counting repeated keys separately.
pub fn (mut db DB) exists(keys ...string) !i64 {
	mut args := ['EXISTS']
	args << keys
	return db.execute_i64(args)
}

// ttl returns the remaining lifetime in seconds, or -1 without expiration and -2 for a missing key.
pub fn (mut db DB) ttl(key string) !i64 {
	return db.execute_i64(['TTL', key])
}

// pttl returns the remaining lifetime in milliseconds, or -1 without expiration and -2 if missing.
pub fn (mut db DB) pttl(key string) !i64 {
	return db.execute_i64(['PTTL', key])
}

// pexpire sets a key's expiration in milliseconds and reports whether it was set.
pub fn (mut db DB) pexpire(key string, milliseconds i64) !bool {
	return db.execute_i64(['PEXPIRE', key, milliseconds.str()])! != 0
}

// expireat sets a key's expiration to a Unix timestamp in seconds.
pub fn (mut db DB) expireat(key string, timestamp i64) !bool {
	return db.execute_i64(['EXPIREAT', key, timestamp.str()])! != 0
}

// pexpireat sets a key's expiration to a Unix timestamp in milliseconds.
pub fn (mut db DB) pexpireat(key string, timestamp i64) !bool {
	return db.execute_i64(['PEXPIREAT', key, timestamp.str()])! != 0
}

// persist removes a key's expiration and reports whether an expiration was removed.
pub fn (mut db DB) persist(key string) !bool {
	return db.execute_i64(['PERSIST', key])! != 0
}

// unlink removes keys, reclaiming their memory asynchronously.
pub fn (mut db DB) unlink(keys ...string) !i64 {
	mut args := ['UNLINK']
	args << keys
	return db.execute_i64(args)
}

// keys returns all keys matching a pattern. Use scan for incremental iteration over large databases.
pub fn (mut db DB) keys(pattern string) ![]string {
	return db.execute_strings(['KEYS', pattern])
}

// scan returns the next cursor and matching keys. Continue until the cursor is '0'.
pub fn (mut db DB) scan(cursor string, options ScanOptions) !(string, []string) {
	return db.execute_scan(scan_args(['SCAN', cursor], options)!)
}

// key_type returns the Redis type of a key, or 'none' if the key does not exist.
pub fn (mut db DB) key_type(key string) !string {
	return db.execute_string(['TYPE', key])
}

// rename renames a key, replacing any existing destination key.
pub fn (mut db DB) rename(key string, new_key string) !string {
	return db.execute_string(['RENAME', key, new_key])
}

// hdel removes hash fields and returns the number removed.
pub fn (mut db DB) hdel(key string, fields ...string) !i64 {
	mut args := ['HDEL', key]
	args << fields
	return db.execute_i64(args)
}

// hexists reports whether a hash field exists.
pub fn (mut db DB) hexists(key string, field string) !bool {
	return db.execute_i64(['HEXISTS', key, field])! != 0
}

// hkeys returns all field names in a hash.
pub fn (mut db DB) hkeys(key string) ![]string {
	return db.execute_strings(['HKEYS', key])
}

// hvals returns all values in a hash as strings.
pub fn (mut db DB) hvals(key string) ![]string {
	return db.execute_strings(['HVALS', key])
}

// hlen returns the number of fields in a hash.
pub fn (mut db DB) hlen(key string) !i64 {
	return db.execute_i64(['HLEN', key])
}

// hstrlen returns the byte length of a hash field's value, or zero for a missing field.
pub fn (mut db DB) hstrlen(key string, field string) !i64 {
	return db.execute_i64(['HSTRLEN', key, field])
}

// hsetnx stores a hash field only when it does not exist.
pub fn (mut db DB) hsetnx[T](key string, field string, value T) !bool {
	return db.execute_i64(['HSETNX', key, field, value_string(value, 'hsetnx')!])! != 0
}

// hincrby increments an integer hash field by the given amount.
pub fn (mut db DB) hincrby(key string, field string, increment i64) !i64 {
	return db.execute_i64(['HINCRBY', key, field, increment.str()])
}

// hincrbyfloat increments a numeric hash field by the given floating point amount.
pub fn (mut db DB) hincrbyfloat(key string, field string, increment f64) !f64 {
	return db.execute_f64(['HINCRBYFLOAT', key, field, increment.str()])
}

// hmget retrieves hash field values in field order, preserving missing fields as none.
// Supported return types are string, integer types, and []u8.
pub fn (mut db DB) hmget[T](key string, fields ...string) ![]?T {
	mut args := ['HMGET', key]
	args << fields
	return db.execute_nullable[T](args)
}

// hscan returns the next cursor and alternating field/value strings for a hash.
// Continue until the cursor is '0'.
pub fn (mut db DB) hscan(key string, cursor string, options ScanOptions) !(string, []string) {
	return db.execute_scan(scan_args(['HSCAN', key, cursor], options)!)
}

// append appends a value to a string key and returns the resulting byte length.
pub fn (mut db DB) append[T](key string, value T) !i64 {
	return db.execute_i64(['APPEND', key, value_string(value, 'append')!])
}

// strlen returns the byte length of a string key, or zero if the key is missing.
pub fn (mut db DB) strlen(key string) !i64 {
	return db.execute_i64(['STRLEN', key])
}

// getrange retrieves a string key's byte range using inclusive start and end offsets.
pub fn (mut db DB) getrange[T](key string, start i64, end i64) !T {
	return db.execute_bulk[T](['GETRANGE', key, start.str(), end.str()])
}

// setrange overwrites a string key starting at the byte offset and returns its resulting length.
pub fn (mut db DB) setrange[T](key string, offset i64, value T) !i64 {
	return db.execute_i64(['SETRANGE', key, offset.str(), value_string(value, 'setrange')!])
}
